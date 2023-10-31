package gutta.apievolution.inprocess.dynproxy;

import static java.lang.reflect.Proxy.newProxyInstance;

import java.lang.reflect.InvocationHandler;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.NoSuchElementException;
import java.util.Optional;
import java.util.Set;
import java.util.stream.Collectors;

import gutta.apievolution.core.apimodel.Field;
import gutta.apievolution.core.apimodel.ListType;
import gutta.apievolution.core.apimodel.Type;
import gutta.apievolution.core.apimodel.UserDefinedType;
import gutta.apievolution.core.apimodel.consumer.ConsumerApiDefinition;
import gutta.apievolution.core.apimodel.consumer.ConsumerField;
import gutta.apievolution.core.apimodel.consumer.ConsumerOperation;
import gutta.apievolution.core.apimodel.consumer.ConsumerRecordType;
import gutta.apievolution.core.apimodel.provider.ProviderField;
import gutta.apievolution.core.apimodel.provider.ProviderOperation;
import gutta.apievolution.core.apimodel.provider.ProviderRecordType;
import gutta.apievolution.core.resolution.DefinitionResolution;
import gutta.apievolution.inprocess.TypeClassMap;
import gutta.apievolution.inprocess.UDTToClassMap;

class ApiInvocationHandler implements InvocationHandler {

    private final Object providerApi;

    private final ConsumerApiDefinition consumerApiDefinition;

    private final DefinitionResolution definitionResolution;

    private final TypeClassMap typeToClassMap;

    ApiInvocationHandler(Object providerApi, ConsumerApiDefinition consumerApiDefinition, DefinitionResolution definitionResolution,
            UDTToClassMap typeToClassMap) {
        this.providerApi = providerApi;
        this.consumerApiDefinition = consumerApiDefinition;
        this.definitionResolution = definitionResolution;
        this.typeToClassMap = new TypeClassMap(typeToClassMap, consumerApiDefinition);
    }

    @Override
    public Object invoke(Object proxy, Method method, Object[] arguments) {
        // Map the consumer operation to the provider operation (in the model)
        String methodName = method.getName();
        ConsumerOperation consumerOperation = this.consumerApiDefinition.resolveOperation(methodName)
                .orElseThrow(NoSuchElementException::new);
        ProviderOperation providerOperation = this.definitionResolution.mapConsumerOperation(consumerOperation);

        // Find the appropriate method for the provider operation on the API object
        ProviderRecordType providerParameterType = providerOperation.getParameterType();
        Class<?> providerParameterClass = this.typeToClassMap.providerTypeToClass(providerParameterType);
        if (!providerParameterClass.isInterface()) {
            throw new InvalidApiException("Parameter type '" + providerParameterClass + "' is not an interface.");
        }

        ProviderRecordType providerResultType = providerOperation.getReturnType();
        Class<?> providerResultClass = this.typeToClassMap.providerTypeToClass(providerResultType);

        Class<?> providerApiClass = this.providerApi.getClass();
        String providerMethodName = providerOperation.getInternalName();
        Method providerMethod;

        try {
            providerMethod = providerApiClass.getMethod(providerMethodName, providerParameterClass);
        } catch (NoSuchMethodException e) {
            throw new InvalidApiException("Missing method '" + providerMethodName + "' on API '" + providerApiClass.getName() + "'.", e);
        }

        if (!providerResultClass.equals(providerMethod.getReturnType())) {
            throw new InvalidApiException("Method '" + providerMethod + "' has an unexpected return type.");
        }

        // The type of the parameter object may be a specialization of the formal type,
        // so we need to find the closest
        // match for the runtime type in the definition. Note that the immediate type of
        // the argument does not have to
        // be a representation of a definition type: If the representation is an
        // interface (which it has to be if the
        // type is also used as a return type), we will receive an object that
        // implements this interface.
        ConsumerRecordType formalConsumerParameterType = consumerOperation.getParameterType();

        if (arguments.length != 1) {
            throw new InvalidInvocationException("Method '" + method.getName() +
                    "' was invoked with invalid number of arguments (expected 1, but got " + arguments.length + ").");
        }
        Object parameterObject = arguments[0];

        ConsumerRecordType actualConsumerParameterType = this.findConsumerTypeMatching(parameterObject.getClass(),
                formalConsumerParameterType);
        if (actualConsumerParameterType == null) {
            throw new InvalidInvocationException("No matching type for class '" + parameterObject.getClass() + "'.");
        }

        ProviderRecordType actualProviderParameterType = (ProviderRecordType) this.definitionResolution
                .mapConsumerType(actualConsumerParameterType);
        Map<Method, FieldMapper> parameterFieldMappers = this.createMappersFor(actualConsumerParameterType, actualProviderParameterType);
        RecordInvocationHandler parameterInvocationHandler = new RecordInvocationHandler(parameterObject, parameterFieldMappers);

        Class<?> actualProviderParameterClass = this.typeToClassMap.providerTypeToClass(actualProviderParameterType);
        Class<?>[] parameterTypes = new Class<?>[] { actualProviderParameterClass };
        Object parameterProxy = newProxyInstance(this.getClass().getClassLoader(), parameterTypes, parameterInvocationHandler);

        Object providerResult;
        try {
            providerResult = providerMethod.invoke(this.providerApi, parameterProxy);
        } catch (InvocationTargetException e) {
            // TODO Map declared exceptions if applicable
            throw new RuntimeException(e);
        } catch (IllegalAccessException | IllegalArgumentException e) {
            // TODO Use an appropriate exception
            throw new RuntimeException(e);
        }

        if (providerResult == null) {
            // No mapping necessary when the result is null
            return null;
        } else {
            // Map the result to the consumer's expectation
            ConsumerRecordType consumerResultType = consumerOperation.getReturnType();
            Class<?> consumerResultClass = this.typeToClassMap.consumerTypeToClass(consumerResultType);

            Map<Method, FieldMapper> resultFieldMappers = this.createMappersFor(null, null);
            RecordInvocationHandler resultInvocationHandler = new RecordInvocationHandler(providerResult, resultFieldMappers);
            Class<?>[] resultTypes = new Class<?>[] { consumerResultClass };
            Object resultProxy = newProxyInstance(this.getClass().getClassLoader(), resultTypes, resultInvocationHandler);

            return resultProxy;
        }
    }

    @SuppressWarnings("unchecked")
    private <T extends Type> T findConsumerTypeMatching(Class<?> type, ConsumerRecordType upperBound) {
        Set<ConsumerRecordType> subTypes = upperBound.getSubTypes();
        Set<ConsumerRecordType> subTypesIncludingBound = new HashSet<>(subTypes.size() + 1);
        subTypesIncludingBound.add(upperBound);
        subTypesIncludingBound.addAll(subTypes);

        Set<Class<?>> acceptableClasses = subTypesIncludingBound.stream().map(this.typeToClassMap::consumerTypeToClass)
                .collect(Collectors.toSet());

        // Find a supertype in the set of acceptable classes. Currently, we just work
        // with the first acceptable
        // class we encounter. In certain situations, it would be more predictable if we
        // retrieved all candidates
        // and selected the most specific one
        Class<?> matchingClass = findSupertypeIn(type, acceptableClasses);
        if (matchingClass == null) {
            return null;
        } else {
            return (T) this.typeToClassMap.classToConsumerType(matchingClass);
        }
    }

    private static Class<?> findSupertypeIn(Class<?> seed, Set<Class<?>> desiredClasses) {
        if (desiredClasses.contains(seed)) {
            // If the current type is one of the desired ones, we have found a match
            return seed;
        }

        // First, try to find a match in the superclass (if any)
        Class<?> superClass = seed.getSuperclass();
        if (superClass != null) {
            Class<?> candidate = findSupertypeIn(superClass, desiredClasses);
            if (candidate != null) {
                return candidate;
            }
        }

        // If no match is found in the superclass, try the implemented interfaces
        for (Class<?> implementedInterface : seed.getInterfaces()) {
            Class<?> candidate = findSupertypeIn(implementedInterface, desiredClasses);
            if (candidate != null) {
                return candidate;
            }
        }

        // If no match is found, return null
        return null;
    }

    private Map<Method, FieldMapper> createMappersFor(ConsumerRecordType sourceType, ProviderRecordType targetType) {
        Class<?> consumerClass = this.typeToClassMap.consumerTypeToClass(sourceType);
        Class<?> providerClass = this.typeToClassMap.providerTypeToClass(targetType);

        Map<Method, FieldMapper> fieldMappers = new HashMap<>();
        targetType.getFields().forEach(field -> this.registerMapperForProviderField(field, consumerClass, providerClass, fieldMappers));

        return fieldMappers;
    }

    private void registerMapperForProviderField(ProviderField field, Class<?> consumerClass, Class<?> providerClass, Map<Method, FieldMapper> fieldMappers) {
        // Find a potential accessor method on the provider class
        Method accessor = this.findAccessorForField(field, providerClass).orElseThrow(
                () -> new InvalidApiException("No accessor for field '" + field + "' on class '" + providerClass.getName() + "'."));

        // Ensure that the accessor method has the expected type
        Type expectedType = field.getType();
        Class<?> expectedClass = this.typeToClassMap.typeToClass(expectedType);
        if (!expectedClass.equals(accessor.getReturnType())) {
            throw new InvalidApiException("Accessor '" + accessor + "' has an unexpected return type (expected '" + expectedClass + "'.");
        }        
        
        FieldMapper fieldMapper = this.createMapperForProviderField(field, accessor, consumerClass, providerClass);
        fieldMappers.put(accessor, fieldMapper);
    }
    
    private FieldMapper createMapperForProviderField(ProviderField providerField, Method accessor, Class<?> consumerClass, Class<?> providerClass) {
        ConsumerField consumerField = this.definitionResolution.mapProviderField(providerField);

        if (consumerField == null) {
            // The provider field may be unmatched
            return new UnmatchedFieldMapper(accessor);
        }

        Type targetType = providerField.getType();
        if (targetType instanceof UserDefinedType) {
            // TODO
            return null;
        } else if (targetType instanceof ListType) {
            return new ListTypeFieldMapper(accessor);
        } else {
            return new BasicTypeFieldMapper(accessor);
        }
    }

    private Optional<Method> findAccessorForField(Field<?, ?> field, Class<?> type) {
        String fieldName = field.getInternalName();

        // Try a "get" method first...
        Optional<Method> optionalGetter = findMethod(type, getterNameFor(fieldName));
        if (optionalGetter.isPresent()) {
            return optionalGetter;
        }

        // ..., then try a method with the exact name (such as in records)
        Optional<Method> optionalAccessor = findMethod(type, fieldName);
        if (optionalAccessor.isPresent()) {
            return optionalAccessor;
        }

        return Optional.empty();
    }

    private static Optional<Method> findMethod(Class<?> type, String name, Class<?>... argumentTypes) {
        try {
            return Optional.of(type.getMethod(name, argumentTypes));
        } catch (NoSuchMethodException e) {
            return Optional.empty();
        }
    }

    private static String getterNameFor(String fieldName) {
        char firstChar = fieldName.charAt(0);
        String remainder = (fieldName.length() == 1) ? "" : fieldName.substring(1);

        return "get" + Character.toUpperCase(firstChar) + remainder;
    }

}
