package gutta.apievolution.inprocess;

import java.lang.reflect.Method;
import java.util.Optional;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.ConcurrentMap;

import gutta.apievolution.core.apimodel.Field;
import gutta.apievolution.core.apimodel.Type;
import gutta.apievolution.core.apimodel.UserDefinedType;
import gutta.apievolution.core.apimodel.consumer.ConsumerApiDefinition;
import gutta.apievolution.core.resolution.DefinitionResolution;

public abstract class AbstractTypeMappingStrategy implements TypeMappingStrategy {
    
    private final ConsumerApiDefinition consumerApiDefinition;
    
    private final DefinitionResolution definitionResolution;
    
    private final TypeClassMap typeToClassMap;
    
    private final ConcurrentMap<Class<?>, ValueMapper> valueMapperCache = new ConcurrentHashMap<>();
    
    protected AbstractTypeMappingStrategy(ConsumerApiDefinition consumerApiDefinition, DefinitionResolution definitionResolution, TypeClassMap typeToClassMap) {
        this.consumerApiDefinition = consumerApiDefinition;
        this.definitionResolution = definitionResolution;
        this.typeToClassMap = typeToClassMap;        
    }
    
    protected ConsumerApiDefinition getConsumerApiDefinition() {
        return this.consumerApiDefinition;
    }
    
    protected DefinitionResolution getDefinitionResolution() {
        return this.definitionResolution;
    }
    
    protected TypeClassMap getTypeToClassMap() {
        return this.typeToClassMap;
    }
    
    @Override
    public ValueMapper mapperFor(Class<?> type) {
        return this.valueMapperCache.computeIfAbsent(type, this::createMapperFor);
    } 
    
    protected abstract ValueMapper createMapperFor(Class<?> javaClass);
    
    @SuppressWarnings("unchecked")
    protected <T extends Type> T findTypeMatching(Class<?> javaClass) {
        Type type = this.getTypeToClassMap().classToType(javaClass);
        if (type != null) {
            // If the current class is a representation of an API type, we have found a match            
            return (T) type;
        }
        
        // Otherwise, try to find a match in the superclass (if any)...
        Class<?> superClass = javaClass.getSuperclass();
        if (superClass != null) {
            T candidate = this.findTypeMatching(superClass);
            if (candidate != null) {
                return candidate;
            }
        }
        
        // ...or the implemented interfaces
        for (Class<?> implementedInterface : javaClass.getInterfaces()) {
            T candidate = this.findTypeMatching(implementedInterface);
            if (candidate != null) {
                return candidate;
            }
        }
        
        // If no match is found, return null
        return null;
    }
    
    protected static boolean isProviderType(Type type) {
        return (type instanceof UserDefinedType) && ((UserDefinedType<?>) type).isProviderType();
    }
    
    protected static Optional<Method> findAccessorForField(Field<?, ?> field, Class<?> type) {
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
