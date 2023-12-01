package gutta.apievolution.inprocess;

import gutta.apievolution.core.apimodel.AtomicType;
import gutta.apievolution.core.apimodel.BasicType;
import gutta.apievolution.core.apimodel.BoundedListType;
import gutta.apievolution.core.apimodel.BoundedStringType;
import gutta.apievolution.core.apimodel.Type;
import gutta.apievolution.core.apimodel.TypeVisitor;
import gutta.apievolution.core.apimodel.UnboundedListType;
import gutta.apievolution.core.apimodel.UnboundedStringType;
import gutta.apievolution.core.apimodel.UserDefinedType;
import gutta.apievolution.core.apimodel.consumer.ConsumerApiDefinition;
import gutta.apievolution.core.apimodel.consumer.ConsumerUserDefinedType;
import gutta.apievolution.core.apimodel.provider.ProviderUserDefinedType;
import gutta.apievolution.core.resolution.DefinitionResolution;

import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * A {@link TypeClassMap} provides a mapping from API types to implementation classes and vice versa.
 */
public class TypeClassMap {

    private final UDTToClassMap udtToClassMap;

    private final Map<Class<?>, Type> consumerTypeLookup;

    private final Map<Class<?>, Type> providerTypeLookup;

    /**
     * Creates a new map using the given data.
     * 
     * @param udtToClassMap        The underlying mapping of user-defined types to implementation classes.
     * @param apiDefinition        The consumer API definition that is used
     * @param definitionResolution The resolution of the consumer API against the provider API
     */
    TypeClassMap(UDTToClassMap udtToClassMap, ConsumerApiDefinition apiDefinition, DefinitionResolution definitionResolution) {
        this.udtToClassMap = udtToClassMap;
        this.consumerTypeLookup = createConsumerTypeLookup(udtToClassMap, apiDefinition);
        this.providerTypeLookup = createProviderTypeLookup(udtToClassMap, apiDefinition, definitionResolution);
    }

    private static Map<Class<?>, Type> createConsumerTypeLookup(UDTToClassMap typeMap, ConsumerApiDefinition apiDefinition) {
        List<UserDefinedType<ConsumerApiDefinition>> userDefinedTypes = apiDefinition.getUserDefinedTypes();
        Map<Class<?>, Type> lookup = new HashMap<>(userDefinedTypes.size());

        for (UserDefinedType<ConsumerApiDefinition> type : userDefinedTypes) {
            Class<?> representingClass = typeMap.consumerTypeToClass(type);

            if (representingClass != null) {
                lookup.put(representingClass, type);
            }
        }

        return lookup;
    }

    private static Map<Class<?>, Type> createProviderTypeLookup(UDTToClassMap typeMap, ConsumerApiDefinition apiDefinition,
            DefinitionResolution definitionResolution) {

        List<UserDefinedType<ConsumerApiDefinition>> userDefinedTypes = apiDefinition.getUserDefinedTypes();
        Map<Class<?>, Type> lookup = new HashMap<>(userDefinedTypes.size());

        for (UserDefinedType<ConsumerApiDefinition> consumerType : userDefinedTypes) {
            ProviderUserDefinedType providerType = (ProviderUserDefinedType) definitionResolution.mapConsumerType(consumerType);
            Class<?> representingClass = typeMap.providerTypeToClass(providerType);

            if (representingClass != null) {
                lookup.put(representingClass, providerType);
            }
        }

        return lookup;
    }

    /**
     * Returns the API type represented by the given class.
     * 
     * @param javaClass The class whose API counterpart is sought
     * @return The API type represented by this class or {@code null} if the class does not represent an API type
     */
    public Type classToType(Class<?> javaClass) {
        Type candidate = this.classToConsumerType(javaClass);
        if (candidate != null) {
            return candidate;
        }

        return this.classToProviderType(javaClass);
    }

    private Type classToConsumerType(Class<?> type) {
        return this.consumerTypeLookup.get(type);
    }

    private Type classToProviderType(Class<?> type) {
        return this.providerTypeLookup.get(type);
    }

    /**
     * Returns the class representing the given API type.
     * 
     * @param <T>  The type of the class
     * @param type The API type whose representation is sought
     * @return The class representing the type
     */
    @SuppressWarnings("unchecked")
    public <T> Class<T> typeToClass(Type type) {
        if (type instanceof ConsumerUserDefinedType) {
            return this.udtToClassMap.consumerTypeToClass((ConsumerUserDefinedType) type);
        } else if (type instanceof ProviderUserDefinedType) {
            return this.udtToClassMap.providerTypeToClass((ProviderUserDefinedType) type);
        } else if (type instanceof BasicType) {
            return (Class<T>) this.basicTypeToClass((BasicType) type);
        } else {
            throw new IllegalArgumentException("Unsupported type '" + type + "'.");
        }
    }

    private Class<?> basicTypeToClass(BasicType type) {
        return new BasicTypeToClassMap().mapBasicTypeToClass(type);
    }

    /**
     * Encoding of the mapping of basic types to classes.
     */
    private static class BasicTypeToClassMap implements TypeVisitor<Class<?>> {

        private static final Class<?> INT32_REPRESENTATION = Integer.class;

        private static final Class<?> INT64_REPRESENTATION = Long.class;

        private static final Class<?> STRING_REPRESENTATION = String.class;

        private static final Class<?> LIST_REPRESENTATION = List.class;

        public Class<?> mapBasicTypeToClass(BasicType type) {
            return type.accept(this);
        }

        @Override
        public Class<?> handleAtomicType(AtomicType atomicType) {
            switch (atomicType) {
            case INT_32:
                return INT32_REPRESENTATION;

            case INT_64:
                return INT64_REPRESENTATION;

            default:
                throw new IllegalArgumentException("Unsupported atomic type '" + atomicType + "'.");
            }
        }

        @Override
        public Class<?> handleBoundedStringType(BoundedStringType boundedStringType) {
            return STRING_REPRESENTATION;
        }

        @Override
        public Class<?> handleUnboundedStringType(UnboundedStringType unboundedStringType) {
            return STRING_REPRESENTATION;
        }

        @Override
        public Class<?> handleBoundedListType(BoundedListType boundedListType) {
            return LIST_REPRESENTATION;
        }

        @Override
        public Class<?> handleUnboundedListType(UnboundedListType unboundedListType) {
            return LIST_REPRESENTATION;
        }

    }

}
