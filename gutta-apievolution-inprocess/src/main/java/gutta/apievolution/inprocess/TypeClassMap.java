package gutta.apievolution.inprocess;

import java.util.HashMap;
import java.util.List;
import java.util.Map;

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

public class TypeClassMap {
    
    private final UDTToClassMap udtToClassMap;
    
    private final Map<Class<?>, Type> consumerTypeLookup;
        
    public TypeClassMap(UDTToClassMap udtToClassMap, ConsumerApiDefinition apiDefinition) {
        this.udtToClassMap = udtToClassMap;
        this.consumerTypeLookup = createConsumerTypeLookup(udtToClassMap, apiDefinition);        
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
    
    public Type classToConsumerType(Class<?> type) {
        return this.consumerTypeLookup.get(type);                
    }
    
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
        
    public <T> Class<T> consumerTypeToClass(Type type) {
        return this.typeToClass(type);
    }

    public <T> Class<T> providerTypeToClass(Type type) {
        return this.typeToClass(type);
    }
    
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
