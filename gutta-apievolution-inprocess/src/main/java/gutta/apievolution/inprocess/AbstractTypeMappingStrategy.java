package gutta.apievolution.inprocess;

import gutta.apievolution.core.apimodel.RecordType;
import gutta.apievolution.core.apimodel.Type;
import gutta.apievolution.core.apimodel.consumer.ConsumerApiDefinition;
import gutta.apievolution.core.resolution.DefinitionResolution;

import java.util.Collection;
import java.util.HashSet;
import java.util.Set;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.ConcurrentMap;
import java.util.function.Consumer;

/**
 * Abstract implementation of a {@link TypeMappingStrategy} that provides common functionality.
 */
public abstract class AbstractTypeMappingStrategy implements TypeMappingStrategy {

    private final ConsumerApiDefinition consumerApiDefinition;

    private final DefinitionResolution definitionResolution;

    private final TypeClassMap typeToClassMap;

    private final ConcurrentMap<Class<?>, ValueMapper> valueMapperCache = new ConcurrentHashMap<>();

    /**
     * Creates a type mapping strategy using the given data.
     * 
     * @param consumerApiDefinition The consumer API that is used
     * @param definitionResolution  The definition of the consumer API to the provider API
     * @param typeToClassMap        A mapping of the API types to their representing classes
     */
    protected AbstractTypeMappingStrategy(ConsumerApiDefinition consumerApiDefinition, DefinitionResolution definitionResolution, TypeClassMap typeToClassMap) {
        this.consumerApiDefinition = consumerApiDefinition;
        this.definitionResolution = definitionResolution;
        this.typeToClassMap = typeToClassMap;
    }

    /**
     * Returns the consumer API definition for this strategy.
     * 
     * @return see above
     */
    protected ConsumerApiDefinition getConsumerApiDefinition() {
        return this.consumerApiDefinition;
    }

    /**
     * Returns the resolution of the consumer API definition against the provider API.
     * 
     * @return see above
     */
    protected DefinitionResolution getDefinitionResolution() {
        return this.definitionResolution;
    }

    /**
     * Returns the mapping of API types to their representing classes.
     * 
     * @return see above
     */
    protected TypeClassMap getTypeToClassMap() {
        return this.typeToClassMap;
    }

    @Override
    public ValueMapper mapperFor(Class<?> type) {
        return this.valueMapperCache.computeIfAbsent(type, this::createMapperFor);
    }

    /**
     * Creates a value mapper for the given class.
     * 
     * @param javaClass The class to create a mapper for
     * @return The created value mapper
     */
    protected abstract ValueMapper createMapperFor(Class<?> javaClass);

    /**
     * Finds the API type that is represented by the given class.
     * 
     * @param <T>       The type of API type (e.g., a record type)
     * @param javaClass The class representing the type
     * @return The type represented by the class or {@code null} if the class does not represent a type
     */
    @SuppressWarnings("unchecked")
    protected <T extends Type> T findTypeRepresentedBy(Class<?> javaClass) {
        Type type = this.getTypeToClassMap().classToType(javaClass);
        if (type != null) {
            // If the current class is a representation of an API type, we have found a match
            return (T) type;
        }

        // Otherwise, find the most specific type of all supertypes (superclasses or interfaces)
        Set<Type> candidates = this.findTypesRepresentedBySuperTypesOf(javaClass);
        return (T) mostSpecificTypeOf(candidates);
    }

    private Set<Type> findTypesRepresentedBySuperTypesOf(Class<?> javaClass) {
        Set<Type> types = new HashSet<>();

        this.collectTypesRepresentedBySuperTypes(javaClass, types::add);

        return types;
    }

    private void collectTypesRepresentedBySuperTypes(Class<?> javaClass, Consumer<Type> collector) {
        Type candidate = this.getTypeToClassMap().classToType(javaClass);
        if (candidate != null) {
            collector.accept(candidate);
        }
                
        Class<?> superClass = javaClass.getSuperclass();
        if (superClass != null) {
            this.collectTypesRepresentedBySuperTypes(superClass, collector);
        }

        // ...or the implemented interfaces
        for (Class<?> implementedInterface : javaClass.getInterfaces()) {
            this.collectTypesRepresentedBySuperTypes(implementedInterface, collector);
        }       
    }
    
    private static Type mostSpecificTypeOf(Collection<Type> types) {
        Type mostSpecificType = null;
        
        for (Type type : types) {
            if (mostSpecificType == null) {
                // The first type encountered is always the most specific one up to this point
                mostSpecificType = type;
                continue;
            } else if (type instanceof RecordType && mostSpecificType instanceof RecordType) {
                // If both the most specific type and the current type are records, they may be in a inheritance hierarchy
                // and the current type may be more specific
                
                RecordType<?, ?, ?> recordType = (RecordType<?, ?, ?>) type;
                RecordType<?, ?, ?> mostSpecificRecordType = (RecordType<?, ?, ?>) mostSpecificType;
                
                if (mostSpecificRecordType.isSupertypeOf(recordType)) {
                    // The current type is more specific than the most specific one up to this point
                    mostSpecificType = recordType;
                } else if (!recordType.isSupertypeOf(mostSpecificRecordType)) {
                    // The current type is a supertype of the most specific one, which is fine
                } else {
                    // The current type and the most specific type are not in the same inheritance hierarchy
                    return onIncompatibleTypes(type, mostSpecificType);
                }                
            } else {
                // If not both the current type and the most specific type are records, they cannot be in an inheritance hierarchy and
                // must therfore be the same
                if (!type.equals(mostSpecificType)) {
                    return onIncompatibleTypes(type, mostSpecificType);
                }
            }
        }
        
        return mostSpecificType;
    }
    
    private static Type onIncompatibleTypes(Type type1, Type type2) {
        throw new InvalidApiException("Inconsistent representation, types '" + type1 + "' and '" + type2 + "' are not in the same inheritance hierarchy.");
    }

}
