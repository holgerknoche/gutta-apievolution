package gutta.apievolution.inprocess;

import gutta.apievolution.core.apimodel.Type;
import gutta.apievolution.core.apimodel.consumer.ConsumerApiDefinition;
import gutta.apievolution.core.resolution.DefinitionResolution;

import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.ConcurrentMap;

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

        // Otherwise, try to find a match in the superclass (if any)...
        Class<?> superClass = javaClass.getSuperclass();
        if (superClass != null) {
            T candidate = this.findTypeRepresentedBy(superClass);
            if (candidate != null) {
                return candidate;
            }
        }

        // ...or the implemented interfaces
        for (Class<?> implementedInterface : javaClass.getInterfaces()) {
            T candidate = this.findTypeRepresentedBy(implementedInterface);
            if (candidate != null) {
                return candidate;
            }
        }

        // If no match is found, return null
        return null;
    }

}
