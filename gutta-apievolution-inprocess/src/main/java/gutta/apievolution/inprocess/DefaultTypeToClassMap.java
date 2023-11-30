package gutta.apievolution.inprocess;

import gutta.apievolution.core.apimodel.UserDefinedType;
import gutta.apievolution.core.apimodel.consumer.ConsumerApiDefinition;
import gutta.apievolution.core.apimodel.provider.ProviderApiDefinition;

/**
 * Default implementation of a {@link UDTToClassMap} that derives the appropriate classes by prepending a given package name.
 */
public class DefaultTypeToClassMap implements UDTToClassMap {

    private final String consumerPackageName;

    private final String providerPackageName;

    /**
     * Creates a new map from a given consumer API definition. The package names used are the name of the consumer API definition and the name of the referenced
     * provider API.
     * 
     * @param consumerApiDefinition The consumer API definition to derive the package names from
     */
    public DefaultTypeToClassMap(ConsumerApiDefinition consumerApiDefinition) {
        this.consumerPackageName = consumerApiDefinition.getName().toString();
        this.providerPackageName = consumerApiDefinition.getReferencedApiName();
    }

    /**
     * Creates a new map using the given package names.
     * 
     * @param consumerPackageName The name of the package containing the consumer types
     * @param providerPackageName The name of the package containing the provider types
     */
    public DefaultTypeToClassMap(String consumerPackageName, String providerPackageName) {
        this.consumerPackageName = consumerPackageName;
        this.providerPackageName = providerPackageName;
    }

    @Override
    public <T> Class<T> consumerTypeToClass(UserDefinedType<ConsumerApiDefinition> type) {
        String className = this.consumerPackageName + "." + type.getInternalName();
        return this.resolveClass(className);
    }

    @Override
    public <T> Class<T> providerTypeToClass(UserDefinedType<ProviderApiDefinition> type) {
        String className = this.providerPackageName + "." + type.getInternalName();
        return this.resolveClass(className);
    }

    @SuppressWarnings("unchecked")
    private <T> Class<T> resolveClass(String className) {
        try {
            return (Class<T>) Class.forName(className);
        } catch (ClassNotFoundException e) {
            throw new RuntimeException(e);
        }
    }

}
