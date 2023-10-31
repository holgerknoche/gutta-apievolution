package gutta.apievolution.inprocess;

import gutta.apievolution.core.apimodel.UserDefinedType;
import gutta.apievolution.core.apimodel.consumer.ConsumerApiDefinition;
import gutta.apievolution.core.apimodel.provider.ProviderApiDefinition;

public class DefaultTypeToClassMap implements UDTToClassMap {

    private final String consumerPackageName;
    
    private final String providerPackageName;

    public DefaultTypeToClassMap(ConsumerApiDefinition consumerApiDefinition) {
        this.consumerPackageName = consumerApiDefinition.getName().toString();
        this.providerPackageName = consumerApiDefinition.getReferencedApiName();
    }
    
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
