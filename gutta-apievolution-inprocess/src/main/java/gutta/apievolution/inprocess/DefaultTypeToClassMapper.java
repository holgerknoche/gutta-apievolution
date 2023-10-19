package gutta.apievolution.inprocess;

import gutta.apievolution.core.apimodel.consumer.ConsumerApiDefinition;
import gutta.apievolution.core.apimodel.consumer.ConsumerRecordType;
import gutta.apievolution.core.apimodel.provider.ProviderRecordType;

public class DefaultTypeToClassMapper implements TypeToClassMap {

    private String consumerPackageName;
    
    private String providerPackageName;

    public DefaultTypeToClassMapper(ConsumerApiDefinition consumerApiDefinition) {
        this.consumerPackageName = consumerApiDefinition.getName().toString();
        this.providerPackageName = consumerApiDefinition.getReferencedApiName();
    }
    
    public DefaultTypeToClassMapper(String consumerPackageName, String providerPackageName) {
        this.consumerPackageName = consumerPackageName;
        this.providerPackageName = providerPackageName;
    }
    
    @Override
    public <T> Class<T> consumerRecordTypeToClass(ConsumerRecordType type) {
        String className = this.consumerPackageName + "." + type.getInternalName();
        return this.resolveClass(className);
    }

    @Override
    public <T> Class<T> providerRecordTypeToClass(ProviderRecordType type) {
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
