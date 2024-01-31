package gutta.apievolution.fixedformat.customerexample.provider;

import gutta.apievolution.fixedformat.apimapping.ApiMappingScript;
import gutta.apievolution.fixedformat.apimapping.provider.ProviderOperationProxy;
import gutta.apievolution.fixedformat.objectmapping.FixedFormatMapper;

import java.nio.charset.Charset;

public class UpsertOperationProviderProxy extends ProviderOperationProxy<Customer, Customer> {

    public UpsertOperationProviderProxy(ApiMappingScript consumerToProviderScript, ApiMappingScript providerToConsumerScript, FixedFormatMapper mapper,
            Charset charset) {
        
        super("upsert", Customer.class, Customer.class, consumerToProviderScript, providerToConsumerScript, mapper, charset);
    }
    
    @Override
    protected Customer invokeOperation(Customer parameter) {
        return parameter;
    }

}
