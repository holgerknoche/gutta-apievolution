package gutta.apievolution.benchmarks.fixedformat.provider;

import gutta.apievolution.fixedformat.apimapping.ApiMappingScript;
import gutta.apievolution.fixedformat.apimapping.provider.ProviderOperationProxy;
import gutta.apievolution.fixedformat.objectmapping.FixedFormatMapper;

import java.nio.charset.Charset;

abstract class ProviderProxyTemplate<R> extends ProviderOperationProxy<ProviderParameter, R> {

    protected ProviderProxyTemplate(String operationName, Class<R> resultType, ApiMappingScript consumerToProviderScript,
            ApiMappingScript providerToConsumerScript, FixedFormatMapper mapper, Charset charset) {
        
        super(operationName, ProviderParameter.class, resultType, consumerToProviderScript, providerToConsumerScript, mapper, charset);
    }

}
