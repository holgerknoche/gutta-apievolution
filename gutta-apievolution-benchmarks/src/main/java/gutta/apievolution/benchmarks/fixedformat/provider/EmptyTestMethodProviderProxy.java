package gutta.apievolution.benchmarks.fixedformat.provider;

import gutta.apievolution.fixedformat.apimapping.ApiMappingScript;
import gutta.apievolution.fixedformat.objectmapping.FixedFormatMapper;

import java.nio.charset.Charset;

public class EmptyTestMethodProviderProxy extends ProviderProxyTemplate<EmptyProviderResult> {

    private static final EmptyProviderResult RESULT = new EmptyProviderResult();

    public EmptyTestMethodProviderProxy(ApiMappingScript consumerToProviderScript, ApiMappingScript providerToConsumerScript, FixedFormatMapper mapper,
            Charset charset) {

        super("testMethodEmpty", EmptyProviderResult.class, consumerToProviderScript, providerToConsumerScript, mapper, charset);
    }

    @Override
    protected EmptyProviderResult invokeOperation(ProviderParameter parameter) {
        return RESULT;
    }

}
