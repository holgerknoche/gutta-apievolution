package gutta.apievolution.benchmarks.fixedformat.consumer;

import gutta.apievolution.fixedformat.apimapping.RequestRouter;
import gutta.apievolution.fixedformat.objectmapping.FixedFormatMapper;

import java.nio.charset.Charset;

public class EmptyTestMethodConsumerProxy extends ConsumerProxyTemplate<EmptyConsumerResult> {

    public EmptyTestMethodConsumerProxy(RequestRouter router, FixedFormatMapper mapper, Charset charset) {
        super("testMethodEmpty", EmptyConsumerResult.class, router, mapper, charset);
    }

}
