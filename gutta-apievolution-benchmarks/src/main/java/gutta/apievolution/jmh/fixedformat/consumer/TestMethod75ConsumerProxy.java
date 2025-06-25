package gutta.apievolution.jmh.fixedformat.consumer;

import gutta.apievolution.fixedformat.apimapping.RequestRouter;
import gutta.apievolution.fixedformat.objectmapping.FixedFormatMapper;

import java.nio.charset.Charset;

public class TestMethod75ConsumerProxy extends ConsumerProxyTemplate<ConsumerResult75> {

    public TestMethod75ConsumerProxy(RequestRouter router, FixedFormatMapper mapper, Charset charset) {
        super("testMethod75", ConsumerResult75.class, router, mapper, charset);
    }

}
