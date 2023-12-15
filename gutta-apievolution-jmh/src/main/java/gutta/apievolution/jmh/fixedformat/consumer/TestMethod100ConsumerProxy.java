package gutta.apievolution.jmh.fixedformat.consumer;

import gutta.apievolution.fixedformat.apimapping.RequestRouter;
import gutta.apievolution.fixedformat.objectmapping.FixedFormatMapper;

import java.nio.charset.Charset;

public class TestMethod100ConsumerProxy extends ConsumerProxyTemplate<ConsumerResult100> {

    public TestMethod100ConsumerProxy(RequestRouter router, FixedFormatMapper mapper, Charset charset) {
        super("testMethod100", ConsumerResult100.class, router, mapper, charset);
    }

}
