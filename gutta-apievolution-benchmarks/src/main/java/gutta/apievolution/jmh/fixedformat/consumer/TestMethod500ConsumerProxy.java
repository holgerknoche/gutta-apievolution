package gutta.apievolution.jmh.fixedformat.consumer;

import gutta.apievolution.fixedformat.apimapping.RequestRouter;
import gutta.apievolution.fixedformat.objectmapping.FixedFormatMapper;

import java.nio.charset.Charset;

public class TestMethod500ConsumerProxy extends ConsumerProxyTemplate<ConsumerResult500> {

    public TestMethod500ConsumerProxy(RequestRouter router, FixedFormatMapper mapper, Charset charset) {
        super("testMethod500", ConsumerResult500.class, router, mapper, charset);
    }

}
