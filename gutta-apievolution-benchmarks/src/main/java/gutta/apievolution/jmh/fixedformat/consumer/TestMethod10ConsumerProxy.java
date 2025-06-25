package gutta.apievolution.jmh.fixedformat.consumer;

import gutta.apievolution.fixedformat.apimapping.RequestRouter;
import gutta.apievolution.fixedformat.objectmapping.FixedFormatMapper;

import java.nio.charset.Charset;

public class TestMethod10ConsumerProxy extends ConsumerProxyTemplate<ConsumerResult10> {

    public TestMethod10ConsumerProxy(RequestRouter router, FixedFormatMapper mapper, Charset charset) {
        super("testMethod10", ConsumerResult10.class, router, mapper, charset);
    }

}
