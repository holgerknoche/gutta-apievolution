package gutta.apievolution.jmh.fixedformat.consumer;

import gutta.apievolution.fixedformat.apimapping.RequestRouter;
import gutta.apievolution.fixedformat.objectmapping.FixedFormatMapper;

import java.nio.charset.Charset;

public class TestMethod25ConsumerProxy extends ConsumerProxyTemplate<ConsumerResult25> {

    public TestMethod25ConsumerProxy(RequestRouter router, FixedFormatMapper mapper, Charset charset) {
        super("testMethod25", ConsumerResult25.class, router, mapper, charset);
    }

}
