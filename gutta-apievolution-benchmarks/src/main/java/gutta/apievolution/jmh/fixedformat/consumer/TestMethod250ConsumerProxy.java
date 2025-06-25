package gutta.apievolution.jmh.fixedformat.consumer;

import gutta.apievolution.fixedformat.apimapping.RequestRouter;
import gutta.apievolution.fixedformat.objectmapping.FixedFormatMapper;

import java.nio.charset.Charset;

public class TestMethod250ConsumerProxy extends ConsumerProxyTemplate<ConsumerResult250> {

    public TestMethod250ConsumerProxy(RequestRouter router, FixedFormatMapper mapper, Charset charset) {
        super("testMethod250", ConsumerResult250.class, router, mapper, charset);
    }

}
