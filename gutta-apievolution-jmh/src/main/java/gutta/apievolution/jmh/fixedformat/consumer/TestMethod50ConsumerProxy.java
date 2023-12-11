package gutta.apievolution.jmh.fixedformat.consumer;

import java.nio.charset.Charset;

import gutta.apievolution.fixedformat.apimapping.RequestRouter;
import gutta.apievolution.fixedformat.objectmapping.FixedFormatMapper;

public class TestMethod50ConsumerProxy extends ConsumerProxyTemplate<ConsumerResult50> {

    public TestMethod50ConsumerProxy(RequestRouter router, FixedFormatMapper mapper, Charset charset) {
        super("testMethod50", ConsumerResult50.class, router, mapper, charset);
    }

}
