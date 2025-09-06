package gutta.apievolution.benchmarks.fixedformat.consumer;

import gutta.apievolution.fixedformat.apimapping.RequestRouter;
import gutta.apievolution.fixedformat.apimapping.consumer.ConsumerOperationProxy;
import gutta.apievolution.fixedformat.objectmapping.FixedFormatMapper;

import java.nio.charset.Charset;

abstract class ConsumerProxyTemplate<R> extends ConsumerOperationProxy<ConsumerParameter, R> {

    protected ConsumerProxyTemplate(String operationName, Class<R> resultType, RequestRouter router, FixedFormatMapper mapper, Charset charset) {
        super(operationName, ConsumerParameter.class, resultType, router, mapper, charset);
    }

}
