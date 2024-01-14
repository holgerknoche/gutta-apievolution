package gutta.apievolution.fixedformat.customerexample.consumer.v1;

import gutta.apievolution.fixedformat.apimapping.RequestRouter;
import gutta.apievolution.fixedformat.apimapping.consumer.ConsumerOperationProxy;
import gutta.apievolution.fixedformat.objectmapping.FixedFormatMapper;

import java.nio.charset.Charset;

class UpsertOperationConsumerProxyV1 extends ConsumerOperationProxy<Customer, Customer> {

    public UpsertOperationConsumerProxyV1(RequestRouter router, FixedFormatMapper mapper, Charset charset) {
        super("upsert", Customer.class, Customer.class, router, mapper, charset);
    }

}
