package gutta.apievolution.fixedformat.customerexample.consumer.v3;

import java.nio.charset.Charset;

import gutta.apievolution.fixedformat.apimapping.RequestRouter;
import gutta.apievolution.fixedformat.apimapping.consumer.ConsumerOperationProxy;
import gutta.apievolution.fixedformat.objectmapping.FixedFormatMapper;

class UpsertOperationConsumerProxyV3 extends ConsumerOperationProxy<Customer, Customer> {

	public UpsertOperationConsumerProxyV3(RequestRouter router, FixedFormatMapper mapper, Charset charset) {
        super("upsert", Customer.class, Customer.class, router, mapper, charset);
    }
	
}
