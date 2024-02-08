package gutta.apievolution.customerexample.fixedformat.consumer.v6;

import java.nio.charset.Charset;

import gutta.apievolution.fixedformat.apimapping.RequestRouter;
import gutta.apievolution.fixedformat.apimapping.consumer.ConsumerOperationProxy;
import gutta.apievolution.fixedformat.objectmapping.FixedFormatMapper;

public class UpsertOperationConsumerProxyV6 extends ConsumerOperationProxy<Customer, Customer> {

	public UpsertOperationConsumerProxyV6(RequestRouter router, FixedFormatMapper mapper, Charset charset) {
        super("upsert", Customer.class, Customer.class, router, mapper, charset);
    }
	
}
