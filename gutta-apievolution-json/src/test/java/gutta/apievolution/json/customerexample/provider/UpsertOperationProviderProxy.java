package gutta.apievolution.json.customerexample.provider;

import gutta.apievolution.core.apimodel.provider.RevisionHistory;
import gutta.apievolution.json.ProviderOperationProxy;

import java.util.Set;

public class UpsertOperationProviderProxy extends ProviderOperationProxy<Customer, Customer> {

    public UpsertOperationProviderProxy(RevisionHistory revisionHistory, Set<Integer> supportedRevisions) {        
        super("upsert", revisionHistory, supportedRevisions, "Customer", "Customer", Customer.class);
    }
    
    @Override
    protected Customer invokeOperation(Customer parameter) {
        return parameter;
    }

}
