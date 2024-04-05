package gutta.apievolution.jmh.json.provider;

import gutta.apievolution.core.apimodel.provider.RevisionHistory;
import gutta.apievolution.json.provider.ProviderOperationProxy;

import java.util.Set;

abstract class ProviderProxyTemplate<R> extends ProviderOperationProxy<ProviderParameter, R> {

    protected ProviderProxyTemplate(String serviceName, RevisionHistory revisionHistory, Set<Integer> supportedRevisions, String resultTypeName) {
        super(serviceName, revisionHistory, supportedRevisions, "ProviderParameter", resultTypeName, ProviderParameter.class);
    }

}
