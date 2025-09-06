package gutta.apievolution.benchmarks.json.provider;

import gutta.apievolution.core.apimodel.provider.RevisionHistory;

import java.util.Set;

public class TestMethodEmptyProviderProxy extends ProviderProxyTemplate<EmptyProviderResult> {

    private static final EmptyProviderResult RESULT = new EmptyProviderResult();
    
    public TestMethodEmptyProviderProxy(RevisionHistory revisionHistory, Set<Integer> supportedRevisions) {
        super("testMethodEmpty", revisionHistory, supportedRevisions, "EmptyProviderResult");
    }

    @Override
    protected EmptyProviderResult invokeOperation(ProviderParameter parameter) {
        return RESULT;
    }

}
