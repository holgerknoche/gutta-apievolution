package gutta.apievolution.json.provider;

import gutta.apievolution.dsl.ProviderApiLoader;
import gutta.apievolution.json.ProviderOperationProxy;

import java.util.Arrays;
import java.util.HashSet;

public class PolyOperationProviderProxy extends ProviderOperationProxy<ProviderSuperType, ProviderSuperType> {

    private static final String OPERATION_NAME = "polyOperation";

    public PolyOperationProviderProxy() {
        super(OPERATION_NAME, ProviderApiLoader.loadHistoryFromClasspath("apis/provider-revision-1.api", "apis/provider-revision-2.api"),
                new HashSet<>(Arrays.asList(0, 1)), "ProviderSuperType", "ProviderSuperType", ProviderSuperType.class);
    }
    
    @Override
    protected ProviderSuperType invokeOperation(ProviderSuperType parameter) {
        return parameter;
    }

}
