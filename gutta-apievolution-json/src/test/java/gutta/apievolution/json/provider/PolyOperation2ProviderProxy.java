package gutta.apievolution.json.provider;

import gutta.apievolution.dsl.ProviderApiLoader;
import gutta.apievolution.json.ProviderOperationProxy;

import java.util.Arrays;
import java.util.HashSet;

public class PolyOperation2ProviderProxy extends ProviderOperationProxy<ProviderStructureWithPolyField, ProviderStructureWithPolyField> {

    private static final String OPERATION_NAME = "polyOperation2";

    public PolyOperation2ProviderProxy() {
        super(OPERATION_NAME, ProviderApiLoader.loadHistoryFromClasspath("apis/provider-revision-1.api", "apis/provider-revision-2.api"),
                new HashSet<>(Arrays.asList(0, 1)), "ProviderStructureWithPolyField", "ProviderStructureWithPolyField", ProviderStructureWithPolyField.class);
    }
    
    @Override
    protected ProviderStructureWithPolyField invokeOperation(ProviderStructureWithPolyField parameter) {
        return parameter;
    }

}
