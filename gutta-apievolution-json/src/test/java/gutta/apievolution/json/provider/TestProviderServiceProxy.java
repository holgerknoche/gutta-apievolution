package gutta.apievolution.json.provider;

import gutta.apievolution.core.apimodel.provider.RevisionHistory;
import gutta.apievolution.dsl.ProviderApiLoader;
import gutta.apievolution.json.ProviderServiceProxy;

import java.util.Arrays;
import java.util.HashSet;

public class TestProviderServiceProxy extends ProviderServiceProxy<ProviderParameter, ProviderResult> {

    private static final String SERVICE_NAME = "TestService";

    public TestProviderServiceProxy() {
        super(SERVICE_NAME, ProviderApiLoader.loadHistoryFromClasspath("apis/provider-revision-1.api",
                "apis/provider-revision-2.api"), new HashSet<>(Arrays.asList(0, 1)), "TestParameter",
                "TestResult", ProviderParameter.class);
    }

    @Override
    protected ProviderResult invokeService(ProviderParameter parameter) {
        ProviderResult result = new ProviderResult();
        result.setRetField(parameter.getFieldA() + "X");
        result.setResultEnum(ProviderEnum.VALUE_2);
        return result;
    }
}
