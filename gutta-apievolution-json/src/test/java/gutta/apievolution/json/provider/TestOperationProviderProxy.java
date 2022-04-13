package gutta.apievolution.json.provider;

import gutta.apievolution.dsl.ProviderApiLoader;
import gutta.apievolution.json.ProviderOperationProxy;

import java.util.*;

public class TestOperationProviderProxy
        extends ProviderOperationProxy<ProviderParameter, ProviderResult> {

    private static final String OPERATION_NAME = "testOperation";

    public TestOperationProviderProxy() {
        super(OPERATION_NAME,
                ProviderApiLoader.loadHistoryFromClasspath("apis/provider-revision-1.api",
                        "apis/provider-revision-2.api"),
                new HashSet<>(Arrays.asList(0, 1)), "TestParameter", "TestResult", ProviderParameter.class);
    }

    @Override
    protected ProviderResult invokeOperation(ProviderParameter parameter) {
        ProviderResult result = new ProviderResult();
        result.setRetField(parameter.getFieldA() + "X");
        result.setResultEnum(ProviderEnum.VALUE_2);

        List<ProviderEnum> resultList = new ArrayList<>(parameter.getTestList());
        Collections.reverse(resultList);
        result.setResultList(resultList);

        return result;
    }
}
