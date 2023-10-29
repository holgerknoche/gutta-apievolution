package gutta.apievolution.inprocess.provider;

public class ProviderApiV1 {
    
    public TestResult testOperation(TestParameter parameter) {
        TestResult result = new TestResult();
        
        result.setResultEnum(parameter.getTestEnum());
        // TODO
        return null;
    }
    

}
