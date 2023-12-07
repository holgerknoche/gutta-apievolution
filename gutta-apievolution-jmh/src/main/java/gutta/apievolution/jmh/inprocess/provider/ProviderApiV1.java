package gutta.apievolution.jmh.inprocess.provider;

public class ProviderApiV1 {
    
    private static final ProviderResult10 RESULT_10 = createProviderResult10();
    
    private static ProviderResult10 createProviderResult10() {
        ProviderResult10 result = new ProviderResult10();
        
        return result;
    }
    
    public ProviderResult10 testMethod10(ProviderParameter parameter) {
        return RESULT_10;
    }

}
