package gutta.apievolution.json.provider;

import com.fasterxml.jackson.annotation.JsonTypeName;

@JsonTypeName("ProviderMonoToPolySubTypeB")
public class ProviderMonoToPolySubTypeB extends ProviderMonoToPolyType {
    
    private String field3;
    
    public String getField3() {
        return this.field3;
    }
    
    public void setField3(String field3) {
        this.field3 = field3;
    }

}
