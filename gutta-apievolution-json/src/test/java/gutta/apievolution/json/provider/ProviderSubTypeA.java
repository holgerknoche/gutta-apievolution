package gutta.apievolution.json.provider;

import com.fasterxml.jackson.annotation.JsonTypeName;

@JsonTypeName("ProviderSubTypeA")
public class ProviderSubTypeA extends ProviderSuperType {
    
    private String fieldA;
    
    public String getFieldA() {
        return this.fieldA;
    }
    
    public void setFieldA(String fieldA) {
        this.fieldA = fieldA;
    }

}
