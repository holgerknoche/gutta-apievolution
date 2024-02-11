package gutta.apievolution.json.provider;

import com.fasterxml.jackson.annotation.JsonTypeName;

@JsonTypeName("ProviderSubTypeB")
public class ProviderSubTypeB extends ProviderSuperType {
    
    private Integer fieldB;
    
    public Integer getFieldB() {
        return this.fieldB;
    }
    
    public void setFieldB(Integer fieldB) {
        this.fieldB = fieldB;
    }

}
