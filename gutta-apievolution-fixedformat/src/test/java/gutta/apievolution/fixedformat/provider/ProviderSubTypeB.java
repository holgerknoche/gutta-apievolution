package gutta.apievolution.fixedformat.provider;

import gutta.apievolution.fixedformat.objectmapping.TypeId;

@TypeId(6)
public class ProviderSubTypeB extends ProviderSuperType {
    
    private Integer fieldB;
    
    public Integer getFieldB() {
        return this.fieldB;
    }
    
    public void setFieldB(Integer fieldB) {
        this.fieldB = fieldB;
    }

}
