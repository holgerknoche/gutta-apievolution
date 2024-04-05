package gutta.apievolution.fixedformat.provider;

import gutta.apievolution.fixedformat.objectmapping.TypeId;

@TypeId(5)
public class ProviderMonoToPolySubTypeA extends ProviderMonoToPolyType {
    
    private Integer field2;
    
    public Integer getField2() {
        return this.field2;
    }
    
    public void setField2(Integer field2) {
        this.field2 = field2;
    }

}
