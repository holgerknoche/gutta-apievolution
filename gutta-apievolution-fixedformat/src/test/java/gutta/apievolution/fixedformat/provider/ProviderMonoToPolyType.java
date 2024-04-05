package gutta.apievolution.fixedformat.provider;

import gutta.apievolution.fixedformat.objectmapping.SubTypes;
import gutta.apievolution.fixedformat.objectmapping.TypeId;

@TypeId(4)
@SubTypes({ProviderMonoToPolySubTypeA.class, ProviderMonoToPolySubTypeB.class})
public class ProviderMonoToPolyType {
    
    private Integer field1;
    
    public Integer getField1() {
        return this.field1;
    }
    
    public void setField1(Integer field1) {
        this.field1 = field1;
    }

}
