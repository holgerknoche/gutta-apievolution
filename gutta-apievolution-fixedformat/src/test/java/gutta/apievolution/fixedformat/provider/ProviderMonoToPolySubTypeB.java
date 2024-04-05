package gutta.apievolution.fixedformat.provider;

import gutta.apievolution.fixedformat.objectmapping.MaxLength;
import gutta.apievolution.fixedformat.objectmapping.TypeId;

@TypeId(6)
public class ProviderMonoToPolySubTypeB extends ProviderMonoToPolyType {
    
    @MaxLength(10)
    private String field3;
    
    public String getField3() {
        return this.field3;
    }
    
    public void setField3(String field3) {
        this.field3 = field3;
    }

}
