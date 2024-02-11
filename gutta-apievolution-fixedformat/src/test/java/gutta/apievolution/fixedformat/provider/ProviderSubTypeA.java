package gutta.apievolution.fixedformat.provider;

import gutta.apievolution.fixedformat.objectmapping.MaxLength;
import gutta.apievolution.fixedformat.objectmapping.TypeId;

@TypeId(5)
public class ProviderSubTypeA extends ProviderSuperType {
    
    @MaxLength(10)
    private String fieldA;
    
    public String getFieldA() {
        return this.fieldA;
    }
    
    public void setFieldA(String fieldA) {
        this.fieldA = fieldA;
    }

}
