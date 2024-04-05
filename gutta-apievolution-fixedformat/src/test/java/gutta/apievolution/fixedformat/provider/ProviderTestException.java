package gutta.apievolution.fixedformat.provider;

import gutta.apievolution.fixedformat.objectmapping.TypeId;

@TypeId(7)
public class ProviderTestException {

    private Integer exceptionField;
    
    public Integer getExceptionField() {
        return this.exceptionField;
    }
    
    public void setExceptionField(Integer exceptionField) {
        this.exceptionField = exceptionField;
    }
    
}
