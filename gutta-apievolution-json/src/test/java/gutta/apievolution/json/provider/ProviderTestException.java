package gutta.apievolution.json.provider;

import com.fasterxml.jackson.annotation.JsonTypeInfo;
import com.fasterxml.jackson.annotation.JsonTypeInfo.Id;
import com.fasterxml.jackson.annotation.JsonTypeName;

@JsonTypeInfo(use = Id.NAME)
@JsonTypeName("ProviderTestException")
public class ProviderTestException {
    
    private int exceptionField;
    
    public int getExceptionField() {
        return this.exceptionField;
    }
    
    public void setExceptionField(int exceptionField) {
        this.exceptionField = exceptionField;
    }

}
