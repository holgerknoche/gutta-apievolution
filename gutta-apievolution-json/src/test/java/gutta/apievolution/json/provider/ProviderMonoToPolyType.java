package gutta.apievolution.json.provider;

import com.fasterxml.jackson.annotation.JsonTypeInfo;
import com.fasterxml.jackson.annotation.JsonTypeInfo.Id;
import com.fasterxml.jackson.annotation.JsonTypeName;

@JsonTypeInfo(use = Id.NAME)
@JsonTypeName("ProviderMonoToPolyType")
public class ProviderMonoToPolyType {
    
    private Integer field1;
    
    public Integer getField1() {
        return this.field1;
    }
    
    public void setField1(Integer field1) {
        this.field1 = field1;
    }

}
