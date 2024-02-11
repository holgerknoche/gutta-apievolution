package gutta.apievolution.json.consumer;

import com.fasterxml.jackson.annotation.JsonTypeName;
import gutta.apievolution.core.util.EqualityUtil;

import java.util.Objects;

@JsonTypeName("ConsumerSubTypeA")
public class ConsumerSubTypeA extends ConsumerSuperType {
    
    private String fieldA;
    
    public String getFieldA() {
        return this.fieldA;
    }
    
    public void setFieldA(String fieldA) {
        this.fieldA = fieldA;
    }
    
    @Override
    public int hashCode() {
        return Objects.hashCode(this.fieldA);
    }
    
    @Override
    public boolean equals(Object that) {
        return EqualityUtil.equals(this, that, this::equalsInternal);
    }
    
    private boolean equalsInternal(ConsumerSubTypeA that) {
        return Objects.equals(this.getFieldA(), that.getFieldA());
    }

}
