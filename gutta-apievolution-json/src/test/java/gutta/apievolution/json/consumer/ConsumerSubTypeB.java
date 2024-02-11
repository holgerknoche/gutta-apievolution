package gutta.apievolution.json.consumer;

import com.fasterxml.jackson.annotation.JsonTypeName;
import gutta.apievolution.core.util.EqualityUtil;

import java.util.Objects;

@JsonTypeName("ConsumerSubTypeB")
public class ConsumerSubTypeB extends ConsumerSuperType {
    
    private Integer fieldB;
    
    public Integer getFieldB() {
        return this.fieldB;
    }
    
    public void setFieldB(Integer fieldB) {
        this.fieldB = fieldB;
    }
    
    @Override
    public int hashCode() {
        return Objects.hashCode(this.fieldB);
    }
    
    @Override
    public boolean equals(Object that) {
        return EqualityUtil.equals(this, that, this::equalsInternal);
    }
    
    private boolean equalsInternal(ConsumerSubTypeB that) {
        return Objects.equals(this.getFieldB(), that.getFieldB());
    }

}
