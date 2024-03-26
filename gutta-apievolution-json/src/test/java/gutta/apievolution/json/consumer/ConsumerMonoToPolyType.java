package gutta.apievolution.json.consumer;

import gutta.apievolution.core.util.EqualityUtil;

import java.util.Objects;

public class ConsumerMonoToPolyType {

    private Integer field1;
    
    public Integer getField1() {
        return this.field1;
    }
    
    public void setField1(Integer field1) {
        this.field1 = field1;
    }
    
    @Override
    public int hashCode() {
        return Objects.hashCode(this.field1);
    }
    
    @Override
    public boolean equals(Object that) {
        return EqualityUtil.equals(this, that, this::equalsInternal);
    }
    
    private boolean equalsInternal(ConsumerMonoToPolyType that) {
        return Objects.equals(this.getField1(), that.getField1());
    }
    
}
