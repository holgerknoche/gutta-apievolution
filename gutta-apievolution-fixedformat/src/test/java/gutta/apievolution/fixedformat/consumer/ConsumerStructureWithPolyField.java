package gutta.apievolution.fixedformat.consumer;

import gutta.apievolution.core.util.EqualityUtil;

import java.util.Objects;

public class ConsumerStructureWithPolyField {
    
    private ConsumerSuperType field;
    
    public ConsumerSuperType getField() {
        return this.field;
    }
    
    public void setField(ConsumerSuperType field) {
        this.field = field;
    }
    
    @Override
    public int hashCode() {
        return Objects.hashCode(this.field);
    }
    
    @Override
    public boolean equals(Object that) {
        return EqualityUtil.equals(this, that, this::equalsInternal);
    }
    
    private boolean equalsInternal(ConsumerStructureWithPolyField that) {
        return Objects.equals(this.getField(), that.getField());
    }

}
