package gutta.apievolution.json.consumer;

import gutta.apievolution.core.util.EqualityUtil;

import java.util.Objects;

public class ConsumerStructureWithMonoToPolyField {
    
    private ConsumerMonoToPolyType field;
    
    public ConsumerMonoToPolyType getField() {
        return this.field;
    }
    
    public void setField(ConsumerMonoToPolyType field) {
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
    
    private boolean equalsInternal(ConsumerStructureWithMonoToPolyField that) {
        return Objects.equals(this.getField(), that.getField());
    }

}
