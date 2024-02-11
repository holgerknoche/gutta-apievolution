package gutta.apievolution.fixedformat.consumer;

import gutta.apievolution.core.util.EqualityUtil;
import gutta.apievolution.fixedformat.objectmapping.MaxLength;
import gutta.apievolution.fixedformat.objectmapping.TypeId;

import java.util.Objects;

@TypeId(5)
public class ConsumerSubTypeA extends ConsumerSuperType {
    
    @MaxLength(10)
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
