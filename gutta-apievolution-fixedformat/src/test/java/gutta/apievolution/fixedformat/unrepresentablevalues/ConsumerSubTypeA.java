package gutta.apievolution.fixedformat.unrepresentablevalues;

import gutta.apievolution.fixedformat.objectmapping.TypeId;

@TypeId(1)
public class ConsumerSubTypeA extends ConsumerSuperType {
    
    private Integer subValueA;
    
    private ConsumerEnumeration enumValue;

    public Integer getSubValueA() {
        return this.subValueA;
    }

    public void setSubValueA(Integer subValueA) {
        this.subValueA = subValueA;
    }

    public ConsumerEnumeration getEnumValue() {
        return this.enumValue;
    }

    public void setEnumValue(ConsumerEnumeration enumValue) {
        this.enumValue = enumValue;
    }

}
