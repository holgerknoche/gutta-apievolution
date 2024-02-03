package gutta.apievolution.fixedformat.unrepresentablevalues;

import gutta.apievolution.fixedformat.objectmapping.TypeId;

@TypeId(1)
public class ProviderSubTypeA extends ProviderSuperType {
    
    private int subValueA;
    
    private ProviderEnumeration enumValue;

    public int getSubValueA() {
        return this.subValueA;
    }

    public void setSubValueA(int subValueA) {
        this.subValueA = subValueA;
    }

    public ProviderEnumeration getEnumValue() {
        return this.enumValue;
    }

    public void setEnumValue(ProviderEnumeration enumValue) {
        this.enumValue = enumValue;
    }

}
