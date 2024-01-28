package gutta.apievolution.json.unrepresentablevalues;

import com.fasterxml.jackson.annotation.JsonTypeName;

@JsonTypeName("SubTypeA")
public class ProviderSubTypeA extends ProviderSuperType {

    private int subValueA;
    
    private ProviderEnumeration enumValue;

    public int getSubValueA() {
        return subValueA;
    }

    public void setSubValueA(int subValueA) {
        this.subValueA = subValueA;
    }

    public ProviderEnumeration getEnumValue() {
        return enumValue;
    }

    public void setEnumValue(ProviderEnumeration enumValue) {
        this.enumValue = enumValue;
    }        
    
}
