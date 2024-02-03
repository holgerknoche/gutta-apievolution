package gutta.apievolution.fixedformat.unrepresentablevalues;

import gutta.apievolution.fixedformat.objectmapping.MaxLength;
import gutta.apievolution.fixedformat.objectmapping.TypeId;

@TypeId(2)
public class ProviderSubTypeB extends ProviderSuperType {

    @MaxLength(20)
    private String subValueB;

    public String getSubValueB() {
        return this.subValueB;
    }

    public void setSubValueB(String subValueB) {
        this.subValueB = subValueB;
    }
    
}
