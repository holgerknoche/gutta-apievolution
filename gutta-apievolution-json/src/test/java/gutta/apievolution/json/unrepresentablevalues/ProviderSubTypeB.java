package gutta.apievolution.json.unrepresentablevalues;

import com.fasterxml.jackson.annotation.JsonTypeName;

@JsonTypeName("SubTypeB")
public class ProviderSubTypeB extends ProviderSuperType {
    
    private String subValueB;

    public String getSubValueB() {
        return subValueB;
    }

    public void setSubValueB(String subValueB) {
        this.subValueB = subValueB;
    }
    
}
