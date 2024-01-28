package gutta.apievolution.json.unrepresentablevalues;

import com.fasterxml.jackson.annotation.JsonTypeName;

@JsonTypeName("SubTypeA")
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
