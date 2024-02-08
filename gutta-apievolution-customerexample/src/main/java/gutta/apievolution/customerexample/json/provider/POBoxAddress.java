package gutta.apievolution.customerexample.json.provider;

import com.fasterxml.jackson.annotation.JsonTypeName;

@JsonTypeName("POBoxAddress")
public class POBoxAddress extends Address {
    
    private int boxNo;

    public int getBoxNo() {
        return this.boxNo;
    }

    public void setBoxNo(int boxNo) {
        this.boxNo = boxNo;
    }        

}
