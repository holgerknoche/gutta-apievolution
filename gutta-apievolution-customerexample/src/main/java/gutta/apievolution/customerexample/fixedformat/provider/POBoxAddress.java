package gutta.apievolution.customerexample.fixedformat.provider;

import gutta.apievolution.fixedformat.objectmapping.TypeId;

@TypeId(4)
public class POBoxAddress extends Address {
    
    private int boxNo;

    public int getBoxNo() {
        return this.boxNo;
    }

    public void setBoxNo(int boxNo) {
        this.boxNo = boxNo;
    }        

}
