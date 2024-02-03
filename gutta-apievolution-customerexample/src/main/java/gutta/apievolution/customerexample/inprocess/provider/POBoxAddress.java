package gutta.apievolution.customerexample.inprocess.provider;

import gutta.apievolution.inprocess.ImplementedBy;

@ImplementedBy(POBoxAddressImpl.class)
public interface POBoxAddress extends Address {

    Integer getBoxNo();
    
    void setBoxNo(Integer boxNo);
    
}
