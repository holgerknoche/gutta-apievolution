package gutta.apievolution.customerexample.inprocess.consumer.dynproxy.v6;

import gutta.apievolution.inprocess.ImplementedBy;

@ImplementedBy(POBoxAddressImpl.class)
public interface POBoxAddress extends Address {
    
    Integer getBoxNo();
    
    void setBoxNo(Integer boxNo);

}
