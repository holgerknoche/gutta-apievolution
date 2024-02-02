package gutta.apievolution.inprocess.customerexample.consumer.dynproxy.v6;

import gutta.apievolution.inprocess.ImplementedBy;

@ImplementedBy(POBoxAddressImpl.class)
public interface POBoxAddress extends Address {
    
    Integer getBoxNo();
    
    void setBoxNo(Integer boxNo);

}
