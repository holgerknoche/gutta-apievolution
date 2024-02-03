package gutta.apievolution.inprocess.customerexample.provider;

import gutta.apievolution.inprocess.ImplementedBy;

@ImplementedBy(FormattedAddressImpl.class)
public interface FormattedAddress {
    
    String getAddress();
    
    void setAddress(String address);

}
