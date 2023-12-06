package gutta.apievolution.inprocess.provider;

import gutta.apievolution.inprocess.ImplementedBy;

@ImplementedBy(ProviderSubTypeAImpl.class)
public interface ProviderSubTypeA extends ProviderSuperType {

    Integer getFieldA();
    
    void setFieldA(Integer fieldA);
    
}
