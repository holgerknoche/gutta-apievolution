package gutta.apievolution.inprocess.provider;

import gutta.apievolution.inprocess.ImplementedBy;

@ImplementedBy(ProviderSuperTypeImpl.class)
public interface ProviderSuperType {
    
    Integer getInheritedField();
    
    void setInheritedField(Integer inheritedField);

}
