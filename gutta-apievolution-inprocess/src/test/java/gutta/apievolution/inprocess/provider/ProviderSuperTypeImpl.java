package gutta.apievolution.inprocess.provider;

abstract class ProviderSuperTypeImpl implements ProviderSuperType {

    private Integer inheritedField;
    
    @Override
    public Integer getInheritedField() {
        return this.inheritedField;
    }
    
    @Override
    public void setInheritedField(Integer inheritedField) {
        this.inheritedField = inheritedField;
    }
    
}
