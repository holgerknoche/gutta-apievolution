package gutta.apievolution.inprocess.provider;

public class ProviderSubTypeAImpl extends ProviderSuperTypeImpl implements ProviderSubTypeA {

    private Integer fieldA;
    
    @Override
    public Integer getFieldA() {
        return this.fieldA;
    }

    @Override
    public void setFieldA(Integer fieldA) {
        this.fieldA = fieldA;
    }

}
