package gutta.apievolution.core.apimodel;

class UnboundedListType extends ListType {

    public UnboundedListType(final Type elementType) {
        super(elementType);
    }

    @Override
    public boolean isBounded() {
        return false;
    }
    
    @Override
    public int getBound() {
        throw new UnsupportedOperationException();
    }
    
}
