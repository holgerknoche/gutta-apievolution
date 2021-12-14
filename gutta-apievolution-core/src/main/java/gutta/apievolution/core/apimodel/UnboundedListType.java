package gutta.apievolution.core.apimodel;

class UnboundedListType extends ListType {

    UnboundedListType(final Type elementType) {
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

    @Override
    public int hashCode() {
        return -1;
    }

    @Override
    public boolean equals(Object that) {
        if (this == that) {
            return true;
        } else if (that instanceof UnboundedListType) {
            return this.stateEquals((UnboundedListType) that);
        } else {
            return false;
        }
    }

    boolean stateEquals(UnboundedListType that) {
        return super.stateEquals(that);
    }

    @Override
    public <R> R accept(TypeVisitor<R> visitor) {
        return visitor.handleUnboundedListType(this);
    }

}
