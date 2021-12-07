package gutta.apievolution.core.apimodel;

/**
 * This type represents the bounded variant of the {@link ListType}.
 */
class BoundedListType extends ListType {

    private final int bound;

    public BoundedListType(final Type elementType, final int bound) {
        super(elementType);

        this.bound = bound;
    }

    @Override
    public boolean isBounded() {
        return true;
    }
    
    @Override
    public int getBound() {
        return this.bound;
    }

    @Override
    public int hashCode() {
        return super.hashCode() + this.bound;
    }

    @Override
    public boolean equals(Object that) {
        if (this == that) {
            return true;
        } else if (that instanceof BoundedListType) {
            return this.stateEquals((BoundedListType) that);
        } else {
            return false;
        }
    }

    boolean stateEquals(BoundedListType that) {
        return super.stateEquals(that) &&
                this.bound == that.bound;
    }
    
}
