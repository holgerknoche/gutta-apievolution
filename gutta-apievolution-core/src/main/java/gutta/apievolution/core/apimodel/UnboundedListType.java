package gutta.apievolution.core.apimodel;

/**
 * This type represents the unbounded variant of the {@link ListType}.
 */
public class UnboundedListType extends ListType {

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

    @Override
    public <R> R accept(TypeVisitor<R> visitor) {
        return visitor.handleUnboundedListType(this);
    }

}
