package gutta.apievolution.core.apimodel;

import gutta.apievolution.core.util.EqualityUtil;

/**
 * This type represents the bounded variant of the {@link ListType}.
 */
public class BoundedListType extends ListType {

    private final int bound;

    BoundedListType(final Type elementType, final int bound) {
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
        return EqualityUtil.equals(this, that, this::stateEquals);
    }

    boolean stateEquals(BoundedListType that) {
        return super.stateEquals(that) && this.bound == that.bound;
    }

    @Override
    public <R> R accept(TypeVisitor<R> visitor) {
        return visitor.handleBoundedListType(this);
    }

    @Override
    public String toString() {
        return this.getElementType() + "[" + this.bound + "]";
    }

}
