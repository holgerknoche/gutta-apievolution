package gutta.apievolution.core.apimodel;

import gutta.apievolution.core.util.EqualityUtil;

/**
 * This type represents the unbounded variant of the {@link ListType}.
 */
public class UnboundedListType extends ListType {

    UnboundedListType(final Type elementType) {
        super(elementType);
    }

    @Override
    public boolean isUnbounded() {
        return true;
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
        return EqualityUtil.equals(this, that, this::stateEquals);
    }

    @Override
    public <R> R accept(TypeVisitor<R> visitor) {
        return visitor.handleUnboundedListType(this);
    }

}
