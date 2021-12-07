package gutta.apievolution.core.apimodel;

/**
 * This type represents the bounded variant of the {@link StringType}.
 */
class BoundedStringType extends StringType {

    private final int bound;

    public BoundedStringType(int bound) {
        this.bound = bound;
    }

    public int getBound() {
        return this.bound;
    }

    @Override
    public int hashCode() {
        return super.hashCode() + this.bound;
    }

    public boolean equals(Object that) {
        if (this == that) {
            return true;
        } else if (that instanceof BoundedStringType) {
            return this.stateEquals((BoundedStringType) that);
        } else {
            return false;
        }
    }

    boolean stateEquals(BoundedStringType that) {
        return this.bound == that.bound;
    }

}
