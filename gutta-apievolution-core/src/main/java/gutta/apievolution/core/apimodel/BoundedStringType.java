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

}
