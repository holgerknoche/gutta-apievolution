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
    
}
