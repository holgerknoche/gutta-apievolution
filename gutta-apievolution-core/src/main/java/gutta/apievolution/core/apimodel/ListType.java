package gutta.apievolution.core.apimodel;

/**
 * List types represent ordered lists of elements of a fixed element type.
 */
public abstract class ListType implements BoundedType {

    private final Type elementType;

    /**
     * Creates an unbounded list type with the given element type.
     * @param elementType The type of the elements within the list type
     * @return The list type
     */
    public static ListType unbounded(final Type elementType) {
        return new UnboundedListType(elementType);
    }

    /**
     * Creates a bounded list type with the given element type and bound.
     * @param elementType The type of the elements within the list type
     * @param bound The bound of the list type
     * @return The list type
     */
    public static ListType bounded(final Type elementType, final int bound) {
        return new BoundedListType(elementType, bound);
    }

    ListType(final Type elementType) {
        this.elementType = elementType;
    }

    /**
     * Returns the element type of this list type.
     * @return see above
     */
    public Type getElementType() {
        return this.elementType;
    }

    /**
     * Returns whether this type is bounded.
     * @return see above
     */
    public abstract boolean isBounded();

    /**
     * Returns the bound of this type. It is only legal to call this method if {@link #isBounded} returns {@code true}.
     * @return see above
     */
    public abstract int getBound();

    @Override
    public int hashCode() {
        return this.elementType.hashCode();
    }

    boolean stateEquals(ListType that) {
        return this.elementType.equals(that.elementType);
    }

}
