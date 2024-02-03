package gutta.apievolution.core.util;

import java.util.function.Predicate;

/**
 * This class provides auxiliary methods to facilitate the implementation of {@link Object#equals(Object)}.
 */
public class EqualityUtil {

    /**
     * Standard implementation body for {@link Object#equals(Object)} that covers the common special cases, namely identity and the other object being
     * {@code null}. If the two objects are of the same type, the given predicate is invoked to determine whether they are actually equal.
     * 
     * <p/>
     * This method is intended to be used as follows:
     * 
     * <pre>
     * <code>
     * 
     * class Foo extends Bar {
     * 
     *   public boolean equals(Object that) {
     *     return EqualityUtil.equals(this, that, this::equalsInternal);
     *   }
     *   
     *   protected equalsInternal(Foo that) {
     *     return super.equalsInternal(that) 
     *         && ...
     *   }
     * </code>
     * </pre>
     * 
     * @param <T>               The (expected) type of the objects that are compared
     * @param thisObject        The "this" object, must not be {@code null}
     * @param thatObject        The "other" object that "this" is compared against
     * @param equalityPredicate A predicate to compare two objects of the same type
     * @return Whether the objects are equal
     */
    @SuppressWarnings("unchecked")
    public static <T> boolean equals(T thisObject, Object thatObject, Predicate<T> equalityPredicate) {
        if (thisObject == thatObject) {
            return true;
        } else if (thatObject != null && (thisObject.getClass() == thatObject.getClass())) {
            return equalityPredicate.test((T) thatObject);
        } else {
            return false;
        }
    }

    private EqualityUtil() {
        // Private constructor
    }

}
