package gutta.apievolution.core.apimodel;

/**
 * Interface for bounded types, i.e., types that can carry a user-defined bound (such as a string type of a given
 * length).
 */
public interface BoundedType extends BasicType {

    @Override
    default boolean isBoundedType() {
        return true;
    }

}
