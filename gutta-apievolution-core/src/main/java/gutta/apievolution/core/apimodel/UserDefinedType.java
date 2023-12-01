package gutta.apievolution.core.apimodel;

import java.util.HashSet;
import java.util.Set;

/**
 * Interface for user-defined types.
 * 
 * @param <A> The type of API definition in which this UDT resides
 */
public interface UserDefinedType<A extends ApiDefinition<A, ?>> extends Type {

    /**
     * Returns the type's id.
     * 
     * @return see above
     */
    int getTypeId();

    /**
     * Return's the type's public name.
     * 
     * @return see above
     */
    String getPublicName();

    /**
     * Return's the type's internal name.
     * 
     * @return see above
     */
    String getInternalName();

    /**
     * Return's the API definition this type resides in.
     * 
     * @return see above
     */
    A getOwner();
    
    /**
     * Denotes whether this type is a record type.
     * @return see above
     */
    default boolean isRecord() {
        return false;
    }
    
    /**
     * Denotes whether this type is an enum type.
     * @return see above
     */
    default boolean isEnum() {
        return false;
    }
    
    @Override
    default boolean isUserDefined() {
        return true;
    }
    
    /**
     * Denotes whether this type is a provider type.
     *  
     * @return see above
     */
    boolean isProviderType();
    
    /**
     * Denotes whether this type is a consumer type.
     * 
     * @return see above
     */
    boolean isConsumerType();
    
    /**
     * Returns the set of user-defined types reachable by this type.
     * 
     * @param inclusive Flag whether to include this type itself in the result
     * @return The (possibly empty) set of reachable user-defined types
     */
    default Set<UserDefinedType<A>> getReachableUserDefinedTypes(Inclusive inclusive) {
        Set<UserDefinedType<A>> reachableTypes = new HashSet<>();
        new ReachableTypesCollector<A>().collectReachableTypesOf(this, inclusive, reachableTypes);
        return reachableTypes;
    }

}
