package gutta.apievolution.core.apimodel;

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

}
