package gutta.apievolution.inprocess;

/**
 * A {@link FieldMapper} encapsulates the mapping of a specific field of a record type.
 */
public interface FieldMapper {

    /**
     * Maps the field represented by this field mapper on the given object and returns the mapped value.
     * 
     * @param object The object to map the field of
     * @return The mapped value of the field
     */
    Object mapField(Object object);

}
