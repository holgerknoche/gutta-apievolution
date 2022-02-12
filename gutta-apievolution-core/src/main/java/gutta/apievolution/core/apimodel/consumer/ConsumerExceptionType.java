package gutta.apievolution.core.apimodel.consumer;

import gutta.apievolution.core.apimodel.ExceptionType;

import java.util.Optional;

/**
 * Consumer-specific implementation of an exception type.
 */
public class ConsumerExceptionType extends ConsumerRecordType implements ExceptionType {

    /**
     * Creates a new exception type from the given data.
     * @param publicName   The type's public name
     * @param internalName The type's internal name, if any. Otherwise, the public name is assumed
     * @param typeId       The type's type id
     * @param owner        The API definition that owns this type
     * @param abstractFlag Denotes whether this type is abstract
     */
    public ConsumerExceptionType(String publicName, Optional<String> internalName, int typeId,
                                 ConsumerApiDefinition owner, boolean abstractFlag) {
        super(publicName, internalName, typeId, owner, abstractFlag);
    }

    /**
     * Creates a new exception type from the given data.
     * @param publicName   The type's public name
     * @param internalName The type's internal name, if any. Otherwise, the public name is assumed
     * @param typeId       The type's type id
     * @param owner        The API definition that owns this type
     * @param abstractFlag Denotes whether this type is abstract
     * @param superType    The type's supertype, if any
     */
    public ConsumerExceptionType(String publicName, Optional<String> internalName, int typeId,
                                 ConsumerApiDefinition owner, boolean abstractFlag,
                                 Optional<ConsumerRecordType> superType) {
        super(publicName, internalName, typeId, owner, abstractFlag, superType);
    }

}
