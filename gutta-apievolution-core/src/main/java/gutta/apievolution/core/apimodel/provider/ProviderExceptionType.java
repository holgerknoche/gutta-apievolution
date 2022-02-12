package gutta.apievolution.core.apimodel.provider;

import gutta.apievolution.core.apimodel.ExceptionType;

import java.util.Optional;

/**
 * Provider-specific implementation of an exception type.
 */
public class ProviderExceptionType extends ProviderRecordType implements ExceptionType {

    /**
     * Creates a new exception type from the given data.
     * @param publicName The exception type's public name
     * @param internalName The exception type's internal name, if any. Otherwise, the public name is assumed
     * @param typeId The exception type's type id
     * @param owner The API definition that owns this exception type
     * @param abstractFlag Denotes whether this type is abstract
     * @param predecessor The type's predecessor, if any
     */
    public ProviderExceptionType(String publicName, Optional<String> internalName, int typeId,
                                 ProviderApiDefinition owner, boolean abstractFlag,
                                 Optional<ProviderRecordType> predecessor) {
        super(publicName, internalName, typeId, owner, abstractFlag, predecessor);
    }

    /**
     * Creates a new exception type from the given data.
     * @param publicName The exception type's public name
     * @param internalName The exception type's internal name, if any. Otherwise, the public name is assumed
     * @param typeId The exception type's type id
     * @param owner The API definition that owns this exception type
     * @param abstractFlag Denotes whether this type is abstract
     * @param superType This type's supertype, if any
     * @param predecessor The type's predecessor, if any
     */
    public ProviderExceptionType(String publicName, Optional<String> internalName, int typeId,
                                 ProviderApiDefinition owner, boolean abstractFlag,
                                 Optional<ProviderRecordType> superType, Optional<ProviderRecordType> predecessor) {
        super(publicName, internalName, typeId, owner, abstractFlag, superType, predecessor);
    }

    @Override
    public <R> R accept(ProviderApiDefinitionElementVisitor<R> visitor) {
        return visitor.handleProviderExceptionType(this);
    }

}
