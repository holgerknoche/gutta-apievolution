package gutta.apievolution.core.apimodel.provider;

import gutta.apievolution.core.apimodel.EnumType;

import java.util.Optional;

/**
 * Provider-specific implementation of an {@link EnumType}.
 */
public class ProviderEnumType extends EnumType<ProviderApiDefinition, ProviderEnumType, ProviderEnumMember>
        implements RevisionedElement<ProviderEnumType>, ProviderUserDefinedType {

    private final Optional<ProviderEnumType> predecessor;

    private Optional<ProviderEnumType> successor;

    /**
     * Creates a new enum type from the given data.
     * @param publicName The enum type's public name
     * @param internalName The enum type's internal name, if any. Otherwise, the public name is assumed
     * @param typeId The enum type's type id
     * @param owner The API definition that owns this enum type
     * @param predecessor The enum type's predecessor, if any
     */
    public ProviderEnumType(final String publicName, final Optional<String> internalName, final int typeId,
                            final ProviderApiDefinition owner, final Optional<ProviderEnumType> predecessor) {
        super(publicName, internalName, typeId, owner);

        this.predecessor = predecessor;
        this.successor = Optional.empty();

        if (predecessor.isPresent()) {
            predecessor.get().setSuccessor(this);
        }
    }

    @Override
    public Optional<ProviderEnumType> getPredecessor() {
        return this.predecessor;
    }

    @Override
    public Optional<ProviderEnumType> getSuccessor() {
        return this.successor;
    }

    private void setSuccessor(final ProviderEnumType successor) {
        this.successor = Optional.of(successor);
    }

    @Override
    public <R> R accept(ProviderApiDefinitionElementVisitor<R> visitor) {
        return visitor.handleProviderEnumType(this);
    }

}
