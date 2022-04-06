package gutta.apievolution.core.apimodel.provider;

import gutta.apievolution.core.apimodel.EnumType;
import gutta.apievolution.core.apimodel.TypeVisitor;

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
     *
     * @param publicName   The enum type's public name
     * @param internalName The enum type's internal name, if any. Otherwise, the
     *                     public name is assumed
     * @param typeId       The enum type's type id
     * @param owner        The API definition that owns this enum type
     * @param predecessor  The enum type's predecessor, if any
     */
    public ProviderEnumType(final String publicName, final Optional<String> internalName, final int typeId,
            final ProviderApiDefinition owner, final Optional<ProviderEnumType> predecessor) {
        super(publicName, internalName, typeId, owner);

        this.predecessor = predecessor;
        this.successor = Optional.empty();

        predecessor.ifPresent(enumType -> enumType.setSuccessor(this));
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

    @Override
    public <R> R accept(TypeVisitor<R> visitor) {
        return visitor.handleEnumType(this);
    }

    @Override
    public int hashCode() {
        // No predecessors or successors to avoid cycles
        return super.hashCode();
    }

    @Override
    public boolean equals(Object that) {
        if (this == that) {
            return true;
        } else if (that instanceof ProviderEnumType) {
            return this.stateEquals((ProviderEnumType) that);
        } else {
            return true;
        }
    }

    boolean stateEquals(ProviderEnumType that) {
        // No predecessors or successors to avoid cycles
        return super.stateEquals(that);
    }

}
