package gutta.apievolution.core.apimodel.provider;

import static gutta.apievolution.core.apimodel.Conventions.*;
import static gutta.apievolution.core.util.UtilityFunctions.ifPresent;

import gutta.apievolution.core.apimodel.EnumType;

import java.util.Optional;

/**
 * Provider-specific implementation of an {@link EnumType}.
 */
public class ProviderEnumType extends EnumType<ProviderApiDefinition, ProviderEnumType, ProviderEnumMember>
        implements RevisionedElement<ProviderEnumType>, ProviderUserDefinedType {

    private final ProviderEnumType predecessor;

    private ProviderEnumType successor;
    
    /**
     * Creates a new enum type from the given data.
     *
     * @param publicName   The enum type's public name
     * @param internalName The enum type's internal name, if any. If {@code null}, the
     *                     public name is assumed
     * @param typeId       The enum type's type id
     * @param owner        The API definition that owns this enum type
     * @param predecessor  The enum type's predecessor
     */
    ProviderEnumType(final String publicName, final String internalName, final int typeId,
            final ProviderApiDefinition owner, final ProviderEnumType predecessor) {
        super(publicName, internalName, typeId, owner);

        this.predecessor = predecessor;
        this.successor = null;

        ifPresent(predecessor, enumType -> enumType.setSuccessor(this));
    }

    @Override
    public Optional<ProviderEnumType> getPredecessor() {
        return Optional.ofNullable(this.predecessor);
    }

    @Override
    public Optional<ProviderEnumType> getSuccessor() {
        return Optional.ofNullable(this.successor);
    }

    private void setSuccessor(final ProviderEnumType successor) {
        this.successor = successor;
    }

    @Override
    public <R> R accept(ProviderApiDefinitionElementVisitor<R> visitor) {
        return visitor.handleProviderEnumType(this);
    }

    // Element creators
    
    public ProviderEnumMember newEnumMember(String publicName) {
        return new ProviderEnumMember(publicName, noInternalName(), this, noPredecessor());
    }
    
    public ProviderEnumMember newEnumMember(String publicName, String internalName, ProviderEnumMember predecessor) {
        return new ProviderEnumMember(publicName, internalName, this, predecessor);
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
