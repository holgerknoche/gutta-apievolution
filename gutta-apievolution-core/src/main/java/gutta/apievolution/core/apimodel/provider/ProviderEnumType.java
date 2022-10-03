package gutta.apievolution.core.apimodel.provider;

import gutta.apievolution.core.apimodel.EnumType;

import java.util.Optional;

import static gutta.apievolution.core.util.UtilityFunctions.*;
import static java.util.Objects.*;

/**
 * Provider-specific implementation of an {@link EnumType}.
 */
public class ProviderEnumType extends EnumType<ProviderApiDefinition, ProviderEnumType, ProviderEnumMember>
        implements RevisionedElement<ProviderEnumType>, ProviderUserDefinedType {

    private final ProviderEnumType predecessor;

    private ProviderEnumType successor;

    public static ProviderEnumType create(String publicName, int typeId, ProviderApiDefinition owner) {
        return withInternalName(publicName, null, typeId, owner);
    }
        
    public static ProviderEnumType withInternalName(final String publicName, final String internalName, final int typeId,
            final ProviderApiDefinition owner) {
        return new ProviderEnumType(publicName, internalName, typeId, owner, null);
    }
    
    public static ProviderEnumType withPredecessor(final String publicName, final String internalName, final int typeId,
            final ProviderApiDefinition owner, final ProviderEnumType predecessor) {
        return new ProviderEnumType(publicName, internalName, typeId, owner, requireNonNull(predecessor));
    }
    
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
    private ProviderEnumType(final String publicName, final String internalName, final int typeId,
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
