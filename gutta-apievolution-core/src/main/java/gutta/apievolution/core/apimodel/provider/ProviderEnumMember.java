package gutta.apievolution.core.apimodel.provider;

import gutta.apievolution.core.apimodel.EnumMember;

import java.util.Optional;

import static gutta.apievolution.core.util.UtilityFunctions.*;
import static java.util.Objects.*;

/**
 * Provider-specific implementation of an {@link EnumMember}.
 */
public class ProviderEnumMember extends EnumMember<ProviderEnumType, ProviderEnumMember>
        implements RevisionedElement<ProviderEnumMember>, ProviderApiDefinitionElement {

    private final ProviderEnumMember predecessor;

    private ProviderEnumMember successor;

    public static ProviderEnumMember create(String publicName, ProviderEnumType owner) {
        return withInternalName(publicName, null, owner);
    }
    
    public static ProviderEnumMember withInternalName(String publicName, String internalName,
            ProviderEnumType owner) {
        return new ProviderEnumMember(publicName, internalName, owner, null);
    }
    
    public static ProviderEnumMember withPredecessor(String publicName, String internalName,
            ProviderEnumType owner, ProviderEnumMember predecessor) {
        return new ProviderEnumMember(publicName, internalName, owner, requireNonNull(predecessor));
    }
    
    /**
     * Creates a new enum member from the given data.
     *
     * @param publicName   The enum member's public name
     * @param internalName The enum member's internal name, if applicable.
     *                     Otherwise, the public name is assumed
     * @param owner        The enum type that owns this member
     * @param predecessor  The member's predecessor, if any
     */
    private ProviderEnumMember(final String publicName, final String internalName,
            final ProviderEnumType owner, final ProviderEnumMember predecessor) {
        super(publicName, internalName, owner);

        this.predecessor = predecessor;
        this.successor = null;

        ifPresent(predecessor, enumMember -> enumMember.setSuccessor(this));
    }

    @Override
    public Optional<ProviderEnumMember> getPredecessor() {
        return Optional.ofNullable(this.predecessor);
    }

    @Override
    public Optional<ProviderEnumMember> getSuccessor() {
        return Optional.ofNullable(this.successor);
    }

    private void setSuccessor(final ProviderEnumMember successor) {
        this.successor = successor;
    }

    @Override
    public <R> R accept(ProviderApiDefinitionElementVisitor<R> visitor) {
        return visitor.handleProviderEnumMember(this);
    }

    @Override
    public int hashCode() {
        // No successors and predecessors as to avoid cycles
        return super.hashCode();
    }

    @Override
    public boolean equals(Object that) {
        if (this == that) {
            return true;
        } else if (that instanceof ProviderEnumMember) {
            return this.stateEquals((ProviderEnumMember) that);
        } else {
            return false;
        }
    }

    boolean stateEquals(ProviderEnumMember that) {
        // No successors and predecessors as to avoid cycles
        return super.stateEquals(that);
    }

}
