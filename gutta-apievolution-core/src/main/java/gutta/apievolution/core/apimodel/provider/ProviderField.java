package gutta.apievolution.core.apimodel.provider;

import gutta.apievolution.core.apimodel.Field;
import gutta.apievolution.core.apimodel.Inherited;
import gutta.apievolution.core.apimodel.Optionality;
import gutta.apievolution.core.apimodel.Type;

import java.util.List;
import java.util.Optional;

import static gutta.apievolution.core.util.UtilityFunctions.ifPresent;

/**
 * Provider-specific implementation of a {@link Field}.
 */
public class ProviderField extends Field<ProviderRecordType, ProviderField>
        implements RevisionedElement<ProviderField>, ProviderUDTMember {

    private final ProviderField predecessor;

    private final List<ProviderField> declaredPredecessors;

    private ProviderField successor;
    
    /**
     * Creates a new field from the given data.
     *
     * @param publicName           The field's public name
     * @param internalName         The field's internal name, if any. If {@code null}, the
     *                             public name is assumed
     * @param owner                The record type that owns this field
     * @param type                 The field's type
     * @param optionality          The field's optionality
     * @param inherited            Denotes whether this field is inherited
     * @param declaredPredecessors The declared predecessors, if any
     * @param predecessor          The field's predecessor, if any
     */
    ProviderField(final String publicName, final String internalName, final ProviderRecordType owner,
            final Type type, Optionality optionality, Inherited inherited, List<ProviderField> declaredPredecessors,
            final ProviderField predecessor) {
        super(publicName, internalName, owner, type, optionality, inherited);

        this.predecessor = predecessor;
        this.declaredPredecessors = declaredPredecessors;
        this.successor = null;

        ifPresent(predecessor, field -> field.setSuccessor(this));
    }

    @Override
    public Optional<ProviderField> getPredecessor() {
        return Optional.ofNullable(this.predecessor);
    }

    @Override
    public Optional<ProviderField> getSuccessor() {
        return Optional.ofNullable(this.successor);
    }

    /**
     * Returns the predecessors declared on this field.
     *
     * @return see above
     */
    public List<ProviderField> getDeclaredPredecessors() {
        return this.declaredPredecessors;
    }

    private void setSuccessor(final ProviderField successor) {
        this.successor = successor;
    }

    @Override
    public <R> R accept(ProviderApiDefinitionElementVisitor<R> visitor) {
        return visitor.handleProviderField(this);
    }

    @Override
    public String toString() {
        return this.getInternalName() + "@" + this.getOwner().toString();
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
        } else if (that instanceof ProviderField) {
            return this.stateEquals((ProviderField) that);
        } else {
            return false;
        }
    }

    boolean stateEquals(ProviderField that) {
        // No predecessors or successors to avoid cycles
        return super.stateEquals(that);
    }

}
