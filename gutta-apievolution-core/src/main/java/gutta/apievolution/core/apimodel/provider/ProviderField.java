package gutta.apievolution.core.apimodel.provider;

import gutta.apievolution.core.apimodel.Field;
import gutta.apievolution.core.apimodel.Optionality;
import gutta.apievolution.core.apimodel.Type;

import java.util.Optional;

/**
 * Provider-specific implementation of a {@link Field}.
 */
public class ProviderField extends Field<ProviderRecordType, ProviderField>
        implements RevisionedElement<ProviderField>, ProviderUDTMember {

    private final Optional<ProviderField> predecessor;

    private Optional<ProviderField> successor;

    /**
     * Creates a new field from the given data.
     * @param publicName The field's public name
     * @param internalName The field's internal name, if any. Otherwise, the public name is assumed
     * @param owner The record type that owns this field
     * @param type The field's type
     * @param optionality The field's optionality
     * @param inherited Denotes whether this field is inherited
     * @param predecessor The field's predecessor, if any
     */
    public ProviderField(final String publicName, final Optional<String> internalName, final ProviderRecordType owner,
                         final Type type, Optionality optionality, boolean inherited,
                         final Optional<ProviderField> predecessor) {
        super(publicName, internalName, owner, type, optionality, inherited);

        this.predecessor = predecessor;
        this.successor = Optional.empty();

        predecessor.ifPresent(field -> field.setSuccessor(this));
    }

    @Override
    public Optional<ProviderField> getPredecessor() {
        return this.predecessor;
    }

    @Override
    public Optional<ProviderField> getSuccessor() {
        return this.successor;
    }

    private void setSuccessor(final ProviderField successor) {
        this.successor = Optional.of(successor);
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
