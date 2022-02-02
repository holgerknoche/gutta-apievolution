package gutta.apievolution.core.apimodel.provider;

import gutta.apievolution.core.apimodel.Field;
import gutta.apievolution.core.apimodel.Optionality;
import gutta.apievolution.core.apimodel.Type;

import java.util.Collections;
import java.util.List;
import java.util.Optional;

/**
 * Provider-specific implementation of a {@link Field}.
 */
public class ProviderField extends Field<ProviderRecordType, ProviderField>
        implements RevisionedElement<ProviderField>, ProviderUDTMember {

    private final Optional<ProviderField> predecessor;

    private final List<ProviderField> declaredPredecessors;

    private Optional<ProviderField> successor;

    /**
     * Determines whether the types of two successive fields represent a type change.
     * @param predecessorType The type of the predecessor field
     * @param successorType The type of the successor field
     * @return {@code True}, if the types are considered different, {@code false} otherwise
     */
    public static boolean isTypeChange(Type predecessorType, Type successorType) {
        if (successorType instanceof RevisionedElement) {
            // If the current type is revisioned, we must compare the its predecessor to the predecessor
            // field's type
            Optional<?> optionalOwnTypePredecessor = ((RevisionedElement<?>) successorType).getPredecessor();

            if (optionalOwnTypePredecessor.isPresent()) {
                Type ownTypePredecessor = (Type) optionalOwnTypePredecessor.get();
                return !(ownTypePredecessor.equals(predecessorType));
            } else {
                // If no predecessor is present, we have a type change
                return true;
            }
        } else {
            // Otherwise, the types can be compared immediately
            return !(successorType.equals(predecessorType));
        }
    }

    /**
     * Creates a new field from the given data.
     * @param publicName The field's public name
     * @param internalName The field's internal name, if any. Otherwise, the public name is assumed
     * @param owner The record type that owns this field
     * @param type The field's type
     * @param optionality The field's optionality
     */
    public ProviderField(final String publicName, final Optional<String> internalName, final ProviderRecordType owner,
                         final Type type, Optionality optionality) {
        this(publicName, internalName, owner, type, optionality, false, Collections.emptyList(), Optional.empty());
    }

    /**
     * Creates a new field from the given data.
     * @param publicName The field's public name
     * @param internalName The field's internal name, if any. Otherwise, the public name is assumed
     * @param owner The record type that owns this field
     * @param type The field's type
     * @param optionality The field's optionality
     * @param inherited Denotes whether this field is inherited
     * @param declaredPredecessors The declared predecessors, if any
     * @param predecessor The field's predecessor, if any
     */
    public ProviderField(final String publicName, final Optional<String> internalName, final ProviderRecordType owner,
                         final Type type, Optionality optionality, boolean inherited,
                         List<ProviderField> declaredPredecessors,
                         final Optional<ProviderField> predecessor) {
        super(publicName, internalName, owner, type, optionality, inherited);

        this.predecessor = predecessor;
        this.declaredPredecessors = declaredPredecessors;
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

    /**
     * Returns the predecessors declared on this field.
     * @return see above
     */
    public List<ProviderField> getDeclaredPredecessors() {
        return this.declaredPredecessors;
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
