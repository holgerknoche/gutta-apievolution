package gutta.apievolution.core.apimodel.provider;

import gutta.apievolution.core.apimodel.RecordType;
import gutta.apievolution.core.apimodel.TypeVisitor;

import java.util.Optional;

/**
 * Provider-specific implementation of a {@link RecordType}.
 */
public class ProviderRecordType extends RecordType<ProviderApiDefinition, ProviderRecordType, ProviderField>
        implements RevisionedElement<ProviderRecordType>, ProviderUserDefinedType {

    private final Optional<ProviderRecordType> predecessor;

    private Optional<ProviderRecordType> successor;

    /**
     * Creates a new record type from the given data.
     * @param publicName The record type's public name
     * @param internalName The record type's internal name, if any. Otherwise, the public name is assumed
     * @param typeId The record type's type id
     * @param owner The API definition that owns this record type
     * @param abstractFlag Denotes whether this type is abstract
     * @param predecessor The type's predecessor, if any
     */
    public ProviderRecordType(final String publicName, final Optional<String> internalName, final int typeId,
                              final ProviderApiDefinition owner, final boolean abstractFlag,
                              final Optional<ProviderRecordType> predecessor) {
        this(publicName, internalName, typeId, owner, abstractFlag, Optional.empty(), predecessor);
    }

    /**
     * Creates a new record type from the given data.
     * @param publicName The record type's public name
     * @param internalName The record type's internal name, if any. Otherwise, the public name is assumed
     * @param typeId The record type's type id
     * @param owner The API definition that owns this record type
     * @param abstractFlag Denotes whether this type is abstract
     * @param superType This type's supertype, if any
     * @param predecessor The type's predecessor, if any
     */
    public ProviderRecordType(final String publicName, final Optional<String> internalName, final int typeId,
                              final ProviderApiDefinition owner, final boolean abstractFlag,
                              final Optional<ProviderRecordType> superType,
                              final Optional<ProviderRecordType> predecessor) {
        super(publicName, internalName, typeId, owner, abstractFlag, superType);

        this.predecessor = predecessor;
        this.successor = Optional.empty();

        predecessor.ifPresent(recordType -> recordType.setSuccessor(this));
    }

    @Override
    public Optional<ProviderRecordType> getPredecessor() {
        return this.predecessor;
    }

    @Override
    public Optional<ProviderRecordType> getSuccessor() {
        return this.successor;
    }

    private void setSuccessor(final ProviderRecordType successor) {
        this.successor = Optional.of(successor);
    }

    @Override
    public <R> R accept(ProviderApiDefinitionElementVisitor<R> visitor) {
        return visitor.handleProviderRecordType(this);
    }

    @Override
    public <R> R accept(TypeVisitor<R> visitor) {
        return visitor.handleRecordType(this);
    }

    @Override
    public String toString() {
        return this.getInternalName() + "@" + this.getOwner().toString();
    }

    @Override
    public int hashCode() {
        // No predecessor / successor to avoid cycles
        return super.hashCode();
    }

    @Override
    public boolean equals(Object that) {
        if (this == that) {
            return true;
        } else if (that instanceof ProviderRecordType) {
            return this.stateEquals((ProviderRecordType) that);
        } else {
            return false;
        }
    }

    boolean stateEquals(ProviderRecordType that) {
        // No predecessor / successor to avoid cycles
        return super.stateEquals(that);
    }

}
