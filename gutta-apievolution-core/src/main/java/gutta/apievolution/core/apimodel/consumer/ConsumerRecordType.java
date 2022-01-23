package gutta.apievolution.core.apimodel.consumer;

import gutta.apievolution.core.apimodel.RecordType;
import gutta.apievolution.core.apimodel.TypeVisitor;

import java.util.Optional;

/**
 * Consumer-specific implementation of a {@link RecordType}.
 */
public class ConsumerRecordType extends RecordType<ConsumerApiDefinition, ConsumerRecordType, ConsumerField>
        implements ConsumerUserDefinedType {

    /**
     * Creates a new record type from the given data.
     * @param publicName The type's public name
     * @param internalName The type's internal name, if any. Otherwise, the public name is assumed
     * @param typeId The type's type id
     * @param owner The API definition that owns this type
     * @param abstractFlag Denotes whether this type is abstract
     */
    public ConsumerRecordType(final String publicName, final Optional<String> internalName, final int typeId,
                              final ConsumerApiDefinition owner, final boolean abstractFlag) {
        super(publicName, internalName, typeId, owner, abstractFlag, Optional.empty());
    }

    /**
     * Creates a new record type from the given data.
     * @param publicName The type's public name
     * @param internalName The type's internal name, if any. Otherwise, the public name is assumed
     * @param typeId The type's type id
     * @param owner The API definition that owns this type
     * @param abstractFlag Denotes whether this type is abstract
     * @param superType The type's supertype, if any
     */
    public ConsumerRecordType(final String publicName, final Optional<String> internalName, final int typeId,
                              final ConsumerApiDefinition owner, final boolean abstractFlag,
                              final Optional<ConsumerRecordType> superType) {
        super(publicName, internalName, typeId, owner, abstractFlag, superType);
    }

    @Override
    public int hashCode() {
        return super.hashCode();
    }

    @Override
    public boolean equals(Object that) {
        if (this == that) {
            return true;
        } else if (that instanceof ConsumerRecordType) {
            return this.stateEquals((ConsumerRecordType) that);
        } else {
            return false;
        }
    }

    boolean stateEquals(ConsumerRecordType that) {
        return super.stateEquals(that);
    }

    @Override
    public <R> R accept(TypeVisitor<R> visitor) {
        return visitor.handleRecordType(this);
    }

    @Override
    public <R> R accept(ConsumerApiDefinitionElementVisitor<R> visitor) {
        return visitor.handleConsumerRecordType(this);
    }

}
