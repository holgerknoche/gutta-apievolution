package gutta.apievolution.core.apimodel.consumer;

import static java.util.Objects.requireNonNull;

import gutta.apievolution.core.apimodel.RecordType;

import java.util.Collections;
import java.util.Set;

/**
 * Consumer-specific implementation of a {@link RecordType}.
 */
public class ConsumerRecordType extends RecordType<ConsumerApiDefinition, ConsumerRecordType, ConsumerField>
        implements ConsumerUserDefinedType {

    /**
     * Creates a minimal record type where the public name is equal to the internal name.
     *
     * @param publicName   The type's public name
     * @param typeId       The type's type id
     * @param owner        The API definition that owns this type
     * @return The created type
     */
    public static ConsumerRecordType createRecordType(final String publicName, final int typeId, final ConsumerApiDefinition owner) {
        return recordType(publicName, null, typeId, owner, false, Collections.emptySet());
    }
    
    /**
     * Creates a new record type from the given data.
     *
     * @param publicName   The type's public name
     * @param internalName The type's internal name. Must not be {@code null}.
     * @param typeId       The type's type id
     * @param owner        The API definition that owns this type
     * @param abstractFlag Denotes whether this type is abstract
     */
    public static ConsumerRecordType withInternalName(final String publicName, final String internalName, final int typeId,
            final ConsumerApiDefinition owner) {
        return recordType(publicName, requireNonNull(internalName), typeId, owner, false, Collections.emptySet());
    }

    public static ConsumerRecordType recordType(String publicName, String internalName, int typeId, ConsumerApiDefinition owner, boolean abstractFlag,
            Set<ConsumerRecordType> superTypes) {
        return new ConsumerRecordType(publicName, internalName, typeId, owner, false, false, superTypes);
    }

    public static ConsumerRecordType minimalExceptionType(String publicName, int typeId, ConsumerApiDefinition owner) {
        return exceptionType(publicName, null, typeId, owner, false, Collections.emptySet());
    }
    
    public static ConsumerRecordType exceptionType(String publicName, String internalName, int typeId, ConsumerApiDefinition owner, boolean abstractFlag,
            Set<ConsumerRecordType> superTypes) {
        return new ConsumerRecordType(publicName, internalName, typeId, owner, false, true, superTypes);
    }

    /**
     * Creates a new record type from the given data.
     *
     * @param publicName   The type's public name
     * @param internalName The type's internal name, if any. Otherwise, the public
     *                     name is assumed
     * @param typeId       The type's type id
     * @param owner        The API definition that owns this type
     * @param abstractFlag Denotes whether this type is abstract
     * @param exception    Denotes whether this type is an exception
     * @param superTypes   The type's supertypes, if any
     */
    private ConsumerRecordType(final String publicName, final String internalName, final int typeId,
            final ConsumerApiDefinition owner, final boolean abstractFlag, boolean exception,
            final Set<ConsumerRecordType> superTypes) {
        super(publicName, internalName, typeId, owner, abstractFlag, exception, superTypes);
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
    public <R> R accept(ConsumerApiDefinitionElementVisitor<R> visitor) {
        return visitor.handleConsumerRecordType(this);
    }

}
