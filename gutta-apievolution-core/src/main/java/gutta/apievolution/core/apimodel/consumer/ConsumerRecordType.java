package gutta.apievolution.core.apimodel.consumer;

import gutta.apievolution.core.apimodel.Abstract;
import gutta.apievolution.core.apimodel.Inherited;
import gutta.apievolution.core.apimodel.Optionality;
import gutta.apievolution.core.apimodel.RecordKind;
import gutta.apievolution.core.apimodel.RecordType;
import gutta.apievolution.core.apimodel.Type;
import gutta.apievolution.core.util.EqualityUtil;

import java.util.Set;

import static gutta.apievolution.core.apimodel.Conventions.noInternalName;

/**
 * Consumer-specific implementation of a {@link RecordType}.
 */
public class ConsumerRecordType extends RecordType<ConsumerApiDefinition, ConsumerRecordType, ConsumerField> implements ConsumerUserDefinedType {

    /**
     * Creates a new record type from the given data.
     *
     * @param publicName   The type's public name
     * @param internalName The type's internal name, if any. Otherwise, the public name is assumed
     * @param typeId       The type's type id
     * @param owner        The API definition that owns this type
     * @param abstractFlag Denotes whether this type is abstract
     * @param exception    Denotes whether this type is an exception
     * @param superTypes   The type's supertypes, if any
     */
    ConsumerRecordType(final String publicName, final String internalName, final int typeId, final ConsumerApiDefinition owner, final Abstract abstractFlag,
            RecordKind recordKind, final Set<ConsumerRecordType> superTypes) {
        super(publicName, internalName, typeId, owner, abstractFlag, recordKind, superTypes);
    }

    // Element creators

    /**
     * Creates a new field in this record type.
     * 
     * @param publicName  The field's public name
     * @param type        The field's type
     * @param optionality The field's optionality
     * @return The created field
     */
    public ConsumerField newField(String publicName, Type type, Optionality optionality) {
        return new ConsumerField(publicName, noInternalName(), this, type, optionality, Inherited.NO);
    }

    /**
     * Creates a new field in this record type.
     * 
     * @param publicName   The field's public type
     * @param internalName The field's internal type
     * @param type         The field's type
     * @param optionality  The field's optionality
     * @param inherited    Denotes whether this field is inherited
     * @return The created field
     */
    public ConsumerField newField(String publicName, String internalName, Type type, Optionality optionality, Inherited inherited) {
        return new ConsumerField(publicName, internalName, this, type, optionality, inherited);
    }

    @Override
    public int hashCode() {
        return super.hashCode();
    }

    @Override
    public boolean equals(Object that) {
        return EqualityUtil.equals(this, that, this::stateEquals);
    }

    boolean stateEquals(ConsumerRecordType that) {
        return super.stateEquals(that);
    }

    @Override
    public <R> R accept(ConsumerApiDefinitionElementVisitor<R> visitor) {
        return visitor.handleConsumerRecordType(this);
    }

}
