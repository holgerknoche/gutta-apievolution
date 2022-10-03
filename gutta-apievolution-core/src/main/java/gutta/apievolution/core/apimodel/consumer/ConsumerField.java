package gutta.apievolution.core.apimodel.consumer;

import static java.util.Objects.requireNonNull;

import gutta.apievolution.core.apimodel.Field;
import gutta.apievolution.core.apimodel.Optionality;
import gutta.apievolution.core.apimodel.Type;

/**
 * Consumer-specific implementations of a {@link Field}.
 */
public class ConsumerField extends Field<ConsumerRecordType, ConsumerField> implements ConsumerApiDefinitionElement {

    /**
     * Creates a new field where the public name is equal to the internal name.
     * 
     * @param publicName The field's public name
     * @param owner The record type that owns this field
     * @param type The type of this field
     * @param optionality The field's optionality
     * @return The created field
     */
    public static ConsumerField create(String publicName, ConsumerRecordType owner, Type type, Optionality optionality) {
        return new ConsumerField(publicName, null, owner, type, optionality, false);
    }
    
    /**
     * Creates a new field with an explicit internal name.
     *
     * @param publicName   The field's public name
     * @param internalName The field's internal name. Must not be {@code null}
     * @param owner        The record type that owns this field
     * @param type         The type of this field
     * @param optionality  The field's optionality
     */
    public static ConsumerField withInternalName(final String publicName, final String internalName, final ConsumerRecordType owner,
            final Type type, Optionality optionality) {
        return new ConsumerField(publicName, requireNonNull(internalName), owner, type, optionality, false);
    }

    /**
     * Creates a new field from the given data.
     *
     * @param publicName   The field's public name
     * @param internalName The field's internal name, if any. If {@code null} the public
     *                     name is assumed
     * @param owner        The record type that owns this field
     * @param type         The type of this field
     * @param optionality  The field's optionality
     * @param inherited    Denotes whether this field is inherited
     */
    public ConsumerField(final String publicName, final String internalName, final ConsumerRecordType owner,
            final Type type, Optionality optionality, boolean inherited) {
        super(publicName, internalName, owner, type, optionality, inherited);
    }

    @Override
    public int hashCode() {
        return super.hashCode();
    }

    @Override
    public boolean equals(Object that) {
        if (this == that) {
            return true;
        } else if (that instanceof ConsumerField) {
            return this.stateEquals((ConsumerField) that);
        } else {
            return false;
        }
    }

    boolean stateEquals(ConsumerField that) {
        return super.stateEquals(that);
    }

    @Override
    public <R> R accept(ConsumerApiDefinitionElementVisitor<R> visitor) {
        return visitor.handleConsumerField(this);
    }

}
