package gutta.apievolution.core.apimodel.consumer;

import gutta.apievolution.core.apimodel.Field;
import gutta.apievolution.core.apimodel.Inherited;
import gutta.apievolution.core.apimodel.Optionality;
import gutta.apievolution.core.apimodel.Type;
import gutta.apievolution.core.util.EqualityUtil;

/**
 * Consumer-specific implementations of a {@link Field}.
 */
public class ConsumerField extends Field<ConsumerRecordType, ConsumerField> implements ConsumerApiDefinitionElement {

    /**
     * Creates a new field from the given data.
     *
     * @param publicName   The field's public name
     * @param internalName The field's internal name, if any. If {@code null} the public name is assumed
     * @param owner        The record type that owns this field
     * @param type         The type of this field
     * @param optionality  The field's optionality
     * @param inherited    Denotes whether this field is inherited
     */
    ConsumerField(final String publicName, final String internalName, final ConsumerRecordType owner, final Type type, Optionality optionality,
            Inherited inherited) {
        super(publicName, internalName, owner, type, optionality, inherited);
    }

    @Override
    public int hashCode() {
        return super.hashCode();
    }

    @Override
    public boolean equals(Object that) {
        return EqualityUtil.equals(this, that, this::stateEquals);
    }

    @Override
    protected boolean stateEquals(ConsumerField that) {
        return super.stateEquals(that);
    }

    @Override
    public <R> R accept(ConsumerApiDefinitionElementVisitor<R> visitor) {
        return visitor.handleConsumerField(this);
    }

}
