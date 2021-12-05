package gutta.apievolution.core.apimodel.consumer;

import gutta.apievolution.core.apimodel.Field;
import gutta.apievolution.core.apimodel.Optionality;
import gutta.apievolution.core.apimodel.Type;

import java.util.Optional;

/**
 * Consumer-specific implementations of a {@link Field}.
 */
public class ConsumerField extends Field<ConsumerRecordType, ConsumerField> {

    /**
     * Creates a new field from the given data.
     * @param publicName The field's public name
     * @param internalName The field's internal name, if any. Otherwise, the public name is assumed
     * @param owner The record type that owns this field
     * @param type The type of this field
     * @param optionality The field's optionality
     */
    public ConsumerField(final String publicName, final Optional<String> internalName, final ConsumerRecordType owner,
                         final Type type, Optionality optionality) {
        super(publicName, internalName, owner, type, optionality);
    }

}
