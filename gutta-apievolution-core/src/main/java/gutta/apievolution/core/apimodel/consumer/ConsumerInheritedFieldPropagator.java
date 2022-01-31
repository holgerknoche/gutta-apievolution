package gutta.apievolution.core.apimodel.consumer;

import gutta.apievolution.core.apimodel.InheritedFieldPropagator;

import java.util.Optional;

class ConsumerInheritedFieldPropagator extends InheritedFieldPropagator<ConsumerRecordType, ConsumerField> {

    @Override
    protected ConsumerField createInheritedField(ConsumerField originalField, ConsumerRecordType targetType) {
        Optional<String> internalName = (originalField.getPublicName().equals(originalField.getInternalName())) ?
                Optional.empty() : Optional.of(originalField.getInternalName());

        return new ConsumerField(originalField.getPublicName(),
                internalName,
                targetType,
                originalField.getType(),
                originalField.getOptionality(),
                true);
    }

}
