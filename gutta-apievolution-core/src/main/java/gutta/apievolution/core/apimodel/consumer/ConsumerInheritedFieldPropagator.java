package gutta.apievolution.core.apimodel.consumer;

import gutta.apievolution.core.apimodel.Inherited;
import gutta.apievolution.core.apimodel.InheritedFieldPropagator;

class ConsumerInheritedFieldPropagator extends InheritedFieldPropagator<ConsumerRecordType, ConsumerField> {

    @Override
    protected ConsumerField createInheritedField(ConsumerField originalField, ConsumerRecordType targetType) {
        return new ConsumerField(originalField.getPublicName(), originalField.getInternalName(), targetType,
                originalField.getType(), originalField.getOptionality(), Inherited.YES);
    }

}
