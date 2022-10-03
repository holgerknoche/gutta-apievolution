package gutta.apievolution.core.apimodel.provider;

import static gutta.apievolution.core.apimodel.Conventions.noDeclaredPredecessors;

import gutta.apievolution.core.apimodel.Inherited;
import gutta.apievolution.core.apimodel.InheritedFieldPropagator;

class ProviderInheritedFieldPropagator
        extends InheritedFieldPropagator<ProviderRecordType, ProviderField> {

    protected ProviderField createInheritedField(ProviderField originalField, ProviderRecordType targetType) {
        ProviderField predecessor = null;
        for (ProviderField potentialPredecessor : originalField.getDeclaredPredecessors()) {
            if (targetType.getPredecessor().isPresent() &&
                    targetType.getPredecessor().get().equals(potentialPredecessor.getOwner())) {
                predecessor = potentialPredecessor;
                break;
            }
        }

        return targetType.newField(originalField.getPublicName(), originalField.getInternalName(), originalField.getType(),
                originalField.getOptionality(), Inherited.YES, noDeclaredPredecessors(), predecessor);
    }

}
