package gutta.apievolution.core.apimodel.provider;

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

        return ProviderField.inheritedField(originalField.getPublicName(), originalField.getInternalName(), targetType, originalField.getType(),
                originalField.getOptionality(), predecessor);
    }

}
