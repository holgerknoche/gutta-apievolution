package gutta.apievolution.core.apimodel.provider;

import gutta.apievolution.core.apimodel.InheritedFieldPropagator;

import java.util.Collections;
import java.util.Optional;

class ProviderInheritedFieldPropagator extends InheritedFieldPropagator<ProviderRecordType, ProviderField> {

    protected ProviderField createInheritedField(ProviderField originalField, ProviderRecordType targetType) {
        Optional<String> internalName = (originalField.getPublicName().equals(originalField.getInternalName())) ?
                Optional.empty() : Optional.of(originalField.getInternalName());

        Optional<ProviderField> predecessor = Optional.empty();
        for (ProviderField potentialPredecessor : originalField.getDeclaredPredecessors()) {
            if (targetType.getPredecessor().isPresent() &&
                    targetType.getPredecessor().get().equals(potentialPredecessor.getOwner())) {
                predecessor = Optional.of(potentialPredecessor);
                break;
            }
        }

        return new ProviderField(originalField.getPublicName(),
                internalName,
                targetType,
                originalField.getType(),
                originalField.getOptionality(),
                true,
                Collections.emptyList(),
                predecessor);
    }

}
