package gutta.apievolution.core.apimodel.provider;

import gutta.apievolution.core.apimodel.InheritedFieldPropagator;
import gutta.apievolution.core.apimodel.RecordType;

import java.util.*;

class ProviderInheritedFieldPropagator extends InheritedFieldPropagator<ProviderRecordType, ProviderField> {

    private Set<ProviderRecordType> processedTypes;

    public void propagateFieldsFor(Collection<ProviderRecordType> recordTypes) {
        this.processedTypes = new HashSet<>();

        // Iterate over "leaf types" with super types
        recordTypes.stream()
                .filter(type -> !type.hasSubTypes())
                .filter(RecordType::hasSuperType)
                .forEach(this::propagateFields);
    }

    private void propagateFields(ProviderRecordType recordType) {
        this.propagateFields(recordType, new ArrayList<>());
    }

    private void propagateFields(ProviderRecordType targetType, List<ProviderField> inheritedFields) {
        // Collect fields from the supertype, if present
        Optional<ProviderRecordType> optionalSuperType = targetType.getSuperType();
        optionalSuperType.ifPresent(superType -> {
            this.propagateFields(superType, inheritedFields);
        });

        // If the current type has not yet been processed, add the inherited types to the
        // current type
        if (!this.processedTypes.contains(targetType)) {
            for (ProviderField inheritedField : inheritedFields) {
                this.createInheritedField(inheritedField, targetType);
            }
            this.processedTypes.add(targetType);
        }

        // Add the current type's declared field to the inherited types
        inheritedFields.addAll(targetType.getDeclaredFields());
    }

    private void createInheritedField(ProviderField originalField, ProviderRecordType targetType) {
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

        new ProviderField(originalField.getPublicName(),
                internalName,
                targetType,
                originalField.getType(),
                originalField.getOptionality(),
                true,
                Collections.emptyList(),
                predecessor);
    }

}
