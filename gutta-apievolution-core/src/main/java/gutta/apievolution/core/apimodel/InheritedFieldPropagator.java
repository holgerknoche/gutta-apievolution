package gutta.apievolution.core.apimodel;

import java.util.ArrayList;
import java.util.Collection;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

/**
 * Abstract supertype for the provider and consumer inherited field propagation.
 *
 * @param <R> The concrete record type to use
 * @param <F> The concrete field type to use
 */
public abstract class InheritedFieldPropagator<R extends RecordType<?, R, F>, F extends Field<R, F>> {

    private Set<R> processedTypes;

    /**
     * Propagates inherited fields for the given record types.
     *
     * @param recordTypes The record types to operate on
     */
    public void propagateFieldsFor(Collection<R> recordTypes) {
        this.processedTypes = new HashSet<>();

        // Iterate over "leaf types" with super types
        recordTypes.stream().filter(type -> !type.hasSubTypes()).filter(RecordType::hasSuperTypes)
                .forEach(this::propagateFields);
    }

    private List<F> propagateFields(R recordType) {
        // Collect fields from the supertypes
        Set<R> superTypes = recordType.getSuperTypes();
        List<F> inheritedFields = new ArrayList<>();
        
        for (R superType : superTypes) {
            List<F> newFields = this.propagateFields(superType);
            inheritedFields.addAll(newFields);
        }

        // If the current type has not yet been processed, add the inherited fields to
        // the current type
        if (!this.processedTypes.contains(recordType)) {
            for (F inheritedField : inheritedFields) {                
                this.createInheritedField(inheritedField, recordType);
            }
            this.processedTypes.add(recordType);
        }

        // Add the current type's declared field to the inherited types
        inheritedFields.addAll(recordType.getDeclaredFields());
        
        return inheritedFields;
    }

    /**
     * Creates an inherited field based on the given original field for the given
     * target type.
     *
     * @param originalField The original field that is inherited to the given type
     * @param targetType    The receiving type of the field
     * @return The created field
     */
    protected abstract F createInheritedField(F originalField, R targetType);

}
