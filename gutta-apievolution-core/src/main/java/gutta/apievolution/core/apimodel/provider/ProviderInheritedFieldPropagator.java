package gutta.apievolution.core.apimodel.provider;

import gutta.apievolution.core.apimodel.Inherited;
import gutta.apievolution.core.apimodel.InheritedFieldPropagator;

import static gutta.apievolution.core.apimodel.Conventions.noDeclaredPredecessors;

class ProviderInheritedFieldPropagator
        extends InheritedFieldPropagator<ProviderRecordType, ProviderField> {

    protected ProviderField createInheritedField(ProviderField originalField, ProviderRecordType targetType) {
        var predecessor = this.determinePredecessor(originalField, targetType);
        
        return targetType.newField(originalField.getPublicName(), originalField.getInternalName(), originalField.getType(),
                originalField.getOptionality(), Inherited.YES, noDeclaredPredecessors(), predecessor);
    }
    
    private ProviderField determinePredecessor(ProviderField originalField, ProviderRecordType targetType) {
        if (targetType.getPredecessor().isEmpty()) {
            // If the target type does not have a predecessor, the inherited field cannot have a predecessor
            return null;
        }
        
        var targetTypePredecessor = targetType.getPredecessor().get();       
        var numberOfDeclaredPredecessors = originalField.getDeclaredPredecessors().size();
        
        if (numberOfDeclaredPredecessors > 1) {
            // If more than one predecessor is explicitly declared (esp. when pulling up multiple fields into one), we have
            // to look for the matching one for this particular type
            for (var potentialPredecessorField : originalField.getDeclaredPredecessors()) {
                if (targetTypePredecessor.equals(potentialPredecessorField.getOwner())) {
                    // If the field is contained in the predecessor type of the target type, we have a match
                    return potentialPredecessorField;
                }
            }

            // There does not have to be a matching predecessor, since a type may inherit the field that did not previously
            // have it
            return null;
        } else if (originalField.getPredecessor().isPresent()) {
            // If the original field has a predecessor (whether explicit or implicit), we look for a matching (possibly also inherited)
            // field in the predecessor type of the target type
            var originalPredecessorField = originalField.getPredecessor().get();
            return targetTypePredecessor.resolveFieldByInternalName(originalPredecessorField.getInternalName()).orElse(null);
        } else {
            // If the original field does not have a predecessor, the inherited field does not have one as well
            return null;
        }
    }
    
}
