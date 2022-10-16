package gutta.apievolution.core.apimodel.provider;

import gutta.apievolution.core.apimodel.ApiDefinitionMorphism;
import gutta.apievolution.core.apimodel.TypeMap;
import gutta.apievolution.core.util.CheckResult;

import java.util.Map;

/**
 * This class represents a map from a distinct provider revision to a merged
 * revision.
 */
public class ToMergedModelMap extends ApiDefinitionMorphism<ProviderApiDefinition, ProviderApiDefinition,
    ProviderUserDefinedType, ProviderUserDefinedType, ProviderField, ProviderField, ProviderEnumMember,
    ProviderEnumMember, ProviderOperation, ProviderOperation> {

    ToMergedModelMap(ProviderApiDefinition sourceDefinition, ProviderApiDefinition targetDefinition,
            TypeMap<ProviderUserDefinedType, ProviderUserDefinedType> typeMap,
            Map<ProviderField, ProviderField> fieldMap,
            Map<ProviderEnumMember, ProviderEnumMember> enumMemberMap,
            Map<ProviderOperation, ProviderOperation> operationMap) {
        
        super(sourceDefinition, targetDefinition, typeMap, fieldMap, enumMemberMap, operationMap);
    }

    @Override
    public CheckResult checkConsistency() {
        CheckResult superResult = super.checkConsistency();
        
        CheckResult ownResult = new CheckResult();
        this.checkSuperTypeConsistency(ownResult);
        
        return superResult.joinWith(ownResult);
    }
    
}
