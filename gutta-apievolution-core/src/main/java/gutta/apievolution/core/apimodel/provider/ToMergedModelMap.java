package gutta.apievolution.core.apimodel.provider;

import gutta.apievolution.core.apimodel.Type;

import java.util.Map;
import java.util.Optional;

/**
 * This class represents a map from a distinct provider revision to a merged revision.
 */
public class ToMergedModelMap {

    private final ProviderTypeLookup typeLookup;

    private final Map<ProviderField, ProviderField> fieldMap;

    private final Map<ProviderEnumMember, ProviderEnumMember> enumMemberMap;
    
    private final Map<ProviderOperation, ProviderOperation> operationMap;

    ToMergedModelMap(ProviderTypeLookup typeLookup, Map<ProviderField, ProviderField> fieldMap,
                     Map<ProviderEnumMember, ProviderEnumMember> enumMemberMap,
                     Map<ProviderOperation, ProviderOperation> operationMap) {
        this.typeLookup = typeLookup;
        this.fieldMap = fieldMap;
        this.enumMemberMap = enumMemberMap;
        this.operationMap = operationMap;
    }

    /**
     * Maps the given type to its equivalent in the merged model.
     * @param <T> The expected metatype
     * @param type The type to map
     * @return The resulting type, if it exists
     */
    @SuppressWarnings("unchecked")
    public <T extends Type> Optional<T> mapType(Type type) {
        return Optional.ofNullable((T) this.typeLookup.lookupType(type));
    }

    /**
     * Maps the given field to its equivalent in the merged model.
     * @param field The field to map
     * @return The resulting field, if it exists
     */
    public Optional<ProviderField> mapField(ProviderField field) {
        return Optional.ofNullable(this.fieldMap.get(field));
    }

    /**
     * Maps the given enum member to its equivalent in the merged model.
     * @param member The enum member to map
     * @return The resulting member, if it exists
     */
    public Optional<ProviderEnumMember> mapEnumMember(ProviderEnumMember member) {
        return Optional.ofNullable(this.enumMemberMap.get(member));
    }
    
    /**
     * Maps the given operation to its equivalent in the merged model.
     * @param operation The operation to map
     * @return The resulting operation, if it exists
     */
    public Optional<ProviderOperation> mapOperation(ProviderOperation operation) {
        return Optional.ofNullable(this.operationMap.get(operation));
    }

}
