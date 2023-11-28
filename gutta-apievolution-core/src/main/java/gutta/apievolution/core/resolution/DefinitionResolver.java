package gutta.apievolution.core.resolution;

import gutta.apievolution.core.apimodel.Type;
import gutta.apievolution.core.apimodel.UserDefinedType;
import gutta.apievolution.core.apimodel.consumer.ConsumerApiDefinition;
import gutta.apievolution.core.apimodel.consumer.ConsumerEnumMember;
import gutta.apievolution.core.apimodel.consumer.ConsumerEnumType;
import gutta.apievolution.core.apimodel.consumer.ConsumerField;
import gutta.apievolution.core.apimodel.consumer.ConsumerOperation;
import gutta.apievolution.core.apimodel.consumer.ConsumerRecordType;
import gutta.apievolution.core.apimodel.consumer.ConsumerUserDefinedType;
import gutta.apievolution.core.apimodel.provider.ModelMerger;
import gutta.apievolution.core.apimodel.provider.ProviderApiDefinition;
import gutta.apievolution.core.apimodel.provider.ProviderEnumMember;
import gutta.apievolution.core.apimodel.provider.ProviderEnumType;
import gutta.apievolution.core.apimodel.provider.ProviderField;
import gutta.apievolution.core.apimodel.provider.ProviderOperation;
import gutta.apievolution.core.apimodel.provider.ProviderRecordType;
import gutta.apievolution.core.apimodel.provider.ProviderUserDefinedType;
import gutta.apievolution.core.apimodel.provider.RevisionHistory;
import gutta.apievolution.core.apimodel.provider.ToMergedModelMap;
import gutta.apievolution.core.validation.ValidationResult;

import java.util.HashMap;
import java.util.Map;
import java.util.Optional;
import java.util.Set;

/**
 * The definition resolver resolves client API definitions against provider API
 * revisions. The resulting resolution allows the provider to transform data
 * provided by the client into its own representation and vice-versa, thus
 * serving the client in a representation it is able to process.
 */
public class DefinitionResolver {

    /**
     * Resolves the given consumer API definition against a given provider revision
     * history.
     *
     * @param revisionHistory    The provider revision history
     * @param supportedRevisions The set of supported revision numbers
     * @param consumerApi        The consumer API to resolve
     * @return A resolution of the consumer API against the provider's internal
     *         representation
     */
    public DefinitionResolution resolveConsumerDefinition(RevisionHistory revisionHistory,
            Set<Integer> supportedRevisions, ConsumerApiDefinition consumerApi) {

        int desiredRevision = consumerApi.getReferencedRevision();
        if (!supportedRevisions.contains(desiredRevision)) {
            throw new DefinitionResolutionException("Revision " + desiredRevision + " is not supported.");
        }

        Optional<ProviderApiDefinition> optionalProviderApi = revisionHistory.getRevision(desiredRevision);
        if (!optionalProviderApi.isPresent()) {
            throw new DefinitionResolutionException("Revision " + desiredRevision + " does not exist.");
        }

        return this.resolveConsumerDefinitionAgainst(optionalProviderApi.get(), consumerApi, revisionHistory);
    }

    DefinitionResolution resolveConsumerDefinitionAgainst(ProviderApiDefinition providerApi,
            ConsumerApiDefinition consumerApi, RevisionHistory revisionHistory) {
        ConsumerToProviderMap consumerToProviderMap = this.createConsumerToProviderMap(consumerApi, providerApi);
        ProviderToConsumerMap providerToConsumerMap = consumerToProviderMap.invert();

        // Perform consistency checks on the maps between consumer API and provider revision
        ValidationResult c2pResult = consumerToProviderMap.checkConsistency();
        c2pResult.throwOnError(DefinitionResolutionException::new);
        
        ValidationResult p2cResult = providerToConsumerMap.checkConsistency();
        p2cResult.throwOnError(DefinitionResolutionException::new);
        
        ToMergedModelMap toMergedModelMap = new ModelMerger().createMergedDefinition(revisionHistory, providerApi);
        ValidationResult toMergedModelResult = toMergedModelMap.checkConsistency();
        toMergedModelResult.throwOnError(DefinitionResolutionException::new);
        
        // Compose the two maps to create a consumer -> provider-internal map
        ConsumerToProviderMap consumerToRepresentationMap = consumerToProviderMap.compose(toMergedModelMap);
        ProviderToConsumerMap representationToConsumerMap = consumerToRepresentationMap.invert();

        ValidationResult joinedValidationResult = new ValidationResult()
                .joinWith(c2pResult)
                .joinWith(p2cResult)
                .joinWith(toMergedModelResult);
        
        return new DefinitionResolution(consumerToRepresentationMap, representationToConsumerMap, joinedValidationResult.getMessages());
    }

    private ConsumerToProviderMap createConsumerToProviderMap(ConsumerApiDefinition consumerApi,
            ProviderApiDefinition providerApi) {
        Map<ConsumerUserDefinedType, ProviderUserDefinedType> consumerToProviderType = 
                this.createTypeMapping(providerApi, consumerApi);

        Map<ConsumerField, ProviderField> consumerToProviderField = this.createFieldMapping(consumerApi,
                consumerToProviderType);
        Map<ConsumerEnumMember, ProviderEnumMember> consumerToProviderMember = this.createMemberMapping(consumerApi,
                consumerToProviderType);
        Map<ConsumerOperation, ProviderOperation> consumerToProviderOperation = this.createOperationMapping(consumerApi,
                providerApi);

        return new ConsumerToProviderMap(consumerApi, providerApi, consumerToProviderType, consumerToProviderField,
                consumerToProviderMember, consumerToProviderOperation);
    }

    private Map<ConsumerUserDefinedType, ProviderUserDefinedType> createTypeMapping(ProviderApiDefinition providerApi,
            ConsumerApiDefinition consumerApi) {
        Map<ConsumerUserDefinedType, ProviderUserDefinedType> consumerToProviderType = new HashMap<>();

        for (UserDefinedType<ConsumerApiDefinition> consumerType : consumerApi.getUserDefinedTypes()) {
            String publicTypeName = consumerType.getPublicName();
            UserDefinedType<ProviderApiDefinition> providerType = providerApi.resolveUserDefinedType(publicTypeName)
                    .orElseThrow(() -> new DefinitionResolutionException(
                            "No matching type for " + consumerType + " (" + publicTypeName + ")."));
            this.assertMatchingType(consumerType, providerType);
            consumerToProviderType.put((ConsumerUserDefinedType) consumerType, (ProviderUserDefinedType) providerType);
        }

        return consumerToProviderType;
    }

    private void assertMatchingType(Type consumerType, Type providerType) {
        if (consumerType instanceof ConsumerEnumType && !(providerType instanceof ProviderEnumType)) {
            throw new DefinitionResolutionException(
                    "Consumer type '" + consumerType + " is an enum, but provider type " + providerType + " is not.");
        }
        if (consumerType instanceof ConsumerRecordType && !(providerType instanceof ProviderRecordType)) {
            throw new DefinitionResolutionException("Consumer type '" + consumerType +
                    " is a record type, but provider type " + providerType + " is not.");
        }
    }

    private Map<ConsumerField, ProviderField> createFieldMapping(ConsumerApiDefinition consumerApi,
            Map<ConsumerUserDefinedType, ProviderUserDefinedType> consumerToProviderType) {
        Map<ConsumerField, ProviderField> consumerToProviderField = new HashMap<>();

        for (UserDefinedType<ConsumerApiDefinition> consumerUDT : consumerApi.getUserDefinedTypes()) {
            if (!(consumerUDT instanceof ConsumerRecordType)) {
                continue;
            }

            ConsumerRecordType consumerType = (ConsumerRecordType) consumerUDT;
            ProviderRecordType providerType = (ProviderRecordType) consumerToProviderType.get(consumerType);
            for (ConsumerField consumerField : consumerType.getDeclaredFields()) {
                String fieldName = consumerField.getPublicName();
                ProviderField providerField = providerType.resolveField(fieldName)
                        .orElseThrow(() -> new DefinitionResolutionException(
                                "Missing field " + fieldName + " in provider type " + providerType));

                this.assertMatchingFields(consumerField, providerField);
                consumerToProviderField.put(consumerField, providerField);
            }
        }

        return consumerToProviderField;
    }

    private void assertMatchingFields(ConsumerField consumerField, ProviderField providerField) {
        // TODO
    }

    private Map<ConsumerEnumMember, ProviderEnumMember> createMemberMapping(ConsumerApiDefinition consumerApi,
            Map<ConsumerUserDefinedType, ProviderUserDefinedType> consumerToProviderType) {
        Map<ConsumerEnumMember, ProviderEnumMember> consumerToProviderMember = new HashMap<>();

        for (UserDefinedType<ConsumerApiDefinition> consumerUDT : consumerApi.getUserDefinedTypes()) {
            if (!(consumerUDT instanceof ConsumerEnumType)) {
                continue;
            }

            ConsumerEnumType consumerType = (ConsumerEnumType) consumerUDT;
            ProviderEnumType providerType = (ProviderEnumType) consumerToProviderType.get(consumerType);
            for (ConsumerEnumMember consumerMember : consumerType.getDeclaredMembers()) {
                String memberName = consumerMember.getPublicName();
                ProviderEnumMember providerMember = providerType.resolveMember(memberName)
                        .orElseThrow(() -> new DefinitionResolutionException(
                                "Missing member " + memberName + " in provider type " + providerType));

                this.assertMatchingMembers(consumerMember, providerMember);
                consumerToProviderMember.put(consumerMember, providerMember);
            }
        }

        return consumerToProviderMember;
    }

    private void assertMatchingMembers(ConsumerEnumMember consumerMember, ProviderEnumMember providerMember) {
        // TODO
    }

    private Map<ConsumerOperation, ProviderOperation> createOperationMapping(ConsumerApiDefinition consumerApi,
            ProviderApiDefinition providerApi) {
        Map<ConsumerOperation, ProviderOperation> consumerToProviderOperation = new HashMap<>();

        for (ConsumerOperation consumerOperation : consumerApi.getOperations()) {
            String operationName = consumerOperation.getPublicName();
            ProviderOperation providerOperation = providerApi.resolveOperation(operationName)
                    .orElseThrow(() -> new DefinitionResolutionException(
                            "Missing operation " + operationName + " in provider API."));

            this.assertMatchingOperations(consumerOperation, providerOperation);
            consumerToProviderOperation.put(consumerOperation, providerOperation);
        }

        return consumerToProviderOperation;
    }

    private void assertMatchingOperations(ConsumerOperation consumerOperation, ProviderOperation providerOperation) {
        // TODO
    }

}
