package gutta.apievolution.core.resolution;

import gutta.apievolution.core.apimodel.Type;
import gutta.apievolution.core.apimodel.UserDefinedType;
import gutta.apievolution.core.apimodel.consumer.*;
import gutta.apievolution.core.apimodel.provider.*;

import java.util.HashMap;
import java.util.Map;
import java.util.Optional;
import java.util.Set;

/**
 * The definition resolver resolves client API definitions against provider API revisions. The resulting resolution
 * allows the provider to transform data provided by the client into its own representation and vice-versa, thus
 * serving the client in a representation it is able to process.
 */
public class DefinitionResolver {

    /**
     * Resolves the given consumer API definition against a given provider revision history.
     * @param revisionHistory The provider revision history
     * @param supportedRevisions The set of supported revision numbers
     * @param consumerApi The consumer API to resolve
     * @return A resolution of the consumer API against the provider's internal representation
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
                                          ConsumerApiDefinition consumerApi,
                                          RevisionHistory revisionHistory) {
        ConsumerToProviderMap consumerToProviderMap = this.createConsumerToProviderMap(consumerApi, providerApi);
        ProviderToConsumerMap providerToConsumerMap = consumerToProviderMap.invert();

        // Perform consistency checks on the maps between consumer API and provider revision
        consumerToProviderMap.checkConsistency();
        providerToConsumerMap.checkConsistency();

        ToMergedModelMap toMergedModelMap = new ModelMerger().createMergedDefinition(revisionHistory, providerApi);

        // Compose the two maps to create a consumer -> provider-internal map
        ConsumerToProviderMap consumerToRepresentationMap = consumerToProviderMap.compose(toMergedModelMap);
        ProviderToConsumerMap representationToConsumerMap = consumerToRepresentationMap.invert();

        return new DefinitionResolution(consumerToRepresentationMap, representationToConsumerMap);
    }

    private ConsumerToProviderMap createConsumerToProviderMap(ConsumerApiDefinition consumerApi,
                                                              ProviderApiDefinition providerApi) {
        Map<Type, Type> consumerToProviderType = this.createTypeMapping(providerApi, consumerApi);

        Map<ConsumerField, ProviderField> consumerToProviderField = this.createFieldMapping(consumerApi,
                consumerToProviderType);
        Map<ConsumerEnumMember, ProviderEnumMember> consumerToProviderMember = this.createMemberMapping(consumerApi,
                consumerToProviderType);

        return new ConsumerToProviderMap(consumerToProviderType, consumerToProviderField, consumerToProviderMember);
    }

    private Map<Type, Type> createTypeMapping(ProviderApiDefinition providerApi, ConsumerApiDefinition consumerApi) {
        Map<Type, Type> consumerToProviderType = new HashMap<>();

        for (UserDefinedType<ConsumerApiDefinition> consumerType : consumerApi.getUserDefinedTypes()) {
            String publicTypeName = consumerType.getPublicName();
            Type providerType = providerApi.resolveUserDefinedType(publicTypeName)
                    .orElseThrow(() -> new DefinitionResolutionException("No matching type for " + consumerType +
                            " (" + publicTypeName + ")."));
            this.assertMatchingType(consumerType, providerType);
            consumerToProviderType.put(consumerType, providerType);
        }

        return consumerToProviderType;
    }

    private void assertMatchingType(Type consumerType, Type providerType) {
        if (consumerType instanceof ConsumerEnumType && !(providerType instanceof ProviderEnumType)) {
            throw new DefinitionResolutionException("Consumer type '" + consumerType +
                    " is an enum, but provider type " + providerType + " is not.");
        }
        if (consumerType instanceof ConsumerRecordType && !(providerType instanceof ProviderRecordType)) {
            throw new DefinitionResolutionException("Consumer type '" + consumerType +
                    " is a record type, but provider type " + providerType + " is not.");
        }
    }

    private Map<ConsumerField, ProviderField> createFieldMapping(ConsumerApiDefinition consumerApi,
                                                                 Map<Type, Type> consumerToProviderType) {
        Map<ConsumerField, ProviderField> consumerToProviderField = new HashMap<>();

        for (UserDefinedType<ConsumerApiDefinition> consumerUDT : consumerApi.getUserDefinedTypes()) {
            if (!(consumerUDT instanceof ConsumerRecordType)) {
                continue;
            }

            ConsumerRecordType consumerType = (ConsumerRecordType) consumerUDT;
            ProviderRecordType providerType = (ProviderRecordType) consumerToProviderType.get(consumerType);
            for (ConsumerField consumerField : consumerType.getDeclaredFields()) {
                String fieldName = consumerField.getPublicName();
                ProviderField providerField = providerType.resolveField(fieldName).orElseThrow(
                        () -> new DefinitionResolutionException("Missing field " + fieldName + " in provider type " +
                                providerType)
                );

                consumerToProviderField.put(consumerField, providerField);
            }
        }

        return consumerToProviderField;
    }

    private Map<ConsumerEnumMember, ProviderEnumMember> createMemberMapping(ConsumerApiDefinition consumerApi,
                                                                            Map<Type, Type> consumerToProviderType) {
        Map<ConsumerEnumMember, ProviderEnumMember> consumerToProviderMember = new HashMap<>();

        for (UserDefinedType<ConsumerApiDefinition> consumerUDT : consumerApi.getUserDefinedTypes()) {
            if (!(consumerUDT instanceof ConsumerEnumType)) {
                continue;
            }

            ConsumerEnumType consumerType = (ConsumerEnumType) consumerUDT;
            ProviderEnumType providerType = (ProviderEnumType) consumerToProviderType.get(consumerType);
            for (ConsumerEnumMember consumerMember : consumerType.getDeclaredMembers()) {
                String memberName = consumerMember.getPublicName();
                ProviderEnumMember providerMember = providerType.resolveMember(memberName).orElseThrow(
                        () -> new DefinitionResolutionException("Missing member " + memberName + " in provider type " +
                                providerType)
                );

                consumerToProviderMember.put(consumerMember, providerMember);
            }
        }

        return consumerToProviderMember;
    }

}
