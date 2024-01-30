package gutta.apievolution.json;

import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.node.ObjectNode;
import gutta.apievolution.core.apimodel.Field;
import gutta.apievolution.core.apimodel.RecordType;
import gutta.apievolution.core.apimodel.Type;
import gutta.apievolution.core.apimodel.consumer.ConsumerApiDefinition;
import gutta.apievolution.core.apimodel.consumer.ConsumerField;
import gutta.apievolution.core.apimodel.provider.ProviderField;
import gutta.apievolution.core.apimodel.provider.ProviderRecordType;
import gutta.apievolution.core.apimodel.provider.RevisionHistory;
import gutta.apievolution.core.resolution.DefinitionResolution;
import gutta.apievolution.core.resolution.DefinitionResolver;
import gutta.apievolution.dsl.ConsumerApiLoader;

import java.io.IOException;
import java.util.Set;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.ConcurrentMap;

/**
 * A provider operation proxy transparently handles revisioned communication on the provider side, i.e. it transforms the request to the internal representation
 * and the response to the public representation.
 */
public abstract class ProviderOperationProxy<P, R> extends AbstractOperationProxy<P, R> {

    private final RevisionHistory revisionHistory;

    private final Set<Integer> supportedRevisions;

    private final Class<P> parameterType;

    private final ConcurrentMap<String, DefinitionResolution> resolutionCache = new ConcurrentHashMap<>();

    /**
     * Creates a new proxy using the given data.
     * 
     * @param operationName      The name of the proxied service
     * @param revisionHistory    The revision history to use
     * @param supportedRevisions The set of supported revisions from the history
     * @param parameterTypeName  The internal name of the parameter type
     * @param resultTypeName     The internal name of the result type
     * @param parameterType      The actual parameter type for request handling
     */
    public ProviderOperationProxy(String operationName, RevisionHistory revisionHistory, Set<Integer> supportedRevisions, String parameterTypeName,
            String resultTypeName, Class<P> parameterType) {

        super(operationName, parameterTypeName, resultTypeName);

        this.revisionHistory = revisionHistory;
        this.supportedRevisions = supportedRevisions;
        this.parameterType = parameterType;
    }

    @SuppressWarnings("unchecked")
    private <T extends JsonNode> T rewritePublicToProviderInternal(Type type, DefinitionResolution definitionResolution, JsonNode representation) {
        return (T) new PublicToInternalRewriter(definitionResolution).rewritePublicToInternal(type, representation);
    }

    private DefinitionResolution createApiResolution(String consumerApiId, String referencedApiName, int referencedRevision) {
        // We currently use the file name as the API id
        ConsumerApiDefinition consumerApi = ConsumerApiLoader.loadFromClasspath(consumerApiId, referencedApiName, referencedRevision);
        return new DefinitionResolver().resolveConsumerDefinition(this.revisionHistory, this.supportedRevisions, consumerApi);
    }

    /**
     * Invokes the underlying service method using the given data.
     * 
     * @param consumerApiId      The consumer API ID that was used for the request
     * @param referencedApiName  The name of the referenced provider API
     * @param referencedRevision The provider revision referenced by the consumer API
     * @param requestJson        The request in JSON format
     * @return The response in JSON format
     */
    public byte[] invokeOperation(String consumerApiId, String referencedApiName, int referencedRevision, byte[] requestJson) {
        DefinitionResolution resolution = this.resolutionCache.computeIfAbsent(consumerApiId,
                consumerId -> this.createApiResolution(consumerId, referencedApiName, referencedRevision));

        return this.invokeOperation(resolution, requestJson);
    }

    /**
     * Invokes the underlying service method using the given data.
     * 
     * @param resolution  The resolution of the consumer API to use
     * @param requestJson The request in JSON format
     * @return The response in JSON format
     */
    public byte[] invokeOperation(DefinitionResolution resolution, byte[] requestJson) {
        ObjectMapper objectMapper = OBJECT_MAPPER;

        try {
            ObjectNode requestNode = (ObjectNode) objectMapper.readTree(requestJson);

            // Determine the actual parameter type name (may be a subtype)
            String actualParameterTypeName = determineSpecificTypeId(requestNode).orElse(this.getParameterTypeName());
            Type parameterType = resolution.resolveProviderType(actualParameterTypeName);
            requestNode = (ObjectNode) this.rewritePublicToProviderInternal(parameterType, resolution, requestNode);

            P parameter = objectMapper.treeToValue(requestNode, this.parameterType);
            R result = this.invokeOperation(parameter);

            Type resultType = resolution.resolveProviderType(this.getResultTypeName());
            JsonNode responseNode = (ObjectNode) objectMapper.valueToTree(result);
            responseNode = this.rewriteInternalToPublic(resultType, resolution, responseNode);
            return objectMapper.writeValueAsBytes(responseNode);
        } catch (IOException e) {
            throw new RuntimeException(e);
        }
    }

    private JsonNode rewriteInternalToPublic(Type type, DefinitionResolution resolution, JsonNode representation) {
        return new InternalToPublicRewriter(resolution).rewriteInternalToPublic(type, representation);
    }

    /**
     * Specific visitor for rewriting a public to a provider-internal representation.
     */
    private static class PublicToInternalRewriter extends AbstractPublicToInternalRewriter {

        private final DefinitionResolution definitionResolution;

        public PublicToInternalRewriter(DefinitionResolution definitionResolution) {
            this.definitionResolution = definitionResolution;
        }

        @Override
        protected PublicToInternalRewriter fork() {
            return new PublicToInternalRewriter(this.definitionResolution);
        }

        @Override
        public JsonNode handleRecordType(RecordType<?, ?, ?> recordType) {
            ObjectNode objectNode = (ObjectNode) representation;

            this.rewriteTypeIdentifier(objectNode, recordType);

            for (Field<?, ?> field : recordType) {
                // Determine the consumer field that is mapped to the provider-internal field. If no such field exists,
                // it is assumed that the consumer does not provide this field. Note that the public name of the provider-internal
                // field may differ from the public name of the consumer field, as the provider-internal representation
                // is built from the latest revision of the field.
                ConsumerField consumerField = this.definitionResolution.mapProviderField((ProviderField) field);
                if (consumerField == null) {
                    continue;
                }

                JsonNode value = objectNode.remove(consumerField.getPublicName());

                if (value != null) {
                    objectNode.set(field.getInternalName(), this.fork().rewritePublicToInternal(field.getType(), value));
                }
            }

            return objectNode;
        }

        @Override
        protected JsonNode onUnrepresentableEnumMember(String name) {
            // Unrepresentable values are only possible on the consumer side
            throw new IllegalStateException("Unknown member name '" + name + "' encountered.");
        }

    }

    private static class InternalToPublicRewriter extends AbstractInternalToPublicRewriter {

        private final DefinitionResolution definitionResolution;

        public InternalToPublicRewriter(DefinitionResolution definitionResolution) {
            this.definitionResolution = definitionResolution;
        }

        @Override
        protected AbstractInternalToPublicRewriter fork() {
            return new InternalToPublicRewriter(this.definitionResolution);
        }

        @Override
        protected ObjectNode handlePolymorphicRecordType(String typeId, ObjectNode objectNode) {
            ProviderRecordType actualProviderType = (ProviderRecordType) this.definitionResolution.resolveProviderType(typeId);

            if (actualProviderType == null) {
                // If no mapped type with the given type id exists, we have an unrepresentable value
                return createUnrepresentableValue();
            } else {
                return this.rewriteRecord(actualProviderType, objectNode);
            }
        }

        @Override
        protected ObjectNode rewriteRecord(RecordType<?, ?, ?> recordType, ObjectNode objectNode) {
            for (Field<?, ?> field : recordType) {
                JsonNode value = objectNode.remove(field.getInternalName());

                Field<?, ?> consumerField = this.definitionResolution.mapField(field);
                if (consumerField != null) {
                    objectNode.set(consumerField.getPublicName(), this.fork().rewriteInternalToPublic(field.getType(), value));
                }
            }

            return objectNode;
        }

    }

}
