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
import gutta.apievolution.core.apimodel.provider.RevisionHistory;
import gutta.apievolution.core.resolution.DefinitionResolution;
import gutta.apievolution.core.resolution.DefinitionResolver;
import gutta.apievolution.dsl.ConsumerApiLoader;

import java.io.IOException;
import java.util.Set;

/**
 * A provider operation proxy transparently handles revisioned communication on the provider side, i.e. it transforms the request to the internal representation
 * and the response to the public representation.
 */
public abstract class ProviderOperationProxy<P, R> extends AbstractOperationProxy {

    private final String serviceName;

    private final RevisionHistory revisionHistory;

    private final Set<Integer> supportedRevisions;

    private final String parameterTypeName;

    private final String resultTypeName;

    private final Class<P> parameterType;

    /**
     * Creates a new proxy using the given data.
     * 
     * @param serviceName        The name of the proxied service
     * @param revisionHistory    The revision history to use
     * @param supportedRevisions The set of supported revisions from the history
     * @param parameterTypeName  The internal name of the parameter type
     * @param resultTypeName     The internal name of the result type
     * @param parameterType      The actual parameter type for request handling
     */
    public ProviderOperationProxy(String serviceName, RevisionHistory revisionHistory, Set<Integer> supportedRevisions, String parameterTypeName,
            String resultTypeName, Class<P> parameterType) {
        this.serviceName = serviceName;
        this.revisionHistory = revisionHistory;
        this.supportedRevisions = supportedRevisions;
        this.parameterTypeName = parameterTypeName;
        this.resultTypeName = resultTypeName;
        this.parameterType = parameterType;
    }

    /**
     * Returns the name of the proxied service.
     * 
     * @return see above
     */
    public String getServiceName() {
        return this.serviceName;
    }

    private JsonNode rewritePublicToProviderInternal(Type type, DefinitionResolution definitionResolution, JsonNode representation) {
        return new PublicToInternalRewriter(definitionResolution).rewritePublicToInternal(type, representation);
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
    public String invokeOperation(String consumerApiId, String referencedApiName, int referencedRevision, String requestJson) {
        // We currently use the file name as the API id
        ConsumerApiDefinition consumerApi = ConsumerApiLoader.loadFromClasspath(consumerApiId, referencedApiName, referencedRevision);
        DefinitionResolution resolution = new DefinitionResolver().resolveConsumerDefinition(this.revisionHistory, this.supportedRevisions, consumerApi);

        return this.invokeOperation(resolution, requestJson);
    }

    /**
     * Invokes the underlying service method using the given data.
     * 
     * @param resolution  The resolution of the consumer API to use
     * @param requestJson The request in JSON format
     * @return The response in JSON format
     */
    public String invokeOperation(DefinitionResolution resolution, String requestJson) {
        ObjectMapper objectMapper = OBJECT_MAPPER;

        try {
            Type parameterType = resolution.resolveProviderType(this.parameterTypeName);
            JsonNode requestNode = objectMapper.readTree(requestJson);
            requestNode = this.rewritePublicToProviderInternal(parameterType, resolution, requestNode);

            P parameter = objectMapper.treeToValue(requestNode, this.parameterType);
            R result = this.invokeOperation(parameter);

            Type resultType = resolution.resolveProviderType(this.resultTypeName);
            JsonNode responseNode = objectMapper.valueToTree(result);
            responseNode = this.rewriteInternalToPublic(resultType, responseNode);

            return objectMapper.writeValueAsString(responseNode);
        } catch (IOException e) {
            throw new RuntimeException(e);
        }
    }

    /**
     * Invokes the actual service method with the given parameter.
     * 
     * @param parameter The parameter to pass to the service method
     * @return The result of the service method
     */
    protected abstract R invokeOperation(P parameter);

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

            for (Field<?, ?> field : recordType.getDeclaredFields()) {
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

    }

}
