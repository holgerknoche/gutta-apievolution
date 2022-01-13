package gutta.apievolution.json;

import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.node.ObjectNode;
import gutta.apievolution.core.apimodel.*;
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

public abstract class ProviderServiceProxy<P, R> extends AbstractInvocationProxy {

    private final String serviceName;

    private final RevisionHistory revisionHistory;

    private final Set<Integer> supportedRevisions;

    private final String parameterTypeName;

    private final String resultTypeName;

    private final Class<P> parameterType;

    public ProviderServiceProxy(String serviceName, RevisionHistory revisionHistory, Set<Integer> supportedRevisions,
                                String parameterTypeName, String resultTypeName, Class<P> parameterType) {
        this.serviceName = serviceName;
        this.revisionHistory = revisionHistory;
        this.supportedRevisions = supportedRevisions;
        this.parameterTypeName = parameterTypeName;
        this.resultTypeName = resultTypeName;
        this.parameterType = parameterType;
    }

    public String getServiceName() {
        return this.serviceName;
    }

    private JsonNode rewritePublicToProviderInternal(Type type, DefinitionResolution definitionResolution, JsonNode representation) {
        if (type instanceof RecordType) {
            RecordType<?, ?, ?> recordType = (RecordType<?, ?, ?>) type;

            ObjectNode objectNode = (ObjectNode) representation;

            for (Field<?, ?> field : recordType.getDeclaredFields()) {
                ConsumerField consumerField = definitionResolution.mapProviderField((ProviderField) field);
                if (consumerField == null) {
                    continue;
                }

                JsonNode value = objectNode.remove(consumerField.getPublicName());

                if (value != null) {
                    objectNode.set(field.getInternalName(), this.rewritePublicToProviderInternal(field.getType(), definitionResolution, value));
                }
            }

            return objectNode;
        } else if (type instanceof EnumType) {
            // TODO
            return representation;
        } else if (type instanceof ListType) {
            // TODO
            return representation;
        } else {
            return representation;
        }
    }

    public String invokeService(String consumerApiId, int referencedRevision, String requestJson) {
        ObjectMapper objectMapper = OBJECT_MAPPER;

        // We currently use the file name as the API id
        ConsumerApiDefinition consumerApi = ConsumerApiLoader.loadFromClasspath(consumerApiId, referencedRevision);
        DefinitionResolution resolution = new DefinitionResolver().resolveConsumerDefinition(this.revisionHistory,
                this.supportedRevisions, consumerApi);

        try {
            Type parameterType = resolution.resolveProviderType(this.parameterTypeName);
            JsonNode requestNode = objectMapper.readTree(requestJson);
            requestNode = this.rewritePublicToProviderInternal(parameterType, resolution, requestNode);

            P parameter = objectMapper.treeToValue(requestNode, this.parameterType);
            R result = this.invokeService(parameter);

            Type resultType = resolution.resolveProviderType(this.resultTypeName);
            JsonNode responseNode = objectMapper.valueToTree(result);
            responseNode = this.rewriteInternalToPublic(resultType, responseNode);

            return objectMapper.writeValueAsString(responseNode);
        } catch (IOException e) {
            throw new RuntimeException(e);
        }
    }

    protected abstract R invokeService(P parameter);

}
