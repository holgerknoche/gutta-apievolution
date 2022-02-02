package gutta.apievolution.json;

import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.node.ObjectNode;
import gutta.apievolution.core.apimodel.Field;
import gutta.apievolution.core.apimodel.RecordType;
import gutta.apievolution.core.apimodel.Type;
import gutta.apievolution.core.apimodel.consumer.ConsumerApiDefinition;
import gutta.apievolution.core.apimodel.consumer.ConsumerRecordType;

import java.io.IOException;
import java.util.NoSuchElementException;
import java.util.Optional;

/**
 * A consumer invocation proxy transparently handles revisioned communication on the consumer side, i.e. it transforms
 * the request to the public representation and the response to the internal representation.
 */
public abstract class ConsumerInvocationProxy extends AbstractInvocationProxy {

    private final ConsumerRecordType parameterType;

    private final ConsumerRecordType resultType;

    private final RequestRouter router;

    /**
     * Creates a new proxy using the given data.
     * @param apiDefinition The API definition to use
     * @param parameterTypeName The internal name of the parameter type in the API definition
     * @param resultTypeName The internal name of the result name in the API definition
     * @param router The router to use for the invocation
     */
    protected ConsumerInvocationProxy(ConsumerApiDefinition apiDefinition, String parameterTypeName,
                                      String resultTypeName, RequestRouter router) {
        Optional<ConsumerRecordType> optionalParameterType = apiDefinition.findUDTByInternalName(parameterTypeName);
        Optional<ConsumerRecordType> optionalResultType = apiDefinition.findUDTByInternalName(resultTypeName);

        this.parameterType = optionalParameterType.orElseThrow(NoSuchElementException::new);
        this.resultType = optionalResultType.orElseThrow(NoSuchElementException::new);
        this.router = router;
    }

    private JsonNode rewritePublicToConsumerInternal(Type type, JsonNode representation) {
        return new PublicToInternalRewriter().rewritePublicToInternal(type, representation);
    }

    /**
     * Invokes the provider method using the given data.
     * @param <T> The type of the result
     * @param apiId The API ID of the consumer API
     * @param referencedRevision The referenced provider revision
     * @param serviceName The service name to use
     * @param parameterObject The parameter object for the method
     * @param resultType The result type for result handling
     * @return The deserialized result
     */
    protected <T> T invokeMethod(String apiId, int referencedRevision, String serviceName, Object parameterObject,
                                 Class<T> resultType) {
        ObjectMapper objectMapper = OBJECT_MAPPER;

        try {
            JsonNode parameterNode = objectMapper.valueToTree(parameterObject);
            parameterNode = this.rewriteInternalToPublic(this.parameterType, parameterNode);

            String requestJson = objectMapper.writeValueAsString(parameterNode);
            String responseJson = this.router.invokeService(apiId, referencedRevision, serviceName, requestJson);

            JsonNode responseNode = objectMapper.readTree(responseJson);
            responseNode = this.rewritePublicToConsumerInternal(this.resultType, responseNode);

            return objectMapper.treeToValue(responseNode, resultType);
        } catch (IOException e) {
            throw new InvocationFailedException(e);
        }
    }

    private static class PublicToInternalRewriter extends AbstractPublicToInternalRewriter {

        @Override
        protected PublicToInternalRewriter fork() {
            return new PublicToInternalRewriter();
        }

        @Override
        public JsonNode handleRecordType(RecordType<?, ?, ?> recordType) {
            ObjectNode objectNode = (ObjectNode) representation;

            for (Field<?, ?> field : recordType.getDeclaredFields()) {
                JsonNode value = objectNode.remove(field.getPublicName());

                if (value != null) {
                    objectNode.set(field.getInternalName(), this.fork().rewritePublicToInternal(field.getType(),
                            value));
                }
            }

            return objectNode;
        }
    }

}
