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
 * A consumer operation proxy transparently handles revisioned communication on the consumer side, i.e. it transforms the request to the public representation
 * and the response to the internal representation.
 */
public abstract class ConsumerOperationProxy<P, R> extends AbstractOperationProxy<P, R> {

    private final ConsumerApiDefinition apiDefinition;

    private final String apiId;

    private final ConsumerRecordType parameterType;

    private final ConsumerRecordType resultType;

    private final Class<R> resultTypeRepresentation;

    private final RequestRouter router;

    /**
     * Creates a new proxy using the given data.
     * 
     * @param apiDefinition     The API definition to use
     * @param apiId             The API ID of the consumer API
     * @param operationName     The name of the invoked operation
     * @param parameterTypeName The internal name of the parameter type in the API definition
     * @param resultTypeName    The internal name of the result name in the API definition
     * @param router            The router to use for the invocation
     */
    protected ConsumerOperationProxy(ConsumerApiDefinition apiDefinition, String apiId, String operationName, String parameterTypeName, String resultTypeName,
            Class<R> resultTypeRepresentation, RequestRouter router) {
        
        super(operationName, parameterTypeName, resultTypeName);
        
        Optional<ConsumerRecordType> optionalParameterType = apiDefinition.findUDTByInternalName(parameterTypeName);
        Optional<ConsumerRecordType> optionalResultType = apiDefinition.findUDTByInternalName(resultTypeName);

        this.apiDefinition = apiDefinition;
        this.apiId = apiId;
        this.parameterType = optionalParameterType.orElseThrow(NoSuchElementException::new);
        this.resultType = optionalResultType.orElseThrow(NoSuchElementException::new);
        this.resultTypeRepresentation = resultTypeRepresentation;
        this.router = router;
    }

    private JsonNode rewritePublicToConsumerInternal(Type type, JsonNode representation) {
        return new PublicToInternalRewriter().rewritePublicToInternal(type, representation);
    }

    /**
     * Invokes the provider operation using the given data.
     * 
     * @param parameterObject The parameter object for the method
     * @return The deserialized result
     */
    public R invokeOperation(P parameterObject) {
        ObjectMapper objectMapper = OBJECT_MAPPER;
        int referencedRevision = this.apiDefinition.getReferencedRevision();

        try {
            JsonNode parameterNode = objectMapper.valueToTree(parameterObject);
            parameterNode = this.rewriteInternalToPublic(this.parameterType, parameterNode);

            byte[] requestJson = objectMapper.writeValueAsBytes(parameterNode);
            byte[] responseJson = this.router.invokeOperation(this.apiId, referencedRevision, this.getOperationName(), requestJson);

            JsonNode responseNode = objectMapper.readTree(responseJson);
            responseNode = this.rewritePublicToConsumerInternal(this.resultType, responseNode);

            return objectMapper.treeToValue(responseNode, this.resultTypeRepresentation);
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

            this.rewriteTypeIdentifier(objectNode, recordType);
            
            for (Field<?, ?> field : recordType.getDeclaredFields()) {
                JsonNode value = objectNode.remove(field.getPublicName());

                if (value != null) {
                    objectNode.set(field.getInternalName(), this.fork().rewritePublicToInternal(field.getType(), value));
                }
            }

            return objectNode;
        }
    }

}
