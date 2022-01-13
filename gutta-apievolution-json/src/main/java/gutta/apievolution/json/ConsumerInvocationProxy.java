package gutta.apievolution.json;

import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.node.ObjectNode;
import gutta.apievolution.core.apimodel.*;
import gutta.apievolution.core.apimodel.consumer.ConsumerApiDefinition;
import gutta.apievolution.core.apimodel.consumer.ConsumerRecordType;

import java.io.IOException;
import java.util.NoSuchElementException;
import java.util.Optional;

public class ConsumerInvocationProxy extends AbstractInvocationProxy {

    private final ConsumerRecordType parameterType;

    private final ConsumerRecordType resultType;

    private final RequestRouter router;

    protected ConsumerInvocationProxy(ConsumerApiDefinition apiDefinition, String parameterTypeName,
                                      String resultTypeName, RequestRouter router) {
        Optional<ConsumerRecordType> optionalParameterType = apiDefinition.findUDTByInternalName(parameterTypeName);
        Optional<ConsumerRecordType> optionalResultType = apiDefinition.findUDTByInternalName(resultTypeName);

        this.parameterType = optionalParameterType.orElseThrow(NoSuchElementException::new);
        this.resultType = optionalResultType.orElseThrow(NoSuchElementException::new);
        this.router = router;
    }

    protected ConsumerInvocationProxy(ConsumerRecordType parameterType, ConsumerRecordType resultType, RequestRouter router) {
        this.parameterType = parameterType;
        this.resultType = resultType;
        this.router = router;
    }

    private JsonNode rewritePublicToConsumerInternal(Type type, JsonNode representation) {
        if (type instanceof RecordType) {
            RecordType<?, ?, ?> recordType = (RecordType<?, ?, ?>) type;

            ObjectNode objectNode = (ObjectNode) representation;

            for (Field<?, ?> field : recordType.getDeclaredFields()) {
                JsonNode value = objectNode.remove(field.getPublicName());

                if (value != null) {
                    objectNode.set(field.getInternalName(), this.rewritePublicToConsumerInternal(field.getType(), value));
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

    protected <T> T invokeMethod(String apiId, int referencedRevision, String serviceName, Object parameterObject, Class<T> resultType) {
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
            throw new RuntimeException(e);
        }
    }

}
