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
import java.util.function.Function;

/**
 * A consumer operation proxy transparently handles revisioned communication on the consumer side, i.e. it transforms the request to the public representation
 * and the response to the internal representation.
 */
public abstract class ConsumerOperationProxy<P, R> extends AbstractOperationProxy<P, R> {

    private final ConsumerApiDefinition apiDefinition;

    private final String apiId;

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

        this.apiDefinition = apiDefinition;
        this.apiId = apiId;
        this.resultTypeRepresentation = resultTypeRepresentation;
        this.router = router;
    }

    private JsonNode rewritePublicToConsumerInternal(Type type, Function<String, Type> typeResolver, JsonNode representation,
            OnUnrepresentableValue<?> onUnrepresentableValue) {
        return new PublicToInternalRewriter(typeResolver, onUnrepresentableValue).rewritePublicToInternal(type, representation);
    }

    /**
     * Invokes the provider operation using the given data and throws an exception on unrepresentable values.
     * 
     * @param parameterObject The parameter object for the method
     * @return The deserialized result
     * @throws UnrepresentableValueException If an unrepresentable value is encountered during deserialization
     */
    public R invokeOperation(P parameterObject) {
        return this.invokeOperation(parameterObject, OnUnrepresentableValue.throwException());
    }

    /**
     * Invokes the provider operation using the given data, and performs the given action if an unrepresentable value is encountered.
     * 
     * @param parameterObject        The parameter object for the method
     * @param onUnrepresentableValue The action to perform if an unrepresentable value is encountered
     * @return The deserialized result
     */
    public R invokeOperation(P parameterObject, OnUnrepresentableValue<?> onUnrepresentableValue) {
        ObjectMapper objectMapper = OBJECT_MAPPER;
        int referencedRevision = this.apiDefinition.getReferencedRevision();

        try {
            JsonNode parameterNode = objectMapper.valueToTree(parameterObject);

            String actualParameterTypeInternalName = determineSpecificTypeId(parameterNode).orElse(this.getParameterTypeName());
            Type parameterType = this.resolveTypeByInternalName(actualParameterTypeInternalName);
            parameterNode = (ObjectNode) this.rewriteInternalToPublic(parameterType, this::resolveTypeByInternalName, parameterNode);

            byte[] requestJson = objectMapper.writeValueAsBytes(parameterNode);
            byte[] responseJson = this.router.invokeOperation(this.apiId, referencedRevision, this.getOperationName(), requestJson);

            JsonNode responseNode = objectMapper.readTree(responseJson);

            String actualResultTypePublicName = determineSpecificTypeId(responseNode).orElse(this.getResultTypeName());
            Type resultType = this.resolveTypeByPublicName(actualResultTypePublicName);
            responseNode = this.rewritePublicToConsumerInternal(resultType, this::resolveTypeByPublicName, responseNode, onUnrepresentableValue);

            return objectMapper.treeToValue(responseNode, this.resultTypeRepresentation);
        } catch (IOException e) {
            throw new InvocationFailedException("Error while processing JSON on the consumer side.", e);
        }
    }

    private Type resolveTypeByInternalName(String internalName) {
        return this.apiDefinition.findUDTByInternalName(internalName).orElse(null);
    }

    private Type resolveTypeByPublicName(String publicName) {
        return this.apiDefinition.resolveUserDefinedType(publicName).orElse(null);
    }

    private JsonNode rewriteInternalToPublic(Type type, Function<String, Type> typeResolver, JsonNode representation) {
        return new InternalToPublicRewriter(typeResolver).rewriteInternalToPublic(type, representation);
    }

    private static class PublicToInternalRewriter extends AbstractPublicToInternalRewriter {

        private final Function<String, Type> typeResolver;

        private final OnUnrepresentableValue<?> onUnrepresentableValue;

        public PublicToInternalRewriter(Function<String, Type> typeResolver, OnUnrepresentableValue<?> onUnrepresentableValue) {
            this.typeResolver = typeResolver;
            this.onUnrepresentableValue = onUnrepresentableValue;
        }

        @Override
        protected AbstractPublicToInternalRewriter fork() {
            return new PublicToInternalRewriter(this.typeResolver, this.onUnrepresentableValue);
        }

        @Override
        protected JsonNode handlePolymorphicRecordType(String typeId, ObjectNode objectNode) {
            ConsumerRecordType type = (ConsumerRecordType) this.typeResolver.apply(typeId);
            if (type == null) {
                throw new IllegalArgumentException("Unknown type id '" + typeId + "'.");
            }

            // Set the appropriate type ID and rewrite the node according to the actual type
            this.handleTypeIdentifier(objectNode, type, RecordType::getInternalName);
            return this.rewriteRecord(type, objectNode);
        }

        @Override
        protected JsonNode rewriteRecord(RecordType<?, ?, ?> recordType, ObjectNode objectNode) {
            if (isUnrepresentableValue(objectNode)) {
                return this.onUnrepresentableValue.throwExceptionOrReturnDefaultNode();
            }

            this.handleTypeIdentifier(objectNode, recordType, RecordType::getInternalName);

            for (Field<?, ?> field : recordType.getDeclaredFields()) {
                JsonNode value = objectNode.remove(field.getPublicName());

                if (value != null) {
                    objectNode.set(field.getInternalName(), this.fork().rewritePublicToInternal(field.getType(), value));
                }
            }

            return objectNode;
        }

        @Override
        protected JsonNode onUnrepresentableEnumMember(String name) {
            return this.onUnrepresentableValue.throwExceptionOrReturnDefaultNode();
        }

    }

    private static class InternalToPublicRewriter extends AbstractInternalToPublicRewriter {

        private final Function<String, Type> typeResolver;

        public InternalToPublicRewriter(Function<String, Type> typeResolver) {
            this.typeResolver = typeResolver;
        }

        @Override
        protected AbstractInternalToPublicRewriter fork() {
            return new InternalToPublicRewriter(this.typeResolver);
        }

        @Override
        protected ObjectNode handlePolymorphicRecordType(String typeId, ObjectNode objectNode) {
            ConsumerRecordType type = (ConsumerRecordType) this.typeResolver.apply(typeId);
            if (type == null) {
                throw new IllegalArgumentException("Unknown type id '" + typeId + "'.");
            }

            // Set the appropriate type ID and rewrite the node according to the actual type
            this.handleTypeIdentifier(objectNode, type, RecordType::getPublicName);
            return this.rewriteRecord(type, objectNode);
        }

        @Override
        protected ObjectNode rewriteRecord(RecordType<?, ?, ?> recordType, ObjectNode objectNode) {
            for (Field<?, ?> field : recordType) {
                JsonNode value = objectNode.remove(field.getInternalName());
                objectNode.set(field.getPublicName(), this.fork().rewriteInternalToPublic(field.getType(), value));
            }

            return objectNode;
        }

    }

}
