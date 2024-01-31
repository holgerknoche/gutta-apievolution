package gutta.apievolution.json;

import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.SerializationFeature;
import com.fasterxml.jackson.databind.node.ArrayNode;
import com.fasterxml.jackson.databind.node.BooleanNode;
import com.fasterxml.jackson.databind.node.ObjectNode;
import com.fasterxml.jackson.databind.node.TextNode;
import gutta.apievolution.core.apimodel.AtomicType;
import gutta.apievolution.core.apimodel.BoundedListType;
import gutta.apievolution.core.apimodel.BoundedStringType;
import gutta.apievolution.core.apimodel.EnumMember;
import gutta.apievolution.core.apimodel.EnumType;
import gutta.apievolution.core.apimodel.ListType;
import gutta.apievolution.core.apimodel.NumericType;
import gutta.apievolution.core.apimodel.RecordType;
import gutta.apievolution.core.apimodel.Type;
import gutta.apievolution.core.apimodel.TypeVisitor;
import gutta.apievolution.core.apimodel.UnboundedListType;
import gutta.apievolution.core.apimodel.UnboundedStringType;

import java.util.NoSuchElementException;
import java.util.Optional;
import java.util.function.Function;

abstract class AbstractOperationProxy<P, R> {

    protected static final ObjectMapper OBJECT_MAPPER = createObjectMapper();

    private static final String TYPE_PROPERTY_NAME = "@type";

    private static final String UNREPRESENTABLE_PROPERTY_NAME = "@unrepresentable";

    private final String operationName;

    private final String parameterTypeName;

    private final String resultTypeName;

    private static ObjectMapper createObjectMapper() {
        ObjectMapper objectMapper = new ObjectMapper();

        objectMapper.disable(SerializationFeature.FAIL_ON_EMPTY_BEANS);

        return objectMapper;
    }

    protected AbstractOperationProxy(String operationName, String parameterTypeName, String resultTypeName) {
        this.operationName = operationName;
        this.parameterTypeName = parameterTypeName;
        this.resultTypeName = resultTypeName;
    }

    public String getOperationName() {
        return this.operationName;
    }

    protected String getParameterTypeName() {
        return this.parameterTypeName;
    }

    protected String getResultTypeName() {
        return this.resultTypeName;
    }

    protected static Optional<String> determineSpecificTypeId(JsonNode node) {
        JsonNode typePropertyNode = node.get(TYPE_PROPERTY_NAME);

        if (typePropertyNode == null || !typePropertyNode.isTextual()) {
            return Optional.empty();
        }

        return Optional.of(typePropertyNode.asText());
    }

    protected static boolean isUnrepresentableValue(ObjectNode node) {
        return node.has(UNREPRESENTABLE_PROPERTY_NAME);
    }

    protected static ObjectNode createUnrepresentableValue() {
        ObjectNode node = OBJECT_MAPPER.createObjectNode();
        node.set(UNREPRESENTABLE_PROPERTY_NAME, BooleanNode.TRUE);
        return node;
    }

    /**
     * Invokes the operation with the given parameter.
     * 
     * @param parameter The parameter to pass to the operation
     * @return The result of the operation
     */
    protected abstract R invokeOperation(P parameter);

    private abstract static class AbstractRepresentationRewriter implements TypeVisitor<JsonNode> {
     
        protected JsonNode representation;
        
        protected boolean requiresTypeIdentifier(RecordType<?, ?, ?> type) {
            return (type.hasSuperTypes() || type.hasSubTypes());
        }
        
        protected void handleTypeIdentifier(ObjectNode node, RecordType<?, ?, ?> type, Function<RecordType<?, ?, ?>, String> nameAccessor) {
            if (this.requiresTypeIdentifier(type)) {
                node.set(TYPE_PROPERTY_NAME, new TextNode(nameAccessor.apply(type)));
            } else {
                node.remove(TYPE_PROPERTY_NAME);
            }
        }
        
        @Override
        public JsonNode handleRecordType(RecordType<?, ?, ?> recordType) {
            ObjectNode objectNode = (ObjectNode) this.representation;

            Optional<String> specificTypeId = determineSpecificTypeId(objectNode);
            if (specificTypeId.isPresent()) {
                // If a specific type ID is present, the actual type may be a subtype of
                // the formal type
                return this.handlePolymorphicRecordType(specificTypeId.get(), objectNode);
            } else {
                // If no type ID is present, simply rewrite the record with the given type
                return this.rewriteRecord(recordType, objectNode);
            }
        }

        protected abstract JsonNode handlePolymorphicRecordType(String typeId, ObjectNode objectNode);

        protected abstract JsonNode rewriteRecord(RecordType<?, ?, ?> recordType, ObjectNode objectNode);
        
    }
    
    /**
     * Visitor implementation for rewriting an internal representation to a public representation.
     */
    protected abstract static class AbstractInternalToPublicRewriter extends AbstractRepresentationRewriter {

        public JsonNode rewriteInternalToPublic(Type type, JsonNode representation) {
            this.representation = representation;
            return type.accept(this);
        }

        protected abstract AbstractInternalToPublicRewriter fork();

        @Override
        public JsonNode handleEnumType(EnumType<?, ?, ?> enumType) {
            TextNode textNode = (TextNode) this.representation;
            String value = textNode.asText();

            EnumMember<?, ?> enumMember = enumType.findMemberByInternalName(value).orElseThrow(NoSuchElementException::new);

            return new TextNode(enumMember.getPublicName());
        }

        JsonNode handleListType(ListType listType) {
            if (this.representation.isNull()) {
                return this.representation;
            }

            AbstractInternalToPublicRewriter rewriter = this.fork();
            Type elementType = listType.getElementType();

            ArrayNode arrayNode = (ArrayNode) this.representation;
            ArrayNode rewrittenArrayNode = OBJECT_MAPPER.createArrayNode();

            for (JsonNode subNode : arrayNode) {
                JsonNode rewrittenSubNode = rewriter.rewriteInternalToPublic(elementType, subNode);
                rewrittenArrayNode.add(rewrittenSubNode);
            }

            return rewrittenArrayNode;
        }

        @Override
        public JsonNode handleBoundedListType(BoundedListType boundedListType) {
            return this.handleListType(boundedListType);
        }

        @Override
        public JsonNode handleUnboundedListType(UnboundedListType unboundedListType) {
            return this.handleListType(unboundedListType);
        }

        @Override
        public JsonNode handleAtomicType(AtomicType atomicType) {
            return this.representation;
        }

        @Override
        public JsonNode handleNumericType(NumericType numericType) {
            return this.representation;
        }

        @Override
        public JsonNode handleBoundedStringType(BoundedStringType boundedStringType) {
            return this.representation;
        }

        @Override
        public JsonNode handleUnboundedStringType(UnboundedStringType unboundedStringType) {
            return this.representation;
        }
    }

    protected abstract static class AbstractPublicToInternalRewriter extends AbstractRepresentationRewriter {

        public JsonNode rewritePublicToInternal(Type type, JsonNode representation) {
            this.representation = representation;
            return type.accept(this);
        }

        @Override
        public JsonNode handleEnumType(EnumType<?, ?, ?> enumType) {
            TextNode textNode = (TextNode) representation;
            String value = textNode.asText();

            EnumMember<?, ?> enumMember = enumType.resolveMember(value).orElse(null);
            if (enumMember != null) {
                return new TextNode(enumMember.getInternalName());
            } else {
                return this.onUnrepresentableEnumMember(value);
            }
        }

        protected abstract JsonNode onUnrepresentableEnumMember(String name);

        @Override
        public JsonNode handleAtomicType(AtomicType atomicType) {
            return this.representation;
        }

        @Override
        public JsonNode handleNumericType(NumericType numericType) {
            return this.representation;
        }        

        protected abstract AbstractPublicToInternalRewriter fork();

        private JsonNode handleListType(ListType listType) {
            if (this.representation.isNull()) {
                return this.representation;
            }

            AbstractPublicToInternalRewriter rewriter = this.fork();
            Type elementType = listType.getElementType();

            ArrayNode arrayNode = (ArrayNode) this.representation;
            ArrayNode rewrittenArrayNode = OBJECT_MAPPER.createArrayNode();

            for (JsonNode subNode : arrayNode) {
                JsonNode rewrittenSubNode = rewriter.rewritePublicToInternal(elementType, subNode);
                rewrittenArrayNode.add(rewrittenSubNode);
            }

            return rewrittenArrayNode;
        }

        @Override
        public JsonNode handleBoundedListType(BoundedListType boundedListType) {
            return this.handleListType(boundedListType);
        }

        @Override
        public JsonNode handleUnboundedListType(UnboundedListType unboundedListType) {
            return this.handleListType(unboundedListType);
        }

        @Override
        public JsonNode handleBoundedStringType(BoundedStringType boundedStringType) {
            return this.representation;
        }

        @Override
        public JsonNode handleUnboundedStringType(UnboundedStringType unboundedStringType) {
            return this.representation;
        }

    }

}
