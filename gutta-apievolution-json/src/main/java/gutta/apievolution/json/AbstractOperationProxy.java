package gutta.apievolution.json;

import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.SerializationFeature;
import com.fasterxml.jackson.databind.node.ArrayNode;
import com.fasterxml.jackson.databind.node.ObjectNode;
import com.fasterxml.jackson.databind.node.TextNode;
import gutta.apievolution.core.apimodel.*;

import java.util.NoSuchElementException;
import java.util.Optional;

abstract class AbstractOperationProxy<P, R> {    
    
    protected static final ObjectMapper OBJECT_MAPPER = createObjectMapper();
    
    private static final String TYPE_PROPERTY_NAME = "@type";
    
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

    @SuppressWarnings("unchecked")
    protected <T extends JsonNode> T rewriteInternalToPublic(Type type, JsonNode representation) {
        return (T) new InternalToPublicRewriter().rewriteInternalToPublic(type, representation);
    }

    protected Optional<String> determineSpecificTypeId(ObjectNode node) {
        JsonNode typePropertyNode = node.get(TYPE_PROPERTY_NAME);
        
        if (typePropertyNode == null || !typePropertyNode.isTextual()) {
            return Optional.empty();
        }
        
        return Optional.of(typePropertyNode.asText());
    }
    
    /**
     * Invokes the operation with the given parameter.
     * 
     * @param parameter The parameter to pass to the operation
     * @return The result of the operation
     */
    protected abstract R invokeOperation(P parameter);
    
    /**
     * Visitor implementation for rewriting an internal representation to a public representation.
     */
    static class InternalToPublicRewriter implements TypeVisitor<JsonNode> {

        JsonNode representation;

        public JsonNode rewriteInternalToPublic(Type type, JsonNode representation) {
            this.representation = representation;
            return type.accept(this);
        }

        InternalToPublicRewriter fork() {
            return new InternalToPublicRewriter();
        }

        @Override
        public JsonNode handleRecordType(RecordType<?, ?, ?> recordType) {
            ObjectNode objectNode = (ObjectNode) this.representation;            

            this.rewriteTypeIdentifier(objectNode, recordType);
            
            for (Field<?, ?> field : recordType.getDeclaredFields()) {
                JsonNode value = objectNode.remove(field.getInternalName());
                objectNode.set(field.getPublicName(), this.fork().rewriteInternalToPublic(field.getType(), value));
            }

            return objectNode;
        }
        
        private void rewriteTypeIdentifier(ObjectNode node, RecordType<?, ?, ?> type) {
            JsonNode typePropertyNode = node.get(TYPE_PROPERTY_NAME);
            
            if (typePropertyNode == null || !typePropertyNode.isTextual()) {
                return;
            }
            
            node.set(TYPE_PROPERTY_NAME, new TextNode(type.getPublicName()));
        }

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

            InternalToPublicRewriter rewriter = this.fork();
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

    protected abstract static class AbstractPublicToInternalRewriter implements TypeVisitor<JsonNode> {

        protected JsonNode representation;

        public JsonNode rewritePublicToInternal(Type type, JsonNode representation) {
            this.representation = representation;
            return type.accept(this);
        }

        @Override
        public JsonNode handleEnumType(EnumType<?, ?, ?> enumType) {
            TextNode textNode = (TextNode) representation;
            String value = textNode.asText();

            EnumMember<?, ?> enumMember = enumType.resolveMember(value).orElseThrow(NoSuchElementException::new);

            return new TextNode(enumMember.getInternalName());
        }

        @Override
        public JsonNode handleAtomicType(AtomicType atomicType) {
            return this.representation;
        }

        @Override
        public JsonNode handleNumericType(NumericType numericType) {
            return this.representation;
        }
        
        protected void rewriteTypeIdentifier(ObjectNode node, RecordType<?, ?, ?> type) {
            JsonNode typePropertyNode = node.get(TYPE_PROPERTY_NAME);
            
            if (typePropertyNode == null || !typePropertyNode.isTextual()) {
                return;
            }
        
            String typeIdentifier = typePropertyNode.asText();
            System.out.println(typeIdentifier);
            
            node.set(TYPE_PROPERTY_NAME, new TextNode(type.getInternalName()));
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
