package gutta.apievolution.json;

import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.SerializationFeature;
import com.fasterxml.jackson.databind.node.ArrayNode;
import com.fasterxml.jackson.databind.node.BooleanNode;
import com.fasterxml.jackson.databind.node.NullNode;
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

/**
 * Abstract superclass for both consumer and provider operation proxies.
 * 
 * @param <P> The parameter type representation of the operation
 * @param <R> The result type representation of the operation
 */
public abstract class AbstractOperationProxy<P, R> {

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

    /**
     * Creates a new operation proxy using the given data.
     * 
     * @param operationName The name of the represented operation
     * @param parameterTypeName The name of the parameter type
     * @param resultTypeName The name of the result type
     */
    protected AbstractOperationProxy(String operationName, String parameterTypeName, String resultTypeName) {
        this.operationName = operationName;
        this.parameterTypeName = parameterTypeName;
        this.resultTypeName = resultTypeName;
    }

    /**
     * Returns the name of the represented operation.
     * 
     * @return see above
     */
    public String getOperationName() {
        return this.operationName;
    }

    /**
     * Returns the name of the parameter type.
     * 
     * @return see above
     */
    protected String getParameterTypeName() {
        return this.parameterTypeName;
    }

    /**
     * Returns the name of the result type.
     * 
     * @return see above
     */
    protected String getResultTypeName() {
        return this.resultTypeName;
    }

    /**
     * Determines the specific type ID of the given JSON node, if present.
     * 
     * @param node The JSON node to determine the type ID of
     * @return The type ID, if present
     */
    protected static Optional<String> determineSpecificTypeId(JsonNode node) {
        JsonNode typePropertyNode = node.get(TYPE_PROPERTY_NAME);

        if (typePropertyNode == null || !typePropertyNode.isTextual()) {
            return Optional.empty();
        }

        return Optional.of(typePropertyNode.asText());
    }

    /**
     * Determines whether the given JSON node represents an unrepresentable value.
     * 
     * @param node The node to inspect
     * @return {@code True} if the node represents an unrepresentable value, {@code false} otherwise
     */
    protected static boolean isUnrepresentableValue(ObjectNode node) {
        return node.has(UNREPRESENTABLE_PROPERTY_NAME);
    }

    /**
     * Creates a JSON node representing an unrepresentable value. 
     * 
     * @return see above
     */
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
            return (type.hasSuperTypes() || type.hasSubTypes() || type.isException());
        }
        
        /**
         * Handles the type identifier on the given object node, according to the given type. If the type
         * requires a type identifier, it is added, otherwise, it is removed.
         * 
         * @param node The JSON node to process
         * @param type The type determining whether or not an identifier is required
         * @param nameAccessor An function that provides the name of the type
         * @return {@code True} if an identifier is required, {@code false} otherwise
         */
        protected boolean handleTypeIdentifier(ObjectNode node, RecordType<?, ?, ?> type, Function<RecordType<?, ?, ?>, String> nameAccessor) {
            if (this.requiresTypeIdentifier(type)) {
                node.set(TYPE_PROPERTY_NAME, new TextNode(nameAccessor.apply(type)));
                return true;
            } else {
                node.remove(TYPE_PROPERTY_NAME);
                return false;
            }
        }
        
        @Override
        public JsonNode handleRecordType(RecordType<?, ?, ?> recordType) {
            if (this.representation.isNull()) {
                return NullNode.getInstance();
            }
            
            ObjectNode objectNode = (ObjectNode) this.representation;

            Optional<String> specificTypeId = determineSpecificTypeId(objectNode);
            if (specificTypeId.isPresent()) {
                // If a specific type ID is present, the actual type may be a subtype of the formal type
                return this.handlePolymorphicRecordType(specificTypeId.get(), recordType, objectNode);
            } else {
                // If no type ID is present, perform a monomorphic or mono-to-poly mapping
                return this.handleMonomorphicRecordType(recordType, objectNode);
            }
        }

        protected abstract JsonNode handlePolymorphicRecordType(String typeId, RecordType<?, ?, ?> formalType, ObjectNode objectNode);

        protected abstract JsonNode handleMonomorphicRecordType(RecordType<?, ?, ?> recordType, ObjectNode objectNode);
        
    }
    
    /**
     * Visitor implementation for rewriting an internal representation to a public representation.
     */
    protected abstract static class AbstractInternalToPublicRewriter extends AbstractRepresentationRewriter {

        /**
         * Rewrites the given JSON node from internal to public representation according to the given type.
         * 
         * @param type The type that represents the JSON node
         * @param representation The JSON representation of the type with internal names
         * @return The rewritten JSON node with public names
         */
        public JsonNode rewriteInternalToPublic(Type type, JsonNode representation) {
            this.representation = representation;
            return type.accept(this);
        }

        /**
         * Returns a fork of this rewriter. 
         * 
         * @return see above
         */
        protected abstract AbstractInternalToPublicRewriter fork();

        @Override
        public JsonNode handleEnumType(EnumType<?, ?, ?> enumType) {
            if (this.representation.isNull()) {
                return NullNode.getInstance();
            }
            
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

    /**
     * Abstract supertype for public-to-internal rewriters.
     */
    protected abstract static class AbstractPublicToInternalRewriter extends AbstractRepresentationRewriter {

        /**
         * Rewrites the given JSON node from public to internal representation according to the given type.
         * 
         * @param type The type that represents the JSON node
         * @param representation The JSON representation of the type with internal names
         * @return The rewritten JSON node with public names
         */
        public JsonNode rewritePublicToInternal(Type type, JsonNode representation) {
            this.representation = representation;
            return type.accept(this);
        }

        @Override
        public JsonNode handleEnumType(EnumType<?, ?, ?> enumType) {
            if (this.representation.isNull()) {
                return NullNode.getInstance();
            }
            
            TextNode textNode = (TextNode) this.representation;
            String value = textNode.asText();

            EnumMember<?, ?> enumMember = enumType.resolveMember(value).orElse(null);
            if (enumMember != null) {
                return new TextNode(enumMember.getInternalName());
            } else {
                return this.onUnrepresentableEnumMember(value);
            }
        }

        /**
         * Handles an unrepresentable enum member with the given name.
         * 
         * @param name The name of the unrepresentable enum member
         * @return The JSON object representing the unrepresentable member
         */
        protected abstract JsonNode onUnrepresentableEnumMember(String name);

        @Override
        public JsonNode handleAtomicType(AtomicType atomicType) {
            return this.representation;
        }

        @Override
        public JsonNode handleNumericType(NumericType numericType) {
            return this.representation;
        }        

        /**
         * Returns a fork of this rewriter. 
         * 
         * @return see above
         */
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
