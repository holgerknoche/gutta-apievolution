package gutta.apievolution.repository;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.node.ArrayNode;
import com.fasterxml.jackson.databind.node.ObjectNode;
import gutta.apievolution.core.apimodel.EnumType;
import gutta.apievolution.core.apimodel.Operation;
import gutta.apievolution.core.apimodel.RecordType;
import gutta.apievolution.core.apimodel.Type;
import gutta.apievolution.core.apimodel.TypeVisitor;
import gutta.apievolution.core.apimodel.consumer.ConsumerEnumMember;
import gutta.apievolution.core.apimodel.consumer.ConsumerField;
import gutta.apievolution.core.apimodel.consumer.ConsumerOperation;
import gutta.apievolution.core.apimodel.provider.ProviderEnumMember;
import gutta.apievolution.core.apimodel.provider.ProviderField;
import gutta.apievolution.core.apimodel.provider.ProviderOperation;
import gutta.apievolution.core.resolution.DefinitionResolution;

import java.io.IOException;
import java.util.stream.Stream;
import javax.ws.rs.core.MediaType;

/**
 * Representation creator for JSON representation mapping, represented as JSON.
 */
class JsonMappingRepresentationCreator implements ApiMappingRepresentationCreator {

    private static final String MEDIA_TYPE = MediaType.APPLICATION_JSON;
    
    private static final ObjectMapper OBJECT_MAPPER = new ObjectMapper();

    private byte[] createPublicToInternalMapping(Stream<Type> types, Stream<? extends Operation<?, ?, ?>> operations) {
        ObjectMapper objectMapper = OBJECT_MAPPER;

        ArrayNode rootNode = objectMapper.createArrayNode();
        PublicToInternalMapper mapper = new PublicToInternalMapper();
        types.forEach(type -> rootNode.add(type.accept(mapper)));
        operations.forEach(operation -> rootNode.add(mapper.handleOperation(operation)));

        try {
            return objectMapper.writeValueAsBytes(rootNode);
        } catch (IOException e) {
            throw new RuntimeException(e);
        }
    }

    private MappingRepresentation toRepresentation(byte[] data) {
        return new MappingRepresentation(MEDIA_TYPE, data);
    }
    
    @Override
    public MappingRepresentation createConsumerSideMapping(DefinitionResolution resolution) {
        return this.toRepresentation(
                this.createPublicToInternalMapping(resolution.consumerTypes(), resolution.consumerOperations())
               );
    }

    @Override
    public MappingRepresentation createProviderSideMapping(DefinitionResolution resolution) {
        return this.toRepresentation(
                this.createPublicToInternalMapping(resolution.providerTypes(), resolution.providerOperations())
               );
    }

    @Override
    public MappingRepresentation createFullMapping(DefinitionResolution resolution) {
        ObjectMapper objectMapper = OBJECT_MAPPER;

        ArrayNode rootNode = objectMapper.createArrayNode();
        InternalToInternalMapper mapper = new InternalToInternalMapper(resolution);
        resolution.consumerTypes().forEach(type -> rootNode.add(type.accept(mapper)));
        resolution.consumerOperations().forEach(operation -> rootNode.add(mapper.handleOperation(operation)));

        try {
            return this.toRepresentation(objectMapper.writeValueAsBytes(rootNode));
        } catch (IOException e) {
            throw new RuntimeException(e);
        }
    }

    /**
     * Mapper to create public-to-internal mappings for both consumer and provider.
     */
    private static class PublicToInternalMapper implements TypeVisitor<ObjectNode> {

        @Override
        public ObjectNode handleRecordType(RecordType<?, ?, ?> recordType) {
            ObjectNode node = OBJECT_MAPPER.createObjectNode();

            node.put("$type", "record");
            node.put("publicName", recordType.getPublicName());
            node.put("internalName", recordType.getInternalName());

            ArrayNode fieldsNode = OBJECT_MAPPER.createArrayNode();
            recordType.getFields().forEach(field -> {
                ObjectNode fieldNode = OBJECT_MAPPER.createObjectNode();
                fieldNode.put("publicName", field.getPublicName());
                fieldNode.put("internalName", field.getInternalName());

                fieldsNode.add(fieldNode);
            });

            node.set("fields", fieldsNode);

            return node;
        }

        @Override
        public ObjectNode handleEnumType(EnumType<?, ?, ?> enumType) {
            ObjectNode node = OBJECT_MAPPER.createObjectNode();

            node.put("$type", "enum");
            node.put("publicName", enumType.getPublicName());
            node.put("internalName", enumType.getInternalName());

            ArrayNode membersNode = OBJECT_MAPPER.createArrayNode();
            enumType.getDeclaredMembers().forEach(member -> {
                ObjectNode memberNode = OBJECT_MAPPER.createObjectNode();
                memberNode.put("publicName", member.getPublicName());
                memberNode.put("internalName", member.getInternalName());

                membersNode.add(memberNode);
            });

            node.set("members", membersNode);

            return node;
        }

        public ObjectNode handleOperation(Operation<?, ?, ?> operation) {
            ObjectNode node = OBJECT_MAPPER.createObjectNode();

            node.put("$type", "operation");
            node.put("publicName", operation.getPublicName());
            node.put("internalName", operation.getInternalName());

            return node;
        }

    }

    /**
     * Mapper to create a provider-internal-to-consumer-internal mapping.
     */
    private static class InternalToInternalMapper implements TypeVisitor<ObjectNode> {

        private final DefinitionResolution resolution;

        public InternalToInternalMapper(DefinitionResolution resolution) {
            this.resolution = resolution;
        }

        @Override
        public ObjectNode handleRecordType(RecordType<?, ?, ?> consumerRecordType) {
            RecordType<?, ?,
                    ?> providerRecordType = (RecordType<?, ?, ?>) this.resolution.mapConsumerType(consumerRecordType);

            ObjectNode node = OBJECT_MAPPER.createObjectNode();

            node.put("$type", "record");
            node.put("providerName", providerRecordType.getInternalName());
            node.put("consumerName", consumerRecordType.getInternalName());

            ArrayNode fieldsNode = OBJECT_MAPPER.createArrayNode();
            consumerRecordType.getFields().forEach(consumerField -> {
                ObjectNode fieldNode = OBJECT_MAPPER.createObjectNode();
                ProviderField providerField = this.resolution.mapConsumerField((ConsumerField) consumerField);

                fieldNode.put("providerName", providerField.getInternalName());
                fieldNode.put("consumerName", consumerField.getInternalName());

                fieldsNode.add(fieldNode);
            });

            node.set("fields", fieldsNode);

            return node;
        }

        @Override
        public ObjectNode handleEnumType(EnumType<?, ?, ?> consumerEnumType) {
            EnumType<?, ?, ?> providerEnumType = (EnumType<?, ?, ?>) this.resolution.mapConsumerType(consumerEnumType);

            ObjectNode node = OBJECT_MAPPER.createObjectNode();

            node.put("$type", "enum");
            node.put("providerName", providerEnumType.getInternalName());
            node.put("consumerName", consumerEnumType.getInternalName());

            ArrayNode membersNode = OBJECT_MAPPER.createArrayNode();
            consumerEnumType.getDeclaredMembers().forEach(consumerMember -> {
                ObjectNode memberNode = OBJECT_MAPPER.createObjectNode();
                ProviderEnumMember providerMember = this.resolution
                        .mapConsumerEnumMember((ConsumerEnumMember) consumerMember);

                memberNode.put("providerName", providerMember.getInternalName());
                memberNode.put("consumerName", consumerMember.getInternalName());

                membersNode.add(memberNode);
            });

            node.set("members", membersNode);

            return node;
        }

        public ObjectNode handleOperation(ConsumerOperation consumerOperation) {
            ProviderOperation providerOperation = this.resolution.mapConsumerOperation(consumerOperation);
            
            ObjectNode node = OBJECT_MAPPER.createObjectNode();
            
            node.put("$type", "operation");
            node.put("providerName", providerOperation.getInternalName());
            node.put("consumerName", consumerOperation.getInternalName());
            
            return node;
        }
        
    }

}
