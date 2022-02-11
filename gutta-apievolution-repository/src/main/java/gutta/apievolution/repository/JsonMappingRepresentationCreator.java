package gutta.apievolution.repository;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.node.ArrayNode;
import com.fasterxml.jackson.databind.node.ObjectNode;
import gutta.apievolution.core.apimodel.*;
import gutta.apievolution.core.apimodel.consumer.ConsumerEnumMember;
import gutta.apievolution.core.apimodel.consumer.ConsumerField;
import gutta.apievolution.core.apimodel.provider.ProviderEnumMember;
import gutta.apievolution.core.apimodel.provider.ProviderField;
import gutta.apievolution.core.resolution.DefinitionResolution;

import java.io.IOException;
import java.util.stream.Stream;

/**
 * Representation creator for JSON representation mapping, represented as JSON.
 */
class JsonMappingRepresentationCreator implements ApiMappingRepresentationCreator {

    private static final ObjectMapper OBJECT_MAPPER = new ObjectMapper();

    private byte[] createPublicToInternalMapping(Stream<Type> types) {
        ObjectMapper objectMapper = OBJECT_MAPPER;

        ArrayNode rootNode = objectMapper.createArrayNode();
        PublicToInternalMapper mapper = new PublicToInternalMapper();
        types.forEach(type -> rootNode.add(type.accept(mapper)));

        try {
            return objectMapper.writeValueAsBytes(rootNode);
        } catch (IOException e) {
            throw new RuntimeException(e);
        }
    }

    @Override
    public byte[] createConsumerSideMapping(DefinitionResolution resolution) {
        return this.createPublicToInternalMapping(resolution.consumerTypes());
    }

    @Override
    public byte[] createProviderSideMapping(DefinitionResolution resolution) {
        return this.createPublicToInternalMapping(resolution.providerTypes());
    }

    @Override
    public byte[] createFullMapping(DefinitionResolution resolution) {
        ObjectMapper objectMapper = OBJECT_MAPPER;

        ArrayNode rootNode = objectMapper.createArrayNode();
        InternalToInternalMapper mapper = new InternalToInternalMapper(resolution);
        resolution.consumerTypes().forEach(type -> rootNode.add(type.accept(mapper)));

        try {
            return objectMapper.writeValueAsBytes(rootNode);
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
            RecordType<?, ?, ?> providerRecordType =
                    (RecordType<?, ?, ?>) this.resolution.mapConsumerType(consumerRecordType);

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
                ProviderEnumMember providerMember = this.resolution.mapConsumerEnumMember(
                        (ConsumerEnumMember) consumerMember);

                memberNode.put("providerName", providerMember.getInternalName());
                memberNode.put("consumerName", consumerMember.getInternalName());

                membersNode.add(memberNode);
            });

            node.set("members", membersNode);

            return node;
        }

    }

}
