package gutta.apievolution.repository;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.node.ArrayNode;
import com.fasterxml.jackson.databind.node.ObjectNode;
import gutta.apievolution.core.apimodel.Field;
import gutta.apievolution.core.apimodel.RecordType;
import gutta.apievolution.core.apimodel.TypeVisitor;
import gutta.apievolution.core.resolution.DefinitionResolution;

import java.io.IOException;

class JsonMappingRepresentationCreator implements ApiMappingRepresentationCreator {

    private static final ObjectMapper OBJECT_MAPPER = new ObjectMapper();

    @Override
    public byte[] createConsumerSideMapping(DefinitionResolution resolution) {
        ObjectMapper objectMapper = OBJECT_MAPPER;

        ArrayNode rootNode = objectMapper.createArrayNode();
        ConsumerSideMapper mapper = new ConsumerSideMapper();

        resolution.consumerTypes().forEach(type -> rootNode.add(type.accept(mapper)));

        try {
            return objectMapper.writeValueAsBytes(rootNode);
        } catch (IOException e) {
            throw new RuntimeException(e);
        }
    }

    @Override
    public byte[] createProviderSideMapping(DefinitionResolution resolution) {
        return new byte[0];
    }

    @Override
    public byte[] createFullMapping(DefinitionResolution resolution) {
        return new byte[0];
    }

    private static class ConsumerSideMapper implements TypeVisitor<ObjectNode> {

        @Override
        public ObjectNode handleRecordType(RecordType<?, ?, ?> recordType) {
            ObjectNode node = OBJECT_MAPPER.createObjectNode();

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
    }

}
