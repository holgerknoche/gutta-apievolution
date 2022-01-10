package gutta.apievolution.json;

import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.node.ObjectNode;
import gutta.apievolution.core.apimodel.*;
import gutta.apievolution.core.apimodel.consumer.ConsumerApiDefinition;
import gutta.apievolution.core.apimodel.consumer.ConsumerField;
import gutta.apievolution.core.apimodel.provider.ProviderApiDefinition;
import gutta.apievolution.core.apimodel.provider.ProviderField;
import gutta.apievolution.core.apimodel.provider.RevisionHistory;
import gutta.apievolution.core.resolution.DefinitionResolution;
import gutta.apievolution.core.resolution.DefinitionResolver;
import gutta.apievolution.core.util.IntegerRange;
import gutta.apievolution.dsl.ConsumerApiLoader;
import gutta.apievolution.dsl.ProviderApiLoader;
import org.junit.jupiter.api.Test;

import java.io.IOException;
import java.io.InputStream;
import java.util.*;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNull;

class JsonMappingTest {

    private RevisionHistory loadRevisionHistory(String... fileNames) throws IOException {
        ClassLoader classLoader = this.getClass().getClassLoader();
        List<InputStream> streams = Stream.of(fileNames)
                .map(name -> classLoader.getResourceAsStream(name))
                .filter(Objects::nonNull)
                .collect(Collectors.toList());

        List<ProviderApiDefinition> apiDefinitions = ProviderApiLoader.loadHistoryFromStreams(IntegerRange.unbounded(),
                streams);
        RevisionHistory revisionHistory = new RevisionHistory(apiDefinitions);

        for (InputStream stream : streams) {
            stream.close();
        }

        return revisionHistory;
    }

    private ConsumerApiDefinition loadConsumerApi(String fileName, int referencedRevision) throws IOException {
        try (InputStream inputStream = this.getClass().getClassLoader().getResourceAsStream(fileName)) {
            return ConsumerApiLoader.loadFromStream(inputStream, referencedRevision);
        }
    }

    @Test
    void testJsonConversation() throws IOException {
        RevisionHistory providerRevisionHistory = this.loadRevisionHistory("apis/provider-revision-1.api",
                "apis/provider-revision-2.api");
        ConsumerApiDefinition consumerApi = this.loadConsumerApi("apis/consumer-api.api", 0);

        Set<Integer> supportedRevisions = new HashSet<>(Arrays.asList(0, 1));
        DefinitionResolution definitionResolution = new DefinitionResolver().resolveConsumerDefinition(providerRevisionHistory, supportedRevisions, consumerApi);

        ObjectMapper objectMapper = new ObjectMapper();

        // Emulate request-response interaction
        // Create parameter and convert it to (internal) JSON
        ConsumerParameter consumerParameter = new ConsumerParameter();
        consumerParameter.setTestField("test value");

        JsonNode parameterNode = objectMapper.valueToTree(consumerParameter);

        // Map internal JSON to external JSON
        Type consumerType = definitionResolution.resolveConsumerType("ConsumerParameter");
        JsonNode externalParameterNode = this.rewriteInternalToPublic(consumerType, parameterNode);

        String requestJson = objectMapper.writeValueAsString(externalParameterNode);

        Type providerParameterType = definitionResolution.mapConsumerType(consumerType);
        JsonNode receivedRequestNode = objectMapper.readTree(requestJson);

        JsonNode providerParameterNode = this.rewritePublicToProviderInternal(providerParameterType, definitionResolution, receivedRequestNode);
        ProviderParameter providerParameter = objectMapper.treeToValue(providerParameterNode, ProviderParameter.class);

        assertEquals("test value", providerParameter.getFieldA());
        assertNull(providerParameter.getField2());

        ProviderResult providerResult = new ProviderResult();
        providerResult.setRetField("return value");

        Type providerResultType = definitionResolution.resolveProviderType("TestResult");
        JsonNode providerResultNode = objectMapper.valueToTree(providerResult);
        JsonNode providerResponseNode = this.rewriteInternalToPublic(providerResultType, providerResultNode);

        String responseJson = objectMapper.writeValueAsString(providerResponseNode);

        Type clientResponseType = definitionResolution.resolveConsumerType("TestResult");
        JsonNode receivedResponseNode = objectMapper.readTree(responseJson);
        JsonNode consumerResponseNode = this.rewritePublicToConsumerInternal(clientResponseType, receivedResponseNode);

        ConsumerResult consumerResult = objectMapper.treeToValue(consumerResponseNode, ConsumerResult.class);
        assertEquals("return value", consumerResult.getResultField());
    }

    private JsonNode rewriteInternalToPublic(Type type, JsonNode representation) {
        if (type instanceof RecordType) {
            RecordType<?, ?, ?> recordType = (RecordType<?, ?, ?>) type;

            ObjectNode objectNode = (ObjectNode) representation;

            for (Field<?, ?> field : recordType.getDeclaredFields()) {
                JsonNode value = objectNode.remove(field.getInternalName());
                objectNode.set(field.getPublicName(), this.rewriteInternalToPublic(field.getType(), value));
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

    private JsonNode rewritePublicToProviderInternal(Type type, DefinitionResolution definitionResolution, JsonNode representation) {
        if (type instanceof RecordType) {
            RecordType<?, ?, ?> recordType = (RecordType<?, ?, ?>) type;

            ObjectNode objectNode = (ObjectNode) representation;

            for (Field<?, ?> field : recordType.getDeclaredFields()) {
                ConsumerField consumerField = definitionResolution.mapProviderField((ProviderField) field);
                if (consumerField == null) {
                    continue;
                }

                JsonNode value = objectNode.remove(consumerField.getPublicName());

                if (value != null) {
                    objectNode.set(field.getInternalName(), this.rewritePublicToProviderInternal(field.getType(), definitionResolution, value));
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

    private static class ConsumerParameter {

        private String testField;

        public String getTestField() {
            return this.testField;
        }

        public void setTestField(String testField) {
            this.testField = testField;
        }
    }

    private static class ConsumerResult {

        private String resultField;

        public String getResultField() {
            return this.resultField;
        }

        public void setResultField(String resultField) {
            this.resultField = resultField;
        }
    }

    private static class ProviderParameter {

        private String fieldA;

        private String field2;

        public String getFieldA() {
            return this.fieldA;
        }

        public void setFieldA(String fieldA) {
            this.fieldA = fieldA;
        }

        public String getField2() {
            return this.field2;
        }

        public void setField2(String field2) {
            this.field2 = field2;
        }

    }

    private static class ProviderResult {

        private String retField;

        public String getRetField() {
            return this.retField;
        }

        public void setRetField(String retField) {
            this.retField = retField;
        }

    }

}
