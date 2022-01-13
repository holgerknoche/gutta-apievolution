package gutta.apievolution.json;

import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.node.ObjectNode;
import gutta.apievolution.core.apimodel.*;
import gutta.apievolution.core.apimodel.consumer.ConsumerApiDefinition;
import gutta.apievolution.core.apimodel.consumer.ConsumerField;
import gutta.apievolution.core.apimodel.provider.ProviderField;
import gutta.apievolution.core.apimodel.provider.RevisionHistory;
import gutta.apievolution.core.resolution.DefinitionResolution;
import gutta.apievolution.core.resolution.DefinitionResolver;
import gutta.apievolution.json.consumer.ConsumerParameter;
import gutta.apievolution.json.consumer.ConsumerResult;
import gutta.apievolution.json.consumer.TestProviderProxy;
import gutta.apievolution.json.provider.TestProviderServiceProxy;
import org.junit.jupiter.api.Disabled;
import org.junit.jupiter.api.Test;

import java.io.IOException;
import java.util.*;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNull;

class JsonMappingTest {

    @Test
    @Disabled
    void testJsonConversation() {
        TestRequestRouter requestRouter = new TestRequestRouter();

        TestProviderServiceProxy serviceProxy = new TestProviderServiceProxy();
        requestRouter.registerProviderService(serviceProxy);

        ConsumerParameter parameter = new ConsumerParameter();
        parameter.setTestField("test value");

        TestProviderProxy providerProxy = new TestProviderProxy(requestRouter);
        ConsumerResult result = providerProxy.invokeProviderMethod(parameter);

        System.out.println(result);
    }

//    @Test
//    void testJsonConversationOld() throws IOException {
//        RevisionHistory providerRevisionHistory = this.loadRevisionHistory("apis/provider-revision-1.api",
//                "apis/provider-revision-2.api");
//        ConsumerApiDefinition consumerApi = this.loadConsumerApi("apis/consumer-api.api", 0);
//
//        Set<Integer> supportedRevisions = new HashSet<>(Arrays.asList(0, 1));
//        DefinitionResolution definitionResolution = new DefinitionResolver().resolveConsumerDefinition(providerRevisionHistory, supportedRevisions, consumerApi);
//
//        ObjectMapper objectMapper = new ObjectMapper();
//
//        // Emulate request-response interaction
//        // Create parameter and convert it to (internal) JSON
//        ConsumerParameter consumerParameter = new ConsumerParameter();
//        consumerParameter.setTestField("test value");
//
//        JsonNode parameterNode = objectMapper.valueToTree(consumerParameter);
//
//        // Map internal JSON to external JSON
//        Type consumerType = definitionResolution.resolveConsumerType("ConsumerParameter");
//        JsonNode externalParameterNode = this.rewriteInternalToPublic(consumerType, parameterNode);
//
//        String requestJson = objectMapper.writeValueAsString(externalParameterNode);
//
//        Type providerParameterType = definitionResolution.mapConsumerType(consumerType);
//        JsonNode receivedRequestNode = objectMapper.readTree(requestJson);
//
//        JsonNode providerParameterNode = this.rewritePublicToProviderInternal(providerParameterType, definitionResolution, receivedRequestNode);
//        ProviderParameter providerParameter = objectMapper.treeToValue(providerParameterNode, ProviderParameter.class);
//
//        assertEquals("test value", providerParameter.getFieldA());
//        assertNull(providerParameter.getField2());
//
//        ProviderResult providerResult = new ProviderResult();
//        providerResult.setRetField("return value");
//
//        Type providerResultType = definitionResolution.resolveProviderType("TestResult");
//        JsonNode providerResultNode = objectMapper.valueToTree(providerResult);
//        JsonNode providerResponseNode = this.rewriteInternalToPublic(providerResultType, providerResultNode);
//
//        String responseJson = objectMapper.writeValueAsString(providerResponseNode);
//
//        Type clientResponseType = definitionResolution.resolveConsumerType("TestResult");
//        JsonNode receivedResponseNode = objectMapper.readTree(responseJson);
//        JsonNode consumerResponseNode = this.rewritePublicToConsumerInternal(clientResponseType, receivedResponseNode);
//
//        ConsumerResult consumerResult = objectMapper.treeToValue(consumerResponseNode, ConsumerResult.class);
//        assertEquals("return value", consumerResult.getResultField());
//    }

}
