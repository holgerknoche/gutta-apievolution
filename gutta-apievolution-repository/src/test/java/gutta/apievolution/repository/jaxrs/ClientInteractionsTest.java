package gutta.apievolution.repository.jaxrs;

import com.fasterxml.jackson.databind.node.ObjectNode;
import io.quarkus.test.junit.QuarkusTest;
import io.restassured.response.Response;
import org.junit.jupiter.api.Test;

import java.nio.charset.StandardCharsets;

import static io.restassured.RestAssured.given;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

/**
 * Test cases for client interactions.
 */
@QuarkusTest
class ClientInteractionsTest {

    /**
     * Test case: Publish a provider and a matching consumer API.
     */
    @Test
    void publishProviderAndConsumerApi() {
        final SimpleObjectMapper objectMapper = new ObjectMapperProvider().getObjectMapper();
        final String historyName = "interactionTest";

        // Publish provider revision
        String publishProviderRequest = "{\"definition\": \"api test { record A { string fieldA } }\"}";

        Response publishResponse = given().when().contentType("application/json").body(publishProviderRequest)
                .post("apis/provider/" + historyName)

                .then().statusCode(200).extract().response();

        byte[] publishResponseBytes = publishResponse.asByteArray();
        ObjectNode publishResponseNode = (ObjectNode) objectMapper.treeFromBytes(publishResponseBytes);
        int publishedRevision = publishResponseNode.get("revisionNumber").asInt();

        // Read provider revision
        Response readResponse = given().when().get("apis/provider/" + historyName + "/" + publishedRevision)

                .then().statusCode(200).extract().response();

        byte[] readResponseBytes = readResponse.asByteArray();
        ObjectNode readResponseNode = (ObjectNode) objectMapper.treeFromBytes(readResponseBytes);

        assertNotNull(readResponseNode.get("definition"));

        // Publish consumer definition
        String publishConsumerRequest = "{\"referencedHistoryName\": \"" + historyName +
                "\", \"referencedRevisionNumber\": " + publishedRevision +
                ", \"consumerName\": \"testConsumer\", \"definition\": \"api test { record A as B { string fieldA as fieldB } }\"}";
        Response publishConsumerResponse = given().when().contentType("application/json").body(publishConsumerRequest)
                .post("apis/consumer")

                .then().statusCode(200).extract().response();

        byte[] publishConsumerResponseBytes = publishConsumerResponse.asByteArray();
        ObjectNode consumerResponseNode = (ObjectNode) objectMapper.treeFromBytes(publishConsumerResponseBytes);
        int consumerRevisionId = consumerResponseNode.get("id").asInt();

        assertTrue(consumerRevisionId > 0);

        // Request consumer-to-public mapping
        Response consumerToPublicMappingResponse = given().when().get("apis/consumer/" + consumerRevisionId + "/map")

                .then().statusCode(200).extract().response();

        String expectedMapping = "[{\"$type\":\"record\",\"publicName\":\"A\",\"internalName\":\"B\",\"fields\":[{\"publicName\":\"fieldA\",\"internalName\":\"fieldB\"}]}]";
        String actualMapping = new String(consumerToPublicMappingResponse.asByteArray(), StandardCharsets.UTF_8);

        assertEquals(expectedMapping, actualMapping);
    }

}
