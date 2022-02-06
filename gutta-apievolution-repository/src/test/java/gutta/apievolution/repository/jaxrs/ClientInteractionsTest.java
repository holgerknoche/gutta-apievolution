package gutta.apievolution.repository.jaxrs;

import io.quarkus.test.junit.QuarkusTest;
import io.restassured.response.Response;
import org.junit.jupiter.api.Test;

import static io.restassured.RestAssured.given;

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
        // Publish provider revision
        String publishProviderRequest = "{\"definition\": \"api test { record A { string fieldA } }\"}";

        given()
                .when()
                .contentType("application/json")
                .body(publishProviderRequest)
                .post("apis/provider/test")

                .then()
                .statusCode(200);

        Response response = given()
                .when()
                .get("apis/provider/test/0")

                .then()
                .statusCode(200)
                .extract()
                .response();

        String result = response.asString();
        System.out.println(result);
    }

}
