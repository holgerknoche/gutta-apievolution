package gutta.apievolution.repository.jaxrs;

import gutta.apievolution.repository.ApiMappingType;
import gutta.apievolution.repository.ApiProcessingException;
import gutta.apievolution.repository.ConsumerApisService;
import gutta.apievolution.repository.PersistentConsumerApiDefinition;

import java.util.Optional;
import javax.enterprise.context.ApplicationScoped;
import javax.inject.Inject;
import javax.ws.rs.*;
import javax.ws.rs.core.Response;

/**
 * JAX-RS resource for consumer API definitions.
 */
@Path("apis/consumer/")
@ApplicationScoped
public class ConsumerApisResource {

    @Inject
    SimpleObjectMapper objectMapper;

    @Inject
    ConsumerApisService apisService;

    /**
     * Reads a given consumer API definition.
     * 
     * @param id The ID of the desired API
     * @return The response for the request
     */
    @GET
    @Path("{id}")
    @Produces("application/json")
    public Response readConsumerApi(@PathParam("id") int id) {
        Optional<PersistentConsumerApiDefinition> optionalResult = this.apisService.readConsumerApi(id);
        if (optionalResult.isPresent()) {
            PersistentConsumerApiDefinition result = optionalResult.get();
            ReadConsumerApiResponse response = this.createResponse(result);

            byte[] jsonBytes = this.objectMapper.toJsonBytes(response);
            return Response.ok(jsonBytes).build();
        } else {
            return Response.status(Response.Status.NOT_FOUND).build();
        }
    }

    private ReadConsumerApiResponse createResponse(PersistentConsumerApiDefinition definition) {
        return new ReadConsumerApiResponse(definition.getId(), definition.getCommitTime(), definition.getConsumerName(),
                definition.getReferencedRevision().getHistoryName(),
                definition.getReferencedRevision().getRevisionNumber(), definition.getDefinitionText());
    }

    /**
     * Creates a mapping for a consumer API definition.
     * 
     * @param id   The id of the consumer API definition to map
     * @param type The type of the desired map
     * @return The response for the request
     */
    @GET
    @Path("{id}/map")
    @Produces("application/json")
    public Response mapConsumerApi(@PathParam("id") int id, @QueryParam("type") String type) {
        ApiMappingType mappingType = this.convertMappingType(type);

        byte[] mappingBytes = this.apisService.mapConsumerApi(id, "json", mappingType);
        return Response.ok(mappingBytes).build();
    }

    private ApiMappingType convertMappingType(String type) {
        String actualType = (type == null) ? "" : type.toLowerCase();

        switch (actualType) {
        case "consumer":
        default:
            return ApiMappingType.CONSUMER;

        case "provider":
            return ApiMappingType.PROVIDER;

        case "full":
            return ApiMappingType.FULL;
        }
    }

    /**
     * Saves a given consumer API definition.
     * 
     * @param requestData The request data
     * @return The response to the request
     */
    @POST
    @Consumes("application/json")
    @Produces("application/json")
    public Response saveConsumerApi(byte[] requestData) {
        try {
            SaveConsumerApiRequest request = this.objectMapper.fromJsonBytes(requestData, SaveConsumerApiRequest.class);

            PersistentConsumerApiDefinition savedApi = this.apisService.saveConsumerApi(request.referencedHistoryName,
                    request.referencedRevisionNumber, request.consumerName, request.definition);

            SaveConsumerApiResponse response = new SaveConsumerApiResponse(savedApi.getId(), savedApi.getCommitTime());

            byte[] responseData = this.objectMapper.toJsonBytes(response);
            return Response.ok(responseData).build();
        } catch (JsonException | ApiProcessingException e) {
            return Response.status(Response.Status.BAD_REQUEST).entity(e.getMessage()).build();
        }
    }

}
