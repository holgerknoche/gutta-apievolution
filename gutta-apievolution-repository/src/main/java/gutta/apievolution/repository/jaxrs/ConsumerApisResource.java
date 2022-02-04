package gutta.apievolution.repository.jaxrs;

import gutta.apievolution.repository.ConsumerApisService;

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
     * @param id The ID of the desired API
     * @return The response for the request
     */
    @GET
    @Path("{id}")
    @Produces("application/json")
    public Response readConsumerApi(@PathParam("id") int id) {
        return Response.ok().build();
    }

    /**
     * Saves a given consumer API definition.
     * @param requestData The request data
     * @return The response to the request
     */
    @POST
    @Consumes("application/json")
    @Produces("application/json")
    public Response saveConsumerApi(byte[] requestData) {
        SaveConsumerApiRequest request = this.objectMapper.fromJsonBytes(requestData, SaveConsumerApiRequest.class);
        this.apisService.saveConsumerApi(
                request.referencedHistoryName,
                request.referencedRevisionNumber,
                request.consumerName,
                request.definition
        );

        return Response.ok().build();
    }

}
