package gutta.apievolution.repository.jaxrs;

import gutta.apievolution.repository.ApiProcessingException;
import gutta.apievolution.repository.PersistentProviderApiDefinition;
import gutta.apievolution.repository.ProviderApisService;
import jakarta.enterprise.context.ApplicationScoped;
import jakarta.inject.Inject;
import jakarta.ws.rs.Consumes;
import jakarta.ws.rs.GET;
import jakarta.ws.rs.POST;
import jakarta.ws.rs.Path;
import jakarta.ws.rs.PathParam;
import jakarta.ws.rs.Produces;
import jakarta.ws.rs.core.Response;

import java.util.Optional;

/**
 * JAX-RS resource for managing provider API definitions.
 */
@Path("apis/provider/")
@ApplicationScoped
public class ProviderApisResource {

    @Inject
    SimpleObjectMapper objectMapper;

    @Inject
    ProviderApisService apisService;

    /**
     * Reads a provider API definition given its history name and revision number.
     * 
     * @param historyName    The history name of the desired definition
     * @param revisionNumber The revision number of the desired definition
     * @return The HTTP response to the request
     */
    @Path("{historyName}/{revisionNumber}")
    @GET
    @Produces("application/json")
    public Response readProviderApiRevision(@PathParam("historyName") String historyName,
            @PathParam("revisionNumber") int revisionNumber) {
        Optional<PersistentProviderApiDefinition> apiDefinition = this.apisService.readApiRevision(historyName,
                revisionNumber);

        if (apiDefinition.isPresent()) {
            ReadProviderApiResponse result = this.convertProviderApi(apiDefinition.get());
            byte[] resultJson = this.objectMapper.toJsonBytes(result);
            return Response.ok(resultJson).build();
        } else {
            return Response.status(Response.Status.NOT_FOUND).build();
        }
    }

    private ReadProviderApiResponse convertProviderApi(PersistentProviderApiDefinition input) {
        return new ReadProviderApiResponse(input.getHistoryName(), input.getRevisionNumber(), input.getCommitTime(),
                input.getSupportedFrom(), input.getSupportedUntil(), input.getDefinitionText());
    }

    /**
     * Reads the supported revisions for the given history.
     * 
     * @param historyName The name of the history
     * @return The response for the request
     */
    @Path("{historyName}/supportedRevisions")
    @GET
    @Produces("application/json")
    public Response readSupportedRevisions(@PathParam("historyName") String historyName) {
        return Response.ok().build();
    }

    /**
     * Saves a given provider API definition in the given history.
     * 
     * @param historyName The name of the revision history
     * @param requestData The request data
     * @return The HTTP response to the request
     */
    @Path("{historyName}")
    @POST
    @Consumes("application/json")
    @Produces("application/json")
    public Response saveProviderApi(@PathParam("historyName") String historyName, byte[] requestData) {
        try {
            SaveProviderApiRequest request = this.objectMapper.fromJsonBytes(requestData, SaveProviderApiRequest.class);

            int revisionNumber = this.apisService.saveApiRevision(historyName, request.supportedFrom,
                    request.supportedUntil, request.definition);

            SaveProviderApiResponse result = new SaveProviderApiResponse(revisionNumber);
            byte[] resultJson = this.objectMapper.toJsonBytes(result);
            return Response.ok(resultJson).build();
        } catch (JsonException | ApiProcessingException e) {
            return Response.status(Response.Status.BAD_REQUEST).entity(e.getMessage()).build();
        }
    }

}
