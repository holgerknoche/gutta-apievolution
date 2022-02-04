package gutta.apievolution.repository.jaxrs;

import gutta.apievolution.repository.ApiProcessingException;
import gutta.apievolution.repository.PersistentProviderApiDefinition;
import gutta.apievolution.repository.ProviderApisService;

import java.util.Optional;
import javax.enterprise.context.ApplicationScoped;
import javax.inject.Inject;
import javax.ws.rs.*;
import javax.ws.rs.core.Response;

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
     * @param historyName The history name of the desired definition
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
            ProviderApi result = this.convertProviderApi(apiDefinition.get());
            byte[] resultJson = this.objectMapper.toJsonBytes(result);
            return Response.ok(resultJson).build();
        } else {
            return Response.status(Response.Status.NOT_FOUND).build();
        }
    }

    private ProviderApi convertProviderApi(PersistentProviderApiDefinition input) {
        return new ProviderApi(
          input.getHistoryName(),
          input.getRevisionNumber(),
          input.getCommitTime(),
          input.getDefinitionText()
        );
    }

    /**
     * Saves a given provider API definition in the given history.
     * @param historyName The name of the revision history
     * @param apiDefinition The API definition to save
     * @return The HTTP response to the request
     */
    @Path("{historyName}")
    @POST
    @Consumes("text/plain")
    public Response saveProviderApi(@PathParam("historyName") String historyName, String apiDefinition) {
        try {
            this.apisService.saveApiRevision(historyName, apiDefinition);
            return Response.ok().build();
        } catch (ApiProcessingException e) {
            return Response.serverError().entity(e.getMessage()).build();
        }
    }

}
