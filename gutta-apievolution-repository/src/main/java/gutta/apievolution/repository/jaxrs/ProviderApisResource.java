package gutta.apievolution.repository.jaxrs;

import com.fasterxml.jackson.databind.ObjectMapper;
import gutta.apievolution.repository.ApiProcessingException;
import gutta.apievolution.repository.PersistentProviderApiDefinition;
import gutta.apievolution.repository.ProviderApisService;

import javax.enterprise.context.ApplicationScoped;
import javax.inject.Inject;
import javax.ws.rs.*;
import javax.ws.rs.core.Response;
import java.io.IOException;
import java.util.Optional;
import java.util.concurrent.Callable;
import java.util.function.Function;

@Path("apis/provider/")
@ApplicationScoped
public class ProviderApisResource {

    @Inject
    SimpleObjectMapper objectMapper;

    @Inject
    ProviderApisService apisService;

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
