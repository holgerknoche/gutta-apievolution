package gutta.apievolution.repository.jaxrs;

import gutta.apievolution.repository.ApiProcessingException;
import gutta.apievolution.repository.ProviderApisService;

import javax.inject.Inject;
import javax.ws.rs.*;
import javax.ws.rs.core.Response;
import java.util.Optional;

@Path("apis/provider/")
public class ProviderApisResource {

    @Inject
    ProviderApisService apisService;

    @Path("{historyName}/{revisionNumber}")
    @GET
    @Produces("text/plain")
    public Response readApiRevision(@PathParam("historyName") String historyName, @PathParam("revisionNumber") int revisionNumber) {
        Optional<String> apiDefinition = this.apisService.readApiRevision(historyName, revisionNumber);

        if (apiDefinition.isPresent()) {
            String definitionString = apiDefinition.get();
            return Response.ok(definitionString).build();
        } else {
            return Response.status(Response.Status.NOT_FOUND).build();
        }
    }

    @Path("{historyName}")
    @POST
    @Consumes("text/plain")
    public Response saveApi(@PathParam("historyName") String historyName, String apiDefinition) {
        try {
            this.apisService.saveApiRevision(historyName, apiDefinition);
            return Response.ok().build();
        } catch (ApiProcessingException e) {
            return Response.serverError().entity(e.getMessage()).build();
        }
    }

}
