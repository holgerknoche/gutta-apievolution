package gutta.apievolution.repository.jaxrs;

import gutta.apievolution.repository.ApiProcessingException;
import gutta.apievolution.repository.ApisService;

import javax.inject.Inject;
import javax.ws.rs.*;
import javax.ws.rs.core.Response;

@Path("apis")
public class ApisResource {

    @Inject
    ApisService apisService;

    @Path("{historyName}/{revisionNumber}")
    @GET
    @Produces("text/plain")
    public Response readApiRevision(@PathParam("historyName") String historyName, @PathParam("revisionNumber") int revisionNumber) {
        String apiDefinition = this.apisService.readApiRevision(historyName, revisionNumber);
        return Response.ok(apiDefinition).build();
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
