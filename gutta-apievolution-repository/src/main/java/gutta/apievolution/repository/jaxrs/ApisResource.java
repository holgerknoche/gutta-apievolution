package gutta.apievolution.repository.jaxrs;

import gutta.apievolution.repository.ApisService;

import javax.inject.Inject;
import javax.ws.rs.Consumes;
import javax.ws.rs.POST;
import javax.ws.rs.Path;
import javax.ws.rs.PathParam;
import javax.ws.rs.core.Response;

@Path("apis")
public class ApisResource {

    @Inject
    ApisService apisService;

    @Path("{historyName}")
    @POST
    @Consumes("application/gutta-apidefinition")
    public Response saveApi(@PathParam("historyName") String historyName, String apiDefinition) {
        this.apisService.saveApiRevision(historyName, apiDefinition);

        return Response.ok().build();
    }

}
