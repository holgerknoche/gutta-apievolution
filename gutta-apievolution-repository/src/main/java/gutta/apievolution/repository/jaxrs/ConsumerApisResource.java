package gutta.apievolution.repository.jaxrs;

import javax.enterprise.context.ApplicationScoped;
import javax.ws.rs.GET;
import javax.ws.rs.Path;
import javax.ws.rs.core.Response;

@Path("apis/consumer/")
@ApplicationScoped
public class ConsumerApisResource {

    @GET
    public Response readConsumerRevision() {
        return Response.ok().build();
    }

}
