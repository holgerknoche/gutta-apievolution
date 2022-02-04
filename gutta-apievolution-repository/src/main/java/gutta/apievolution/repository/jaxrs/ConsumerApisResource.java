package gutta.apievolution.repository.jaxrs;

import javax.enterprise.context.ApplicationScoped;
import javax.ws.rs.GET;
import javax.ws.rs.Path;
import javax.ws.rs.core.Response;

/**
 * JAX-RS resource for consumer API definitions.
 */
@Path("apis/consumer/")
@ApplicationScoped
public class ConsumerApisResource {

    /**
     * Reads a given consumer definition.
     * @return The response for the request
     */
    @GET
    public Response readConsumerRevision() {
        return Response.ok().build();
    }

}
