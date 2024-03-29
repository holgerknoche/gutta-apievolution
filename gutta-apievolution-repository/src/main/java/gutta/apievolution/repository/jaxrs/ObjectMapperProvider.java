package gutta.apievolution.repository.jaxrs;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.datatype.jdk8.Jdk8Module;
import com.fasterxml.jackson.datatype.jsr310.JavaTimeModule;
import com.fasterxml.jackson.module.paramnames.ParameterNamesModule;
import jakarta.enterprise.context.ApplicationScoped;
import jakarta.enterprise.inject.Produces;

/**
 * CDI provider for the simple object mapper for JSON processing.
 */
@ApplicationScoped
public class ObjectMapperProvider {

    private static final ObjectMapper OBJECT_MAPPER = createObjectMapper();

    private static ObjectMapper createObjectMapper() {
        ObjectMapper objectMapper = new ObjectMapper();

        objectMapper.registerModules(new ParameterNamesModule(), new Jdk8Module(), new JavaTimeModule());

        return objectMapper;
    }

    /**
     * CDI Producer method for creating a simple object mapper.
     * 
     * @return An object mapper instance
     */
    @Produces
    public SimpleObjectMapper getObjectMapper() {
        return new SimpleObjectMapper(OBJECT_MAPPER);
    }

}
