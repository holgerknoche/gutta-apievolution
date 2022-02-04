package gutta.apievolution.repository.jaxrs;

import com.fasterxml.jackson.databind.ObjectMapper;

import java.io.IOException;

class SimpleObjectMapper {

    private final ObjectMapper objectMapper;

    public SimpleObjectMapper(ObjectMapper objectMapper) {
        this.objectMapper = objectMapper;
    }

    public <T> T fromJsonBytes(byte[] jsonBytes, Class<T> targetType) {
        try {
            return this.objectMapper.readValue(jsonBytes, targetType);
        } catch (IOException e) {
            throw new JsonException(e);
        }
    }

    public byte[] toJsonBytes(Object object) {
        try {
            return this.objectMapper.writeValueAsBytes(object);
        } catch (IOException e) {
            throw new JsonException(e);
        }
    }

}
