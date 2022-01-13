package gutta.apievolution.json;

import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.node.ObjectNode;
import gutta.apievolution.core.apimodel.*;

abstract class AbstractInvocationProxy {

    protected static final ObjectMapper OBJECT_MAPPER = new ObjectMapper();

    protected JsonNode rewriteInternalToPublic(Type type, JsonNode representation) {
        if (type instanceof RecordType) {
            RecordType<?, ?, ?> recordType = (RecordType<?, ?, ?>) type;

            ObjectNode objectNode = (ObjectNode) representation;

            for (Field<?, ?> field : recordType.getDeclaredFields()) {
                JsonNode value = objectNode.remove(field.getInternalName());
                objectNode.set(field.getPublicName(), this.rewriteInternalToPublic(field.getType(), value));
            }

            return objectNode;
        } else if (type instanceof EnumType) {
            // TODO
            return representation;
        } else if (type instanceof ListType) {
            // TODO
            return representation;
        } else {
            return representation;
        }
    }

}
