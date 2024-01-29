package gutta.apievolution.json;

import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.node.NullNode;

public interface OnUnrepresentableValue<T extends JsonNode> {
    
    static OnUnrepresentableValue<NullNode> returnNull() {
        return NullNode::getInstance;
    }
    
    static OnUnrepresentableValue<JsonNode> throwException() {
        return () -> {
            throw new UnrepresentableValueException("An unrepresentable value was encountered.");
        };
    }
    
    T throwExceptionOrReturnDefaultNode();

}
