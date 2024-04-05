package gutta.apievolution.json.consumer;

import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.node.NullNode;

/**
 * Interface for actions that are executed if an unrepresentable value is encountered in the JSON.
 * 
 * @param <T> The type of the default JSON node to insert, if applicable
 */
public interface OnUnrepresentableValue<T extends JsonNode> {
    
    /**
     * Returns a null node if an unrepresentable value is encountered.
     * 
     * @return see above
     */
    public static OnUnrepresentableValue<NullNode> returnNull() {
        return NullNode::getInstance;
    }
    
    /**
     * Throws an {@link UnrepresentableValueException} if an unrepresentable value is encountered.
     * 
     * @return see above
     */
    public static OnUnrepresentableValue<JsonNode> throwException() {
        return () -> {
            throw new UnrepresentableValueException("An unrepresentable value was encountered.");
        };
    }
    
    /**
     * Throws an exception or returns a default node to replace an unrepresentable value. 
     * 
     * @return The default node if no (runtime) exception is thrown
     */
    public T throwExceptionOrReturnDefaultNode();

}
