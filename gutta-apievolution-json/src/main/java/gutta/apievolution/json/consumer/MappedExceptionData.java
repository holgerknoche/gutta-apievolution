package gutta.apievolution.json.consumer;

import com.fasterxml.jackson.annotation.JsonTypeInfo;
import com.fasterxml.jackson.annotation.JsonTypeInfo.Id;

/**
 * Interface for mapped exception data, which can be transformed into an actual exception.
 */
@JsonTypeInfo(use = Id.NAME)
public interface MappedExceptionData {
    
    /**
     * Creates the actual exception for the exception data.
     * 
     * @return see above
     */
    RuntimeException createMappedException();

}
