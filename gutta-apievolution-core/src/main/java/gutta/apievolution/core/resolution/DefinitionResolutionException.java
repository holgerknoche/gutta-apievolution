package gutta.apievolution.core.resolution;

/**
 * Denotes that an error has occured while resolving a consumer definition against a provider definition.
 */
public class DefinitionResolutionException extends RuntimeException {

    DefinitionResolutionException(String message) {
        super(message);
    }

    DefinitionResolutionException(String message, Throwable cause) {
        super(message, cause);
    }

}
