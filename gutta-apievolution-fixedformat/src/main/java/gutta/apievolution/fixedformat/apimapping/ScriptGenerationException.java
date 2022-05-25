package gutta.apievolution.fixedformat.apimapping;

/**
 * This exception denotes that some error has occurred during the mapping script generation.
 */
public class ScriptGenerationException extends RuntimeException {

    private static final long serialVersionUID = 6623343620191156049L;

    ScriptGenerationException(String message) {
        super(message);
    }
    
}
