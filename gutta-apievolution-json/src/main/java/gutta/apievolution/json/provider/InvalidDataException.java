package gutta.apievolution.json.provider;

/**
 * This exception is thrown when invalid (JSON) data is encountered.
 */
class InvalidDataException extends RuntimeException {    
    
    private static final long serialVersionUID = -2621792519634892079L;

    public InvalidDataException(String message, Throwable cause) {
        super(message, cause);
    }

}
