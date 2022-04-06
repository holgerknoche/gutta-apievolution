package gutta.apievolution.dsl;

import org.antlr.v4.runtime.Token;

/**
 * This exception denotes that an error has occurred during the parsing of an
 * API definition.
 */
public class APIParseException
        extends RuntimeException {

    private static final long serialVersionUID = 4220185620253787830L;

    private static String createMessage(final Token token, final String message) {
        return token.getLine() + ":" + token.getCharPositionInLine() + ": " + message;
    }

    /**
     * Creates a new exception from the given data.
     *
     * @param location The token at which the error occurred
     * @param message  The error message
     */
    public APIParseException(final Token location, final String message) {
        super(createMessage(location, message));
    }

    /**
     * Creates a new exception from the given data.
     *
     * @param location The token at which the error occurred
     * @param message  The error message
     * @param cause    The underlying cause of this exception
     */
    public APIParseException(final Token location, final String message, final Throwable cause) {
        super(createMessage(location, message), cause);
    }

}
