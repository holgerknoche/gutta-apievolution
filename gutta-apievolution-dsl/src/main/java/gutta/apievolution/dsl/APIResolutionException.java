package gutta.apievolution.dsl;

import org.antlr.v4.runtime.Token;

/**
 * This exception denotes that an error has occurred during the resolution of an API definition.
 */
public class APIResolutionException extends RuntimeException {

    private static final long serialVersionUID = -2560140692689234150L;

    private final transient Token token;

    /**
     * Creates a new exception from the given data.
     * @param token The token at which the error occurred
     * @param message The error message
     */
    public APIResolutionException(final Token token, final String message) {
        super(message);

        this.token = token;
    }

    /**
     * Creates a new message from the given data.
     * @param token The token at which the error occurred
     * @param message The error message
     * @param cause The underlying cause for this exception
     */
    public APIResolutionException(final Token token, final String message, final Throwable cause) {
        super(message, cause);

        this.token = token;
    }

    /**
     * Checks whether this exception has occurred at the given position.
     * @param line The line to check this exception against
     * @param positionInLine The position in the line to check this exception against
     * @return {@code True}, iff this exception occurred at the given position
     */
    public boolean isAtPosition(final int line, final int positionInLine) {
        return (this.token.getLine() == line && this.token.getCharPositionInLine() == positionInLine);
    }

    @Override
    public String toString() {
        return this.token + ": " + this.getMessage();
    }

}
