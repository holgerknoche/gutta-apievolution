package gutta.apievolution.core.util;

import java.util.ArrayList;
import java.util.List;
import java.util.function.Function;

/**
 * This class represents the result of a consistency check.
 */
public class CheckResult {

    private boolean hasError = false;

    private final List<String> messages = new ArrayList<>();

    private String lastErrorMessage;
    
    /**
     * Denotes whether the check resulted in an error.
     * 
     * @return see above
     */
    public boolean hasError() {
        return this.hasError;
    }

    /**
     * Returns messages that were issued during the check.
     * 
     * @return see above
     */
    public List<String> getMessages() {
        return this.messages;
    }

    /**
     * Adds an error message to this check result.
     * 
     * @param message The error message to add
     */
    public void addErrorMessage(String message) {
        this.hasError = true;
        this.messages.add(message);
        this.lastErrorMessage = message;
    }

    /**
     * Adds a message to this check result.
     * 
     * @param message The message to add
     */
    public void addMessage(String message) {
        this.messages.add(message);
    }

    /**
     * Joins this result with the given one. The result contains an error if any of
     * the joined results contained one, and the union of error messages.
     * 
     * @param other The result to join with
     * @return The joined result
     */
    public CheckResult joinWith(CheckResult other) {
        this.hasError = (this.hasError() || other.hasError());
        this.messages.addAll(other.getMessages());
        
        if (other.lastErrorMessage != null) {
            this.lastErrorMessage = other.lastErrorMessage;
        }

        return this;
    }
    
    /**
     * Throws an exception created using the given creator if an error exists in this check result.
     * The exception is created using the last error message.
     * @param <E> The type of exception to create
     * @param exceptionCreator The exception creator to use
     * @throws E The created exception
     */
    public <E extends Exception> void throwOnError(Function<String, E> exceptionCreator) throws E {
        if (this.hasError) {        
            E exception = exceptionCreator.apply(this.lastErrorMessage);
            throw exception;
        }
    }

}
