package gutta.apievolution.core.validation;

import java.util.ArrayList;
import java.util.List;
import java.util.function.Function;

/**
 * This class represents the result of a validation of a structured element, e.g., an API model or a definition resolution.
 */
public class ValidationResult {

    private boolean hasError = false;

    private final List<ValidationMessage> messages = new ArrayList<>();

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
    public List<ValidationMessage> getMessages() {
        return this.messages;
    }

    /**
     * Adds an error message to this validation result.
     * 
     * @param message The error message to add
     */
    public void addErrorMessage(String message) {
        this.hasError = true;
        this.messages.add(ValidationMessage.error(message));
        this.lastErrorMessage = message;
    }

    /**
     * Adds a warning message to this validation result.
     * 
     * @param message The error message to add
     */
    public void addWarningMessage(String message) {
        this.messages.add(ValidationMessage.warning(message));
    }
    
    /**
     * Adds an informational message to this validation result.
     * 
     * @param message The message to add
     */
    public void addInfoMessage(String message) {
        this.messages.add(ValidationMessage.info(message));
    }

    /**
     * Joins this result with the given one. The result contains an error if any of
     * the joined results contained one, and the union of error messages.
     * 
     * @param other The result to join with
     * @return The joined result
     */
    public ValidationResult joinWith(ValidationResult other) {
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
