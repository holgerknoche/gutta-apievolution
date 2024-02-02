package gutta.apievolution.core.validation;

import gutta.apievolution.core.util.EqualityUtil;

import static java.util.Objects.requireNonNull;

/**
 * A {@link ValidationMessage} represents a message that has come up during the validation of an element or model.
 */
public class ValidationMessage {

    private final Severity severity;

    private final String text;

    /**
     * Creates a new error message with the given text.
     * 
     * @param text The message text, may not be {@code null}
     * @return The validation message
     */
    public static ValidationMessage error(String text) {
        return new ValidationMessage(Severity.ERROR, text);
    }

    /**
     * Creates a new warning message with the given text.
     * 
     * @param text The message text, may not be {@code null}
     * @return The validation message
     */
    public static ValidationMessage warning(String text) {
        return new ValidationMessage(Severity.WARNING, text);
    }

    /**
     * Creates a new informational message with the given text.
     * 
     * @param text The message text, may not be {@code null}
     * @return The validation message
     */
    public static ValidationMessage info(String text) {
        return new ValidationMessage(Severity.INFO, text);
    }

    private ValidationMessage(Severity severity, String text) {
        this.severity = severity;
        this.text = requireNonNull(text);
    }

    /**
     * Returns the severity of this message.
     * 
     * @return see above
     */
    public Severity getSeverity() {
        return this.severity;
    }

    /**
     * Returns the message text of this message.
     * 
     * @return see above
     */
    public String getText() {
        return this.text;
    }

    @Override
    public int hashCode() {
        return this.getText().hashCode();
    }

    @Override
    public boolean equals(Object that) {
        return EqualityUtil.equals(this, that, this::equalsInternal);
    }

    private boolean equalsInternal(ValidationMessage that) {
        return (this.severity == that.severity) && this.text.equals(that.text);
    }

    @Override
    public String toString() {
        return "[" + this.severity + "] " + this.text;
    }

}
