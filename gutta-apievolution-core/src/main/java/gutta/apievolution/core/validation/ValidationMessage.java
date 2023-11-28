package gutta.apievolution.core.validation;

import static java.util.Objects.*;

public class ValidationMessage {

    private final Severity severity;
    
    private final String text;
    
    public static ValidationMessage error(String text) {
        return new ValidationMessage(Severity.ERROR, text);
    }
    
    public static ValidationMessage warning(String text) {
        return new ValidationMessage(Severity.WARNING, text);
    }
    
    public static ValidationMessage info(String text) {
        return new ValidationMessage(Severity.INFO, text);
    }
    
    private ValidationMessage(Severity severity, String text) {
        this.severity = severity;
        this.text = requireNonNull(text);
    }
    
    public Severity getSeverity() {
        return this.severity;
    }
    
    public String getText() {
        return this.text;
    }
    
    @Override
    public int hashCode() {
        return this.getText().hashCode();
    }
    
    public boolean equals(Object that) {
        if (this == that) {
            return true;
        } else if (this.getClass() == that.getClass()) {
            return this.equals((ValidationMessage) that);            
        } else {
            return false;
        }
    }
    
    private boolean equals(ValidationMessage that) {
        return (this.severity == that.severity) &&
                this.text.equals(that.text);
    }
    
    @Override
    public String toString() {
        return "[" + this.severity + "] " + this.text;
    }
    
}
