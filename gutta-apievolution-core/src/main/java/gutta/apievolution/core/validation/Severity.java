package gutta.apievolution.core.validation;

/**
 * Enumeration of the possible severities of a {@link ValidationMessage}.
 */
public enum Severity {
    /**
     * The message refers to an error, i.e., the validation has failed.
     */
    ERROR,
    /**
     * The message refers to a warning, i.e., an issue that should be looked into.
     */
    WARNING,
    /**
     * The message is only for informational purposes.
     */
    INFO
}
