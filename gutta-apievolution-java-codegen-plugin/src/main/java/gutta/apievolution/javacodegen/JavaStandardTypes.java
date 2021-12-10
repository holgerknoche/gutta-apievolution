package gutta.apievolution.javacodegen;

/**
 * Enumeration of Java standard types (i.e., Integer, Long, ...) for code generation.
 */
public enum JavaStandardTypes implements JavaType {
    INT_TYPE("Integer"),
    NUMERIC_TYPE("java.math.BigDecimal"),
    LONG_TYPE("Long"),
    STRING_TYPE("String");

    private final String fullyQualifiedName;

    JavaStandardTypes(String fullyQualifiedName) {
        this.fullyQualifiedName = fullyQualifiedName;
    }

    @Override
    public String getFullyQualifiedName() {
        return this.fullyQualifiedName;
    }
}
