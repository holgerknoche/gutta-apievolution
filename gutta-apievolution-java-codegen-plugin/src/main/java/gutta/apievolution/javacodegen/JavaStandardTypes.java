package gutta.apievolution.javacodegen;

public enum JavaStandardTypes implements JavaType {
    INT_TYPE("Integer"),
    NUMERIC_TYPE("java.math.BigDecimal"),
    LONG_TYPE("Long"),
    STRING_TYPE("String");

    private final String fullyQualifiedName;

    private JavaStandardTypes(String fullyQualifiedName) {
        this.fullyQualifiedName = fullyQualifiedName;
    }

    @Override
    public String getFullyQualifiedName() {
        return this.fullyQualifiedName;
    }
}
