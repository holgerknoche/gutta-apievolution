package gutta.apievolution.javacodegen;

class JavaListType implements JavaType {

    public final JavaType elementType;

    JavaListType(JavaType elementType) {
        this.elementType = elementType;
    }

    @Override
    public String getFullyQualifiedName() {
        return "java.util.List<" + this.elementType.getFullyQualifiedName() + ">";
    }
}
