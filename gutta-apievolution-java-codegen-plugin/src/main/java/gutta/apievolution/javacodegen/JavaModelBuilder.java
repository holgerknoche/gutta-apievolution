package gutta.apievolution.javacodegen;

import gutta.apievolution.core.apimodel.NumericType;
import gutta.apievolution.core.apimodel.Type;
import gutta.apievolution.core.apimodel.TypeVisitor;
import gutta.apievolution.core.apimodel.UnboundedStringType;
import gutta.apievolution.core.apimodel.provider.*;

import java.util.Collection;
import java.util.HashMap;
import java.util.Map;

class JavaModelBuilder implements ProviderApiDefinitionElementVisitor<JavaType>, TypeVisitor<JavaType> {

    private final Map<Type, JavaUserDefinedType> knownClasses;

    public JavaModelBuilder() {
        this.knownClasses = new HashMap<>();
    }

    public Collection<JavaUserDefinedType> getJavaClasses() {
        return this.knownClasses.values();
    }

    public void processElement(ProviderApiDefinitionElement element) {
        element.accept(this);
    }

    private JavaType resolveType(Type type) {
        JavaType knownJavaType = this.knownClasses.get(type);
        if (knownJavaType != null) {
            return knownJavaType;
        }

        return type.accept(this);
    }

    @Override
    public JavaType handleNumericType(NumericType numericType) {
        return JavaStandardTypes.NUMERIC_TYPE;
    }

    @Override
    public JavaType handleUnboundedStringType(UnboundedStringType unboundedStringType) {
        return JavaStandardTypes.STRING_TYPE;
    }

    @Override
    public JavaType handleProviderRecordType(ProviderRecordType recordType) {
        String packageName = recordType.getOwner().getName().toString();
        JavaInterface targetClass = new JavaInterface(packageName, recordType.getInternalName());
        this.knownClasses.put(recordType, targetClass);

        for (ProviderField field : recordType.getDeclaredFields()) {
            JavaType fieldType = this.resolveType(field.getType());
            JavaProperty javaProperty = new JavaProperty(field.getInternalName(), fieldType);
            targetClass.addField(javaProperty);
        }

        return targetClass;
    }

    @Override
    public JavaType handleProviderEnumType(ProviderEnumType enumType) {
        // Otherwise, create and register a new one
        String packageName = enumType.getOwner().getName().toString();
        JavaEnum targetClass = new JavaEnum(packageName, enumType.getInternalName());
        this.knownClasses.put(enumType, targetClass);

        for (ProviderEnumMember member : enumType.getDeclaredMembers()) {
            JavaEnumMember javaMember = new JavaEnumMember(member.getInternalName());
            targetClass.addMember(javaMember);
        }

        return targetClass;
    }
}
