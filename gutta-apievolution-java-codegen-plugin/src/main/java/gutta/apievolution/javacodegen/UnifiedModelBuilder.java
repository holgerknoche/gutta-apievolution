package gutta.apievolution.javacodegen;

import gutta.apievolution.core.apimodel.NumericType;
import gutta.apievolution.core.apimodel.Type;
import gutta.apievolution.core.apimodel.TypeVisitor;
import gutta.apievolution.core.apimodel.UnboundedStringType;
import gutta.apievolution.core.apimodel.provider.*;

import java.util.Collection;
import java.util.HashMap;
import java.util.Map;
import java.util.Optional;

class UnifiedModelBuilder implements ProviderApiDefinitionElementVisitor<JavaType>, TypeVisitor<JavaType> {

    private final Map<Type, JavaUserDefinedType> knownClasses;

    public UnifiedModelBuilder() {
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
        // See if there is already a Java type for any successor of this type
        Optional<ProviderRecordType> knownSuccessor =
                recordType.findFirstSuccessorMatching(type -> this.knownClasses.containsKey(type));

        JavaInterface targetClass;
        if (knownSuccessor.isPresent()) {
            // If there is a type for a successor, reuse this type
            targetClass = (JavaInterface) this.knownClasses.get(knownSuccessor);
        } else {
            // Otherwise, create and register a new one
            String packageName = recordType.getOwner().getName().toString();
            targetClass = new JavaInterface(packageName, recordType.getInternalName());
            this.knownClasses.put(recordType, targetClass);
        }

        for (ProviderField field : recordType.getDeclaredFields()) {
            JavaType fieldType = this.resolveType(field.getType());
            JavaProperty javaProperty = new JavaProperty(field.getInternalName(), fieldType);
            targetClass.addField(javaProperty);
        }

        return targetClass;
    }

    @Override
    public JavaType handleProviderEnumType(ProviderEnumType enumType) {
        // See if there is already a Java type for any successor of this type
        Optional<ProviderEnumType> knownSuccessor =
                enumType.findFirstSuccessorMatching(type -> this.knownClasses.containsKey(type));

        JavaEnum targetClass;
        if (knownSuccessor.isPresent()) {
            // If there is a type for a successor, reuse this type
            targetClass = (JavaEnum) this.knownClasses.get(knownSuccessor);
        } else {
            // Otherwise, create and register a new one
            String packageName = enumType.getOwner().getName().toString();
            targetClass = new JavaEnum(packageName, enumType.getInternalName());
            this.knownClasses.put(enumType, targetClass);
        }

        for (ProviderEnumMember member : enumType.getMembers()) {
            JavaEnumMember javaMember = new JavaEnumMember(member.getInternalName());
            targetClass.addMember(javaMember);
        }

        return targetClass;
    }
}
