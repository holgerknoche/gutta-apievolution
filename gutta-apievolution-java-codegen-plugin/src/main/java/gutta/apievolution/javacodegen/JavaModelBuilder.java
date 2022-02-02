package gutta.apievolution.javacodegen;

import gutta.apievolution.core.apimodel.NumericType;
import gutta.apievolution.core.apimodel.Type;
import gutta.apievolution.core.apimodel.TypeVisitor;
import gutta.apievolution.core.apimodel.UnboundedStringType;
import gutta.apievolution.core.apimodel.provider.*;

import java.util.Collection;
import java.util.HashMap;
import java.util.Map;

class JavaModelBuilder {

    public Collection<JavaUserDefinedType> buildModelForApi(ProviderApiDefinition apiDefinition) {
        ModelBuildingPass1 pass1 = new ModelBuildingPass1();
        Map<Type, JavaUserDefinedType> initialTypeMap = pass1.createRecordTypes(apiDefinition);

        ModelBuildingPass2 pass2 = new ModelBuildingPass2(initialTypeMap);
        return pass2.buildModel(apiDefinition);
    }

    private static class ModelBuildingPass1 implements ProviderApiDefinitionElementVisitor<Void> {

        private final Map<Type, JavaUserDefinedType> recordTypes = new HashMap<>();

        public Map<Type, JavaUserDefinedType> createRecordTypes(ProviderApiDefinition apiDefinition) {
            apiDefinition.forEach(element -> element.accept(this));
            return this.recordTypes;
        }

        @Override
        public Void handleProviderRecordType(ProviderRecordType recordType) {
            String packageName = recordType.getOwner().getName().toString();
            JavaInterface targetClass = new JavaInterface(packageName, recordType.getInternalName());
            this.recordTypes.put(recordType, targetClass);

            return null;
        }

    }

    private static class ModelBuildingPass2 implements ProviderApiDefinitionElementVisitor<JavaType>,
            TypeVisitor<JavaType> {

        private final Map<Type, JavaUserDefinedType> knownClasses;

        public ModelBuildingPass2(Map<Type, JavaUserDefinedType> initialTypeMap) {
            this.knownClasses = initialTypeMap;
        }

        public Collection<JavaUserDefinedType> buildModel(ProviderApiDefinition apiDefinition) {
            apiDefinition.forEach(element -> element.accept(this));
            return knownClasses.values();
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
            JavaInterface targetClass = (JavaInterface) this.knownClasses.get(recordType);

            recordType.getSuperType().ifPresent(superType -> {
                JavaInterface superInterface = (JavaInterface) this.knownClasses.get(superType);
                targetClass.setSuperType(superInterface);
            });

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

}
