package gutta.apievolution.javacodegen;

import gutta.apievolution.core.apimodel.*;
import gutta.apievolution.core.apimodel.provider.*;

import java.util.*;
import java.util.stream.Collectors;

class JavaModelBuilder {

    public JavaModel buildModelForApi(ProviderApiDefinition apiDefinition) {
        ModelBuildingPass1 pass1 = new ModelBuildingPass1();
        Map<Type, JavaUserDefinedType> initialTypeMap = pass1.createRecordTypes(apiDefinition);

        ModelBuildingPass2 pass2 = new ModelBuildingPass2(initialTypeMap);
        Collection<JavaUserDefinedType> udts = pass2.buildModel(apiDefinition);

        ModelBuildingPass3 pass3 = new ModelBuildingPass3(initialTypeMap);
        List<JavaService> services = pass3.buildServices(apiDefinition);

        return new JavaModel(udts, services);
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

            JavaRecordLikeType targetClass;
            if (recordType.isException()) {
                targetClass = new JavaException(packageName, recordType.getInternalName());
            } else {
                targetClass = new JavaInterface(packageName, recordType.getInternalName());
            }

            this.recordTypes.put(recordType, targetClass);

            return null;
        }

    }

    private static class ModelBuildingPass2
            implements ProviderApiDefinitionElementVisitor<JavaType>, TypeVisitor<JavaType> {

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
        public JavaType handleAtomicType(AtomicType atomicType) {
            switch (atomicType) {
            case INT_32:
                return JavaStandardTypes.INT_TYPE;

            case INT_64:
                return JavaStandardTypes.LONG_TYPE;

            default:
                throw new IllegalArgumentException();
            }
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
            JavaRecordLikeType targetClass = (JavaRecordLikeType) this.knownClasses.get(recordType);

            // TODO Support multiple supertypes for non-exceptions
            if (recordType.getSuperTypes().size() > 1) {
                throw new UnsupportedOperationException();
            }
            
            if (!recordType.getSuperTypes().isEmpty()) {
                ProviderRecordType superType = recordType.getSuperTypes().iterator().next();
                if (recordType.isException()) {
                    JavaException superException = (JavaException) this.knownClasses.get(superType);
                    ((JavaException) targetClass).setSuperType(superException);
                } else {
                    JavaInterface superInterface = (JavaInterface) this.knownClasses.get(superType);
                    ((JavaInterface) targetClass).setSuperType(superInterface);
                }
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

    private static class ModelBuildingPass3 implements ProviderApiDefinitionElementVisitor<Void> {

        private static final String DEFAULT_SERVICE_NAME = "Api";

        private static final String SERVICE_NAME_ANNOTATION = "ServiceName";

        private final Map<Type, JavaUserDefinedType> knownClasses;

        private Map<String, JavaService> services;

        private String packageName;

        public ModelBuildingPass3(Map<Type, JavaUserDefinedType> knownClasses) {
            this.knownClasses = knownClasses;
        }

        public List<JavaService> buildServices(ProviderApiDefinition apiDefinition) {
            this.services = new HashMap<>();
            this.packageName = apiDefinition.getName().toString();

            apiDefinition.forEach(element -> element.accept(this));
            return new ArrayList<>(this.services.values());
        }

        @Override
        public Void handleProviderOperation(ProviderOperation operation) {
            String name = operation.getInternalName();
            JavaInterface resultType = (JavaInterface) this.knownClasses.get(operation.getReturnType());
            JavaInterface parameterType = (JavaInterface) this.knownClasses.get(operation.getParameterType());

            List<JavaException> thrownExceptions = operation.getThrownExceptions().stream()
                    .map(exception -> (JavaException) this.knownClasses.get(exception)).collect(Collectors.toList());

            Optional<Annotation> serviceAnnotation = operation.getAnnotation(SERVICE_NAME_ANNOTATION);
            String serviceName = (serviceAnnotation.isPresent()) ? serviceAnnotation.get().getValue() :
                    DEFAULT_SERVICE_NAME;

            JavaService javaService = this.services.computeIfAbsent(serviceName,
                    svcName -> new JavaService(this.packageName, svcName));
            JavaServiceOperation javaOperation = new JavaServiceOperation(name, resultType, parameterType,
                    thrownExceptions);

            javaService.addOperation(javaOperation);

            return null;
        }

    }

}
