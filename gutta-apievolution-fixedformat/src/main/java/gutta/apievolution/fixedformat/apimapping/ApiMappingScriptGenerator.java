package gutta.apievolution.fixedformat.apimapping;

import gutta.apievolution.core.apimodel.AtomicType;
import gutta.apievolution.core.apimodel.BoundedListType;
import gutta.apievolution.core.apimodel.BoundedStringType;
import gutta.apievolution.core.apimodel.EnumMember;
import gutta.apievolution.core.apimodel.EnumType;
import gutta.apievolution.core.apimodel.Field;
import gutta.apievolution.core.apimodel.RecordType;
import gutta.apievolution.core.apimodel.Type;
import gutta.apievolution.core.apimodel.TypeVisitor;
import gutta.apievolution.core.apimodel.UserDefinedType;
import gutta.apievolution.core.apimodel.consumer.ConsumerEnumMember;
import gutta.apievolution.core.apimodel.provider.ProviderField;
import gutta.apievolution.core.resolution.DefinitionResolution;

import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.NoSuchElementException;
import java.util.Optional;
import java.util.function.Function;
import java.util.stream.Collectors;
import java.util.stream.Stream;

public class ApiMappingScriptGenerator {

    private static final int INT32_SIZE = 4;
    
    private static final int INT64_SIZE = 8;
    
    private static final int ENUM_SIZE = 4;
    
    private static final int NUMBER_OF_ELEMENTS_INDICATOR_SIZE = 4;
    
    private static final int DISCRIMINATOR_SIZE = 4;
    
    public ApiMappingScript generateMappingScript(DefinitionResolution resolution, MappingDirection direction) {
        // First, calculate the sizes and offsets of all relevant types for both consumer and provider
        Map<Type, TypeInfo<?>> consumerTypeInfo = this.createTypeInfos(resolution.consumerTypes());
        Map<Type, TypeInfo<?>> providerTypeInfo = this.createTypeInfos(resolution.providerTypes());
        
        Map<Type, TypeInfo<?>> sourceInfo;
        Map<Type, TypeInfo<?>> targetInfo;
        
        // TODO Then, use this data to create a mapping script.
        if (direction == MappingDirection.CONSUMER_TO_PRODUCER) {
            sourceInfo = consumerTypeInfo;
            targetInfo = providerTypeInfo;
        } else {
            sourceInfo = providerTypeInfo;
            targetInfo = consumerTypeInfo;
            
            // TODO Implement this
            throw new IllegalArgumentException();
        }
        
        return this.createScript(sourceInfo, targetInfo, resolution);
    }    
    
    private ApiMappingScript createScript(Map<Type, TypeInfo<?>> sourceInfo, Map<Type, TypeInfo<?>> targetInfo, DefinitionResolution resolution) {
        MappingOperationCreator operationCreator = new MappingOperationCreator(sourceInfo, targetInfo, resolution);
        
        // TODO Remove hard-coded client-to-provider direction
        List<UserDefinedTypeMappingOperation> operations = resolution.providerTypes()
            .filter(UserDefinedType.class::isInstance)
            .map(operationCreator::deriveOperation)
            .map(UserDefinedTypeMappingOperation.class::cast)
            .collect(Collectors.toList());

        // Sort operations by type id
        Collections.sort(operations, (op1, op2) -> Integer.compare(op1.getTypeId(), op2.getTypeId()));
        
        return new ApiMappingScript(operations);
    }    
    
    private static class MappingOperationCreator implements TypeVisitor<ApiMappingOperation> {
        
        private final Map<Type, TypeInfo<?>> sourceInfos;
        
        private final Map<Type, TypeInfo<?>> targetInfos;
        
        private final DefinitionResolution resolution;
        
        private final Map<Type, ApiMappingOperation> typeToOperation;
        
        public MappingOperationCreator(Map<Type, TypeInfo<?>> sourceInfos, Map<Type, TypeInfo<?>> targetInfos, DefinitionResolution resolution) {
            this.sourceInfos = sourceInfos;
            this.targetInfos = targetInfos;
            this.resolution = resolution;
            this.typeToOperation = new HashMap<>();
        }
        
        public ApiMappingOperation deriveOperation(Type type) {
            // We cannot use computeIfAbsent here, since we may call this
            // operation recursively
            ApiMappingOperation operation = this.typeToOperation.get(type);
            if (operation != null) {
                return operation;
            }
            
            operation = type.accept(this);
            this.typeToOperation.put(type, operation);
            
            return operation;
        }
        
        @Override
        public ApiMappingOperation handleAtomicType(AtomicType atomicType) {
            switch (atomicType) {
            case INT_32:
                return new CopyOperation(INT32_SIZE);
                
            case INT_64:
                return new CopyOperation(INT64_SIZE);
                
            default:
                throw new IllegalArgumentException();
            }
        }
        
        @Override
        public ApiMappingOperation handleBoundedListType(BoundedListType boundedListType) {
            Type targetElementType = boundedListType.getElementType();
            Type sourceElementType = this.resolution.mapProviderType(targetElementType);
            
            TypeInfo<?> sourceElementTypeInfo = this.sourceInfos.get(sourceElementType);
            TypeInfo<?> targetElementTypeInfo = this.targetInfos.get(targetElementType);
            
            ApiMappingOperation elementMappingOperation = this.deriveOperation(targetElementType);
            
            return new ListMappingOperation(boundedListType.getBound(), sourceElementTypeInfo.getSize(), targetElementTypeInfo.getSize(), elementMappingOperation);
        }
        
        @Override
        public ApiMappingOperation handleBoundedStringType(BoundedStringType boundedStringType) {
            return new CopyOperation(boundedStringType.getBound());
        }
        
        @Override
        public ApiMappingOperation handleEnumType(EnumType<?, ?, ?> enumType) {
            EnumTypeInfo<?> targetTypeInfo = (EnumTypeInfo<?>) this.targetInfos.get(enumType);
            
            EnumType<?, ?, ?> sourceType = (EnumType<?, ?, ?>) this.resolution.mapProviderType(enumType);
            EnumTypeInfo<?> sourceTypeInfo = (EnumTypeInfo<?>) this.sourceInfos.get(sourceType);
            
            int numberOfMembers = sourceType.getDeclaredMembers().size();
            int[] ordinalMap = new int[numberOfMembers];
            
            for (EnumMember<?, ?> sourceMember : sourceType) {
                EnumMember<?, ?> targetMember = this.resolution.mapConsumerEnumMember((ConsumerEnumMember) sourceMember);
                
                Integer sourceOrdinal = sourceTypeInfo.getOrdinalFor(sourceMember);
                Integer targetOrdinal = targetTypeInfo.getOrdinalFor(targetMember);
                
                if (targetOrdinal == null) {
                    ordinalMap[sourceOrdinal] = -1;
                } else {
                    ordinalMap[sourceOrdinal] = targetOrdinal;
                }
            }
            
            return new EnumMappingOperation(enumType.getTypeId(), ordinalMap);
        }
        
        @Override
        public ApiMappingOperation handleRecordType(RecordType<?, ?, ?> recordType) {
            Type sourceType = this.resolution.mapProviderType(recordType);
            RecordTypeInfo<?> sourceTypeInfo = (RecordTypeInfo<?>) this.sourceInfos.get(sourceType);
            
            List<FieldMapping> fieldMappings = new ArrayList<>();
            for (Field<?, ?> targetField : recordType) {
                Field<?, ?> sourceField = this.resolution.mapProviderField((ProviderField) targetField);
                
                FieldMapping fieldMapping;
                if (sourceField == null) {
                    // If there is no source field, skip the target field
                    TypeInfo<?> targetFieldTypeInfo = this.targetInfos.get(targetField.getType());
                    fieldMapping = new FieldMapping(0, new SkipOperation(targetFieldTypeInfo.getSize()));
                } else {
                    // If there is a source field, perform the appropriate mapping operation
                    FieldInfo sourceFieldInfo = sourceTypeInfo.getFieldInfoFor(sourceField).orElseThrow(NoSuchElementException::new);
                    ApiMappingOperation fieldMappingOperation = this.deriveOperation(targetField.getType());
                    
                    fieldMapping = new FieldMapping(sourceFieldInfo.getOffset(), fieldMappingOperation);
                }
                
                fieldMappings.add(fieldMapping);
            }
            
            return new RecordMappingOperation(recordType.getTypeId(), fieldMappings);
        }
        
    }
    
    private Map<Type, TypeInfo<?>> createTypeInfos(Stream<Type> types) {
        Map<Type, TypeInfo<?>> typeInfos = new HashMap<>();        
        TypeInfoCreator infoCreator = new TypeInfoCreator(typeInfos);
                
        types.forEach(infoCreator::determineInfoForType);
        
        return typeInfos;
    }    

    private static class TypeInfo<T extends Type> {
        
        private final int size;
        
        private final T type;
        
        public TypeInfo(T type, int size) {
            this.type = type;
            this.size = size;
        }
        
        public int getSize() {
            return this.size;
        }
        
        public T getType() {
            return this.type;
        }
                
    }
    
    private static class RecordTypeInfo<T extends RecordType<?, ?, ?>> extends TypeInfo<T> {
        
        private final List<FieldInfo> fieldInfos;
        
        private final Map<Field<?, ?>, FieldInfo> fieldInfoLookup;
        
        public RecordTypeInfo(T type, int size, List<FieldInfo> fieldInfos) {
            super(type, size);
            
            this.fieldInfos = fieldInfos;
            this.fieldInfoLookup = createFieldInfoLookup(fieldInfos);
        }
        
        private static Map<Field<?, ?>, FieldInfo> createFieldInfoLookup(List<FieldInfo> fieldInfos) {
            return fieldInfos.stream().collect(Collectors.toMap(FieldInfo::getField, Function.identity()));
        }
        
        public List<FieldInfo> getFieldInfos() {
            return this.fieldInfos;
        }
        
        public Optional<FieldInfo> getFieldInfoFor(Field<?, ?> field) {
            return Optional.ofNullable(this.fieldInfoLookup.get(field));
        }
        
    }
    
    private static class EnumTypeInfo<T extends EnumType<?, ?, ?>> extends TypeInfo<T> {
        
        private final Map<EnumMember<?, ?>, Integer> memberToOrdinal;
        
        public EnumTypeInfo(T type, int size) {
            super(type, size);
            
            int currentOrdinal = 0;
            Map<EnumMember<?, ?>, Integer> memberToOrdinal = new HashMap<>();
            for (EnumMember<?, ?> member : type) {
                memberToOrdinal.put(member, currentOrdinal);
                currentOrdinal++;
            }
            
            this.memberToOrdinal = memberToOrdinal;
        }
        
        public Integer getOrdinalFor(EnumMember<?, ?> member) {
            return this.memberToOrdinal.get(member);
        }
        
    }
    
    private static class FieldInfo {
        
        private final int offset;
        
        private final int size;
        
        private final Field<?, ?> field;
        
        public FieldInfo(Field<?, ?> field, int offset, int size) {
            this.field = field;
            this.offset = offset;
            this.size = size;
        }
        
        public int getSize() {
            return this.size;
        }
        
        public int getOffset() {
            return this.offset;
        }
        
        public Field<?, ?> getField() {
            return this.field;
        }
        
    }
    
    private static class TypeInfoCreator implements TypeVisitor<TypeInfo<?>> {
        
        private final Map<Type, TypeInfo<?>> typeInfos;
        
        public TypeInfoCreator(Map<Type, TypeInfo<?>> typeInfos) {
            this.typeInfos = typeInfos;
        }
        
        public TypeInfo<?> determineInfoForType(Type type) {
            TypeInfo<?> typeInfo = this.typeInfos.get(type); 
            if (typeInfo != null) {
                return typeInfo;
            }
            
            typeInfo = type.accept(this);
            this.typeInfos.put(type, typeInfo);
            
            return typeInfo;
        }
        
        @Override
        public TypeInfo<?> handleAtomicType(AtomicType atomicType) {
            int size;
            
            switch (atomicType) {
            case INT_32:
                size = INT32_SIZE;
                break;
                
            case INT_64:
                size = INT64_SIZE;
                break;
                
            default:
                throw new IllegalArgumentException("Unsupported atomic type " + atomicType + ".");    
            }
            
            return new TypeInfo<>(atomicType, size);
        }
        
        @Override
        public TypeInfo<?> handleBoundedListType(BoundedListType boundedListType) {
            Type elementType = boundedListType.getElementType();
            TypeInfo<?> elementTypeInfo = this.determineInfoForType(elementType);
            
            // Four additional bytes to store the actual number of elements
            int size = (boundedListType.getBound() * elementTypeInfo.getSize()) + NUMBER_OF_ELEMENTS_INDICATOR_SIZE;
            
            return new TypeInfo<Type>(boundedListType, size);
        }
        
        @Override
        public TypeInfo<?> handleBoundedStringType(BoundedStringType boundedStringType) {
            return new TypeInfo<>(boundedStringType, boundedStringType.getBound());
        }
        
        @Override
        public TypeInfo<?> handleEnumType(EnumType<?, ?, ?> enumType) {
            // Enums are encoded as int32 values
            return new EnumTypeInfo<>(enumType, ENUM_SIZE);
        }
        
        @Override
        public TypeInfo<?> handleRecordType(RecordType<?, ?, ?> recordType) {
            int offset = 0;
            
            List<FieldInfo> fieldInfos = new ArrayList<>();
            for (Field<?, ?> field : recordType) {
                FieldInfo fieldInfo = this.handleField(field, offset);
                
                fieldInfos.add(fieldInfo);
                offset += fieldInfo.getSize();                
            }
            
            int size = offset;
            if (recordType.hasSubTypes()) {
                // If the record type has subtypes, we need a discriminator (int32)
                size += DISCRIMINATOR_SIZE;
            }
            
            return new RecordTypeInfo<>(recordType, size, fieldInfos);
        }
                
        private FieldInfo handleField(Field<?, ?> field, int offset) {
            Type fieldType = field.getType();
            TypeInfo<?> fieldTypeInfo = this.determineInfoForType(fieldType);
                                    
            return new FieldInfo(field, offset, fieldTypeInfo.getSize());
        }
        
    }        
    
    public enum MappingDirection {
        CONSUMER_TO_PRODUCER,
        PRODUCER_TO_CONSUMER
    }

}
