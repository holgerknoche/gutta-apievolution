package gutta.apievolution.fixedformat.apimapping;

import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.NoSuchElementException;
import java.util.Optional;
import java.util.Set;
import java.util.function.Function;
import java.util.stream.Collectors;
import java.util.stream.Stream;

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
import gutta.apievolution.core.apimodel.consumer.ConsumerField;
import gutta.apievolution.core.apimodel.provider.ProviderEnumMember;
import gutta.apievolution.core.apimodel.provider.ProviderField;
import gutta.apievolution.core.resolution.DefinitionResolution;
import gutta.apievolution.fixedformat.apimapping.PolymorphicRecordMappingOperation.PolymorphicRecordMapping;

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

    	MappingInfoProvider mappingInfoProvider;
    	if (direction == MappingDirection.CONSUMER_TO_PRODUCER) {
    	    mappingInfoProvider = new ConsumerToProviderMappingInfoProvider(consumerTypeInfo, providerTypeInfo, resolution);
    	} else {
    	    mappingInfoProvider = new ProviderToConsumerMappingInfoProvider(consumerTypeInfo, providerTypeInfo, resolution);
    	}    	

    	TypeEntryCreator typeEntryCreator = new TypeEntryCreator(mappingInfoProvider);
    	List<TypeEntry> typeEntries = typeEntryCreator.createTypeEntries();
    	
    	return new ApiMappingScript(typeEntries);
    }    

    private static abstract class MappingInfoProvider {
        
        final Map<Type, TypeInfo<?>> consumerTypeInfo;
        
        final Map<Type, TypeInfo<?>> providerTypeInfo;
        
        final DefinitionResolution resolution;
        
        protected MappingInfoProvider(Map<Type, TypeInfo<?>> consumerTypeInfo, Map<Type, TypeInfo<?>> providerTypeInfo, DefinitionResolution resolution) {
            this.consumerTypeInfo = consumerTypeInfo;
            this.providerTypeInfo = providerTypeInfo;
            this.resolution = resolution;
        }
        
        abstract Stream<Type> getTargetTypes();
        
        abstract <T extends Type> T toSourceType(Type targetType);
        
        abstract <T extends TypeInfo<?>> T getInfoForSourceType(Type sourceType);
        
        abstract <T extends TypeInfo<?>> T getInfoForTargetType(Type targetType);
        
        abstract EnumMember<?, ?> toTargetEnumMember(EnumMember<?, ?> sourceMember);
        
        abstract Field<?, ?> toSourceField(Field<?, ?> targetField);
        
    }
    
    private static class ConsumerToProviderMappingInfoProvider extends MappingInfoProvider {
        
        public ConsumerToProviderMappingInfoProvider(Map<Type, TypeInfo<?>> consumerTypeInfo, Map<Type, TypeInfo<?>> providerTypeInfo, DefinitionResolution resolution) {
            super(consumerTypeInfo, providerTypeInfo, resolution);
        }
        
        @Override
        Stream<Type> getTargetTypes() {
            return this.resolution.providerTypes();
        }
        
        @Override
        @SuppressWarnings("unchecked")
        <T extends Type> T toSourceType(Type targetType) {
            return (T) this.resolution.mapProviderType(targetType);
        }

        @Override
        @SuppressWarnings("unchecked")
        <T extends TypeInfo<?>> T getInfoForSourceType(Type sourceType) {
            return (T) this.consumerTypeInfo.get(sourceType);
        }

        @Override
        @SuppressWarnings("unchecked")
        <T extends TypeInfo<?>> T getInfoForTargetType(Type targetType) {
            return (T) this.providerTypeInfo.get(targetType);
        }

        @Override
        EnumMember<?, ?> toTargetEnumMember(EnumMember<?, ?> sourceMember) {            
            return this.resolution.mapConsumerEnumMember((ConsumerEnumMember) sourceMember);
        }
        
        @Override
        Field<?, ?> toSourceField(Field<?, ?> targetField) {
            return this.resolution.mapProviderField((ProviderField) targetField);
        }
        
    }
    
    private static class ProviderToConsumerMappingInfoProvider extends MappingInfoProvider {
        
        public ProviderToConsumerMappingInfoProvider(Map<Type, TypeInfo<?>> consumerTypeInfo, Map<Type, TypeInfo<?>> providerTypeInfo, DefinitionResolution resolution) {
            super(consumerTypeInfo, providerTypeInfo, resolution);
        }
        
        @Override
        Stream<Type> getTargetTypes() {
            return this.resolution.consumerTypes();
        }
        
        @Override
        @SuppressWarnings("unchecked")
        <T extends Type> T toSourceType(Type targetType) {
            return (T) this.resolution.mapConsumerType(targetType);
        }

        @Override
        @SuppressWarnings("unchecked")
        <T extends TypeInfo<?>> T getInfoForSourceType(Type sourceType) {
            return (T) this.providerTypeInfo.get(sourceType);
        }

        @Override
        @SuppressWarnings("unchecked")
        <T extends TypeInfo<?>> T getInfoForTargetType(Type targetType) {
            return (T) this.consumerTypeInfo.get(targetType);
        }

        @Override
        EnumMember<?, ?> toTargetEnumMember(EnumMember<?, ?> sourceMember) {            
            return this.resolution.mapProviderEnumMember((ProviderEnumMember) sourceMember);
        }
        
        @Override
        Field<?, ?> toSourceField(Field<?, ?> targetField) {
            return this.resolution.mapConsumerField((ConsumerField) targetField);
        }
        
    }
    
    private static class TypeEntryCreator implements TypeVisitor<TypeEntry> {
        
        private final MappingInfoProvider mappingInfoProvider;
        
        private Map<Type, Integer> typeToEntryIndex;
        
        private MappingOperationCreator operationCreator;
                
        public TypeEntryCreator(MappingInfoProvider mappingInfoProvider) {
            this.mappingInfoProvider = mappingInfoProvider;
        }
        
        public List<TypeEntry> createTypeEntries() {
        	// Filter out all user-defined types. Javac seems to have problems figuring out the correct types,
        	// so we do not use the purely stream-based approach here
        	List<UserDefinedType<?>> udts = new ArrayList<>();
        	this.mappingInfoProvider.getTargetTypes().forEach(type -> {
        		if (type instanceof UserDefinedType) {
        			udts.add((UserDefinedType<?>) type);
        		}
        	});       
        	            
            // Sort entries by type id and create type-to-entry map
            Collections.sort(udts, (udt1, udt2) -> Integer.compare(udt1.getTypeId(), udt2.getTypeId()));
            int entryIndex = 0;
            Map<Type, Integer> typeToEntryIndex = new HashMap<>();
            
            for (UserDefinedType<?> udt : udts) {
                typeToEntryIndex.put(udt, entryIndex);
                entryIndex++;
            }
            
            this.typeToEntryIndex = typeToEntryIndex;
            this.operationCreator = new MappingOperationCreator(this.mappingInfoProvider, typeToEntryIndex);
            
            List<TypeEntry> typeEntries = udts.stream()
                    .map(this::createTypeEntryFor)
                    .collect(Collectors.toList());
            
            this.typeToEntryIndex = null;
            this.operationCreator = null;
            
            return typeEntries;
        }               

        private TypeEntry createTypeEntryFor(UserDefinedType<?> type) {
            return type.accept(this);
        }
        
        @Override
        public TypeEntry handleEnumType(EnumType<?, ?, ?> enumType) {
            EnumType<?, ?, ?> sourceType = this.mappingInfoProvider.toSourceType(enumType);                                           
            EnumTypeInfo<?> sourceTypeInfo = this.mappingInfoProvider.getInfoForSourceType(sourceType);
            EnumTypeInfo<?> targetTypeInfo = this.mappingInfoProvider.getInfoForTargetType(enumType);
            
            int numberOfMembers = sourceType.getDeclaredMembers().size();
            int[] ordinalMap = new int[numberOfMembers];
            
            for (EnumMember<?, ?> sourceMember : sourceType) {
                EnumMember<?, ?> targetMember = this.mappingInfoProvider.toTargetEnumMember(sourceMember);
                
                Integer sourceOrdinal = sourceTypeInfo.getOrdinalFor(sourceMember);
                Integer targetOrdinal = targetTypeInfo.getOrdinalFor(targetMember);
                
                if (targetOrdinal == null) {
                    ordinalMap[sourceOrdinal] = -1;
                } else {
                    ordinalMap[sourceOrdinal] = targetOrdinal;
                }
            }
            
            int entryIndex = this.typeToEntryIndex.get(enumType);
            return new EnumTypeEntry(entryIndex, enumType.getTypeId(), ordinalMap);
        }
        
        @Override
        public TypeEntry handleRecordType(RecordType<?, ?, ?> recordType) {
            Type sourceType = this.mappingInfoProvider.toSourceType(recordType);
            RecordTypeInfo<?> sourceTypeInfo = this.mappingInfoProvider.getInfoForSourceType(sourceType);
            
            List<FieldMapping> fieldMappings = new ArrayList<>();
            for (Field<?, ?> targetField : recordType) {
                Field<?, ?> sourceField = this.mappingInfoProvider.toSourceField(targetField);
                
                FieldMapping fieldMapping;
                if (sourceField == null) {
                    // If there is no source field, skip the target field
                    TypeInfo<?> targetFieldTypeInfo = this.mappingInfoProvider.getInfoForTargetType(targetField.getType());
                    fieldMapping = new FieldMapping(0, new SkipOperation(targetFieldTypeInfo.getSize()));
                } else {
                    // If there is a source field, perform the appropriate mapping operation
                    FieldInfo sourceFieldInfo = sourceTypeInfo.getFieldInfoFor(sourceField).orElseThrow(NoSuchElementException::new);
                    ApiMappingOperation fieldMappingOperation = this.deriveOperation(targetField.getType());
                    
                    fieldMapping = new FieldMapping(sourceFieldInfo.getOffset(), fieldMappingOperation);
                }
                
                fieldMappings.add(fieldMapping);
            }
            
            int entryIndex = this.typeToEntryIndex.get(recordType);
            return new RecordTypeEntry(entryIndex, recordType.getTypeId(), fieldMappings);
        }
        
        private ApiMappingOperation deriveOperation(Type type) {
            return this.operationCreator.deriveOperation(type);
        }
        
    }
    
    private static class MappingOperationCreator implements TypeVisitor<ApiMappingOperation> {
        
        private final Map<Type, ApiMappingOperation> typeToOperation;
        
        private final Map<Type, Integer> typeToEntryIndex;
        
        private final MappingInfoProvider mappingInfoProvider;
        
        protected MappingOperationCreator(MappingInfoProvider mappingInfoProvider, Map<Type, Integer> typeToEntryIndex) {
            this.mappingInfoProvider = mappingInfoProvider;
            this.typeToEntryIndex = typeToEntryIndex;
            this.typeToOperation = new HashMap<>();
        }                
        
        private ApiMappingOperation deriveOperation(Type type) {
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
            Type sourceElementType = this.mappingInfoProvider.toSourceType(targetElementType);
            
            TypeInfo<?> sourceElementTypeInfo = this.mappingInfoProvider.getInfoForSourceType(sourceElementType);
            TypeInfo<?> targetElementTypeInfo = this.mappingInfoProvider.getInfoForTargetType(targetElementType);
            
            ApiMappingOperation elementMappingOperation = this.deriveOperation(targetElementType);
            
            return new ListMappingOperation(boundedListType.getBound(), sourceElementTypeInfo.getSize(), targetElementTypeInfo.getSize(), elementMappingOperation);
        }
        
        @Override
        public ApiMappingOperation handleBoundedStringType(BoundedStringType boundedStringType) {
            return new CopyOperation(boundedStringType.getBound());
        }
        
        @Override
        public ApiMappingOperation handleEnumType(EnumType<?, ?, ?> enumType) {
        	int entryIndex = this.typeToEntryIndex.get(enumType);
        	return new EnumMappingOperation(entryIndex);
        }
        
        @Override
        public ApiMappingOperation handleRecordType(RecordType<?, ?, ?> recordType) {
            if (recordType.hasSubTypes()) {
            	return this.handlePolymorphicRecordType(recordType);
            } else {
            	return this.handleNonPolymorphicRecordType(recordType);
            }
        }
        
        private ApiMappingOperation handleNonPolymorphicRecordType(RecordType<?, ?, ?> recordType) {
        	int entryIndex = this.typeToEntryIndex.get(recordType);
            return new RecordMappingOperation(entryIndex);
        }
        
        private ApiMappingOperation handlePolymorphicRecordType(RecordType<?, ?, ?> recordType) {
        	Set<RecordType<?, ?, ?>> allConcreteSubtypes = this.collectAllConcreteSubtypes(recordType);
        	
        	// TODO Collect the necessary data, esp. target type ids        	
        	Map<Integer, PolymorphicRecordMapping> idToRecordMapping = new HashMap<>(allConcreteSubtypes.size());
        	for (RecordType<?, ?, ?> targetType : allConcreteSubtypes) {
        		RecordType<?, ?, ?> sourceType = this.mappingInfoProvider.toSourceType(targetType);
        		
        		int sourceTypeId = sourceType.getTypeId();
        		int targetTypeId = targetType.getTypeId();        		
        		int entryIndex = this.typeToEntryIndex.get(targetType);
        		
        		PolymorphicRecordMapping recordMapping = new PolymorphicRecordMapping(sourceTypeId, targetTypeId, entryIndex);
        		idToRecordMapping.put(recordMapping.getSourceTypeId(), recordMapping);
        	}
        	
        	return new PolymorphicRecordMappingOperation(idToRecordMapping);
        }
        
        private Set<RecordType<?, ?, ?>> collectAllConcreteSubtypes(RecordType<?, ?, ?> recordType) {
        	return this.collectAllConcreteSubtypes(recordType, new HashSet<>());
        }
        
        private Set<RecordType<?, ?, ?>> collectAllConcreteSubtypes(RecordType<?, ?, ?> recordType, Set<RecordType<?, ?, ?>> knownTypes) {
        	if (knownTypes.contains(recordType)) {
        		return knownTypes;
        	}
        	 
        	if (!recordType.isAbstract()) {
        		knownTypes.add(recordType);
        	}
        	
        	for (RecordType<?, ?, ?> subType : recordType.getSubTypes()) {
        		this.collectAllConcreteSubtypes(subType, knownTypes);
        	}
        	
        	return knownTypes;
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
        
        @Override
        public String toString() {
        	return "Type info for " + this.getType();
        }
                
    }
    
    private static class RecordTypeInfo<T extends RecordType<?, ?, ?>> extends TypeInfo<T> {
                
        private final Map<Field<?, ?>, FieldInfo> fieldInfoLookup;
        
        public RecordTypeInfo(T type, int size, List<FieldInfo> fieldInfos) {
            super(type, size);
            
            this.fieldInfoLookup = createFieldInfoLookup(fieldInfos);
        }
        
        private static Map<Field<?, ?>, FieldInfo> createFieldInfoLookup(List<FieldInfo> fieldInfos) {
            return fieldInfos.stream().collect(Collectors.toMap(FieldInfo::getField, Function.identity()));
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
