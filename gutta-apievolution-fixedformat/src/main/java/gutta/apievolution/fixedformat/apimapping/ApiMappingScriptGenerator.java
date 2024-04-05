package gutta.apievolution.fixedformat.apimapping;

import gutta.apievolution.core.apimodel.AtomicType;
import gutta.apievolution.core.apimodel.BoundedListType;
import gutta.apievolution.core.apimodel.BoundedStringType;
import gutta.apievolution.core.apimodel.EnumMember;
import gutta.apievolution.core.apimodel.EnumType;
import gutta.apievolution.core.apimodel.Field;
import gutta.apievolution.core.apimodel.NumericType;
import gutta.apievolution.core.apimodel.Operation;
import gutta.apievolution.core.apimodel.RecordType;
import gutta.apievolution.core.apimodel.Type;
import gutta.apievolution.core.apimodel.TypeVisitor;
import gutta.apievolution.core.apimodel.UserDefinedType;
import gutta.apievolution.core.apimodel.consumer.ConsumerEnumMember;
import gutta.apievolution.core.apimodel.consumer.ConsumerField;
import gutta.apievolution.core.apimodel.consumer.ConsumerOperation;
import gutta.apievolution.core.apimodel.provider.ProviderEnumMember;
import gutta.apievolution.core.apimodel.provider.ProviderField;
import gutta.apievolution.core.apimodel.provider.ProviderOperation;
import gutta.apievolution.core.resolution.DefinitionResolution;
import gutta.apievolution.fixedformat.apimapping.PolymorphicRecordMappingOperation.PolymorphicRecordMapping;

import java.util.ArrayList;
import java.util.Collection;
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

import static gutta.apievolution.fixedformat.objectmapping.Flags.FLAGS_SIZE;

/**
 * This class provides operations to generate an API mapping script from a definition resolution.
 */
public class ApiMappingScriptGenerator {

    private static final int INT32_SIZE = 4;

    private static final int INT64_SIZE = 8;

    private static final int ENUM_SIZE = 4;

    private static final int NUMBER_OF_ELEMENTS_INDICATOR_SIZE = 4;

    private static final int DISCRIMINATOR_SIZE = 4;

    /**
     * Generates a mapping script from the given definition resolution.
     * 
     * @param resolution The definition resolution to generate the script from
     * @param direction  The direction of the desired mapping
     * @return The generated mapping script
     */
    public ApiMappingScript generateMappingScript(DefinitionResolution resolution, MappingDirection direction) {
        // First, calculate the sizes and offsets of all relevant types for both consumer and provider
        Map<Type, TypeInfo<?>> consumerTypeInfo = this.createTypeInfos(resolution.consumerTypes());
        Map<Type, TypeInfo<?>> providerTypeInfo = this.createTypeInfos(resolution.providerTypes());

        MappingInfoProvider mappingInfoProvider;
        if (direction == MappingDirection.CONSUMER_TO_PROVIDER) {
            mappingInfoProvider = new ConsumerToProviderMappingInfoProvider(consumerTypeInfo, providerTypeInfo, resolution);
        } else {
            mappingInfoProvider = new ProviderToConsumerMappingInfoProvider(consumerTypeInfo, providerTypeInfo, resolution);
        }

        EntryCreator entryCreator = new EntryCreator(mappingInfoProvider);
        List<TypeEntry> typeEntries = entryCreator.createTypeEntries();
        List<OperationEntry> operationEntries = entryCreator.createOperationEntries();

        return new ApiMappingScript(typeEntries, operationEntries);
    }

    private abstract static class MappingInfoProvider {

        final Map<Type, TypeInfo<?>> consumerTypeInfo;

        final Map<Type, TypeInfo<?>> providerTypeInfo;

        final DefinitionResolution resolution;

        protected MappingInfoProvider(Map<Type, TypeInfo<?>> consumerTypeInfo, Map<Type, TypeInfo<?>> providerTypeInfo, DefinitionResolution resolution) {
            this.consumerTypeInfo = consumerTypeInfo;
            this.providerTypeInfo = providerTypeInfo;
            this.resolution = resolution;
        }

        abstract Collection<Type> getTargetTypes();

        abstract Collection<? extends Operation<?, ?, ?>> getTargetOperations();

        abstract <T extends Type> T toSourceType(Type targetType);

        abstract <T extends TypeInfo<?>> T getInfoForSourceType(Type sourceType);

        abstract <T extends TypeInfo<?>> T getInfoForTargetType(Type targetType);

        abstract EnumMember<?, ?> toTargetEnumMember(EnumMember<?, ?> sourceMember);

        abstract Field<?, ?> toSourceField(Field<?, ?> targetField);
        
        abstract Operation<?, ?, ?> toSourceOperation(Operation<?, ?, ?> targetOperation);

    }

    private static class ConsumerToProviderMappingInfoProvider extends MappingInfoProvider {

        public ConsumerToProviderMappingInfoProvider(Map<Type, TypeInfo<?>> consumerTypeInfo, Map<Type, TypeInfo<?>> providerTypeInfo,
                DefinitionResolution resolution) {
            super(consumerTypeInfo, providerTypeInfo, resolution);
        }

        @Override
        Collection<Type> getTargetTypes() {
            return this.resolution.providerTypes();
        }

        @Override
        Collection<? extends Operation<?, ?, ?>> getTargetOperations() {
            return this.resolution.providerOperations();
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
        
        @Override
        Operation<?, ?, ?> toSourceOperation(Operation<?, ?, ?> targetOperation) {
            return this.resolution.mapProviderOperation((ProviderOperation) targetOperation);
        }

    }

    private static class ProviderToConsumerMappingInfoProvider extends MappingInfoProvider {

        public ProviderToConsumerMappingInfoProvider(Map<Type, TypeInfo<?>> consumerTypeInfo, Map<Type, TypeInfo<?>> providerTypeInfo,
                DefinitionResolution resolution) {
            super(consumerTypeInfo, providerTypeInfo, resolution);
        }

        @Override
        Collection<Type> getTargetTypes() {
            return this.resolution.consumerTypes();
        }

        @Override
        Collection<? extends Operation<?, ?, ?>> getTargetOperations() {
            return this.resolution.consumerOperations();
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
        
        @Override
        Operation<?, ?, ?> toSourceOperation(Operation<?, ?, ?> targetOperation) {
            return this.resolution.mapConsumerOperation((ConsumerOperation) targetOperation);
        }

    }

    private static class EntryCreator implements TypeVisitor<TypeEntry> {

        private final MappingInfoProvider mappingInfoProvider;

        private final Map<Type, Integer> typeToEntryIndex;

        private final Map<Type, TypeEntry> typeToEntry;

        private final MappingOperationCreator operationCreator;

        public EntryCreator(MappingInfoProvider mappingInfoProvider) {
            this.mappingInfoProvider = mappingInfoProvider;
            this.typeToEntryIndex = createTypeToEntryIndex(mappingInfoProvider);
            this.typeToEntry = new HashMap<>();
            this.operationCreator = new MappingOperationCreator(this.mappingInfoProvider, this::createTypeEntryFor);
        }

        private static Map<Type, Integer> createTypeToEntryIndex(MappingInfoProvider mappingInfoProvider) {
            List<UserDefinedType<?>> udts = getTargetUDTs(mappingInfoProvider);

            // Sort entries by type id and create type-to-entry map
            Collections.sort(udts, (udt1, udt2) -> Integer.compare(udt1.getTypeId(), udt2.getTypeId()));
            int entryIndex = 0;
            Map<Type, Integer> typeToEntryIndex = new HashMap<>();

            for (UserDefinedType<?> udt : udts) {
                typeToEntryIndex.put(udt, entryIndex);
                entryIndex++;
            }

            return typeToEntryIndex;
        }

        private static List<UserDefinedType<?>> getTargetUDTs(MappingInfoProvider mappingInfoProvider) {
            // Filter out all user-defined types. Javac seems to have problems figuring out the correct types,
            // so we do not use the purely stream-based approach here
            List<UserDefinedType<?>> udts = new ArrayList<>();
            mappingInfoProvider.getTargetTypes().forEach(type -> {
                if (type instanceof UserDefinedType) {
                    udts.add((UserDefinedType<?>) type);
                }
            });

            return udts;
        }

        public List<TypeEntry> createTypeEntries() {
            List<TypeEntry> typeEntries = getTargetUDTs(this.mappingInfoProvider).stream().map(this::createTypeEntryFor).collect(Collectors.toList());

            Collections.sort(typeEntries, (entry1, entry2) -> Integer.compare(entry1.getEntryIndex(), entry2.getEntryIndex()));

            return typeEntries;
        }

        private TypeEntry createTypeEntryFor(UserDefinedType<?> type) {
            // This method may be called recursively, so we cannot use computeIfAbsent here
            TypeEntry candidate = this.typeToEntry.get(type);
            if (candidate != null) {
                return candidate;
            }

            candidate = type.accept(this);
            this.typeToEntry.put(type, candidate);

            return candidate;
        }

        public List<OperationEntry> createOperationEntries() {
            int entryIndex = 0;

            List<? extends Operation<?, ?, ?>> operations = new ArrayList<>(this.mappingInfoProvider.getTargetOperations());            
            // Sort operations by public name for more deterministic scripts
            Collections.sort(operations, (op1, op2) -> op1.getPublicName().compareTo(op2.getPublicName()));
            
            List<OperationEntry> entries = new ArrayList<>(operations.size());

            for (Operation<?, ?, ?> operation : operations) {
                OperationEntry operationEntry = this.createOperationEntry(entryIndex, operation, this.mappingInfoProvider);
                entries.add(operationEntry);

                entryIndex++;
            }

            return entries;
        }

        private OperationEntry createOperationEntry(int entryIndex, Operation<?, ?, ?> targetOperation, MappingInfoProvider mappingInfoProvider) {
            ApiMappingOperation resultMappingOperation = this.operationCreator.createResultMappingOperation(targetOperation);           
            ApiMappingOperation parameterMappingOperation = this.operationCreator.createParameterMappingOperation(targetOperation);

            return new OperationEntry(entryIndex, targetOperation.getPublicName(), parameterMappingOperation, resultMappingOperation);
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

            int dataSize = 0;
            List<FieldMapping> fieldMappings = new ArrayList<>();
            for (Field<?, ?> targetField : recordType) {
                Field<?, ?> sourceField = this.mappingInfoProvider.toSourceField(targetField);

                TypeInfo<?> targetFieldTypeInfo = this.mappingInfoProvider.getInfoForTargetType(targetField.getType());

                FieldMapping fieldMapping;
                if (sourceField == null) {
                    // If there is no source field, skip the target field
                    fieldMapping = new FieldMapping(0, new SkipOperation(targetFieldTypeInfo.getSize()));
                } else {
                    // If there is a source field, perform the appropriate mapping operation
                    FieldInfo sourceFieldInfo = sourceTypeInfo.getFieldInfoFor(sourceField).orElseThrow(NoSuchElementException::new);
                    ApiMappingOperation fieldMappingOperation = this.deriveOperation(targetField.getType());

                    fieldMapping = new FieldMapping(sourceFieldInfo.getOffset(), fieldMappingOperation);
                }

                fieldMappings.add(fieldMapping);
                dataSize += targetFieldTypeInfo.getSize();
            }

            int entryIndex = this.typeToEntryIndex.get(recordType);
            return new RecordTypeEntry(entryIndex, recordType.getTypeId(), dataSize, fieldMappings);
        }

        private ApiMappingOperation deriveOperation(Type type) {
            return this.operationCreator.deriveOperation(type);
        }

    }

    private static class MappingOperationCreator implements TypeVisitor<ApiMappingOperation> {

        private final Map<Type, ApiMappingOperation> typeToOperation;

        private final Function<UserDefinedType<?>, TypeEntry> typeEntryResolver;

        private final MappingInfoProvider mappingInfoProvider;

        protected MappingOperationCreator(MappingInfoProvider mappingInfoProvider, Function<UserDefinedType<?>, TypeEntry> typeEntryResolver) {
            this.mappingInfoProvider = mappingInfoProvider;
            this.typeEntryResolver = typeEntryResolver;
            this.typeToOperation = new HashMap<>();
        }

        private ApiMappingOperation deriveOperation(Type type) {
            // We cannot use computeIfAbsent here, since we may call this operation recursively
            ApiMappingOperation operation = this.typeToOperation.get(type);
            if (operation != null) {
                return operation;
            }

            operation = type.accept(this);
            this.typeToOperation.put(type, operation);

            return operation;
        }

        @SuppressWarnings("unchecked")
        private <T extends TypeEntry> T resolveTypeEntryFor(UserDefinedType<?> type) {
            return (T) this.typeEntryResolver.apply(type);
        }

        @Override
        public ApiMappingOperation handleAtomicType(AtomicType atomicType) {
            TypeInfo<?> typeInfo = this.mappingInfoProvider.getInfoForSourceType(atomicType);
            return new CopyOperation(typeInfo.getSize());
        }

        @Override
        public ApiMappingOperation handleBoundedListType(BoundedListType boundedListType) {
            Type targetElementType = boundedListType.getElementType();
            Type sourceElementType = this.mappingInfoProvider.toSourceType(targetElementType);

            TypeInfo<?> sourceElementTypeInfo = this.mappingInfoProvider.getInfoForSourceType(sourceElementType);
            TypeInfo<?> targetElementTypeInfo = this.mappingInfoProvider.getInfoForTargetType(targetElementType);

            ApiMappingOperation elementMappingOperation = this.deriveOperation(targetElementType);

            return new ListMappingOperation(boundedListType.getBound(), sourceElementTypeInfo.getSize(), targetElementTypeInfo.getSize(),
                    elementMappingOperation);
        }

        @Override
        public ApiMappingOperation handleBoundedStringType(BoundedStringType boundedStringType) {
            TypeInfo<?> typeInfo = this.mappingInfoProvider.getInfoForSourceType(boundedStringType);
            return new CopyOperation(typeInfo.getSize());
        }

        @Override
        public ApiMappingOperation handleEnumType(EnumType<?, ?, ?> enumType) {
            EnumTypeEntry typeEntry = this.resolveTypeEntryFor(enumType);
            return new EnumMappingOperation(typeEntry);
        }

        @Override
        public ApiMappingOperation handleNumericType(NumericType numericType) {
            // We can use either the source and target type info as they need to have the same spec
            TypeInfo<?> typeInfo = this.mappingInfoProvider.getInfoForSourceType(numericType);
            return new CopyOperation(typeInfo.getSize());
        }

        @Override
        public ApiMappingOperation handleRecordType(RecordType<?, ?, ?> recordType) {
            RecordType<?, ?, ?> sourceType = this.mappingInfoProvider.toSourceType(recordType);
            
            boolean sourceIsPolymorphic = sourceType.hasSubTypes();
            boolean targetIsPolymorphic = recordType.hasSubTypes();
            
            if (sourceIsPolymorphic) {
                if (targetIsPolymorphic) {
                    return this.createPolymorphicRecordMappingOperation(recordType);
                } else {
                    return this.createPolyToMonoRecordMappingOperation(recordType);
                }
            } else {
                if (targetIsPolymorphic) {
                    return this.createMonoToPolyRecordMappingOperation(recordType);
                } else {
                    return this.createMonomorphicRecordMappingOperation(recordType);
                }
            }
        }

        private ApiMappingOperation createMonomorphicRecordMappingOperation(RecordType<?, ?, ?> recordType) {
            RecordTypeEntry typeEntry = this.resolveTypeEntryFor(recordType);
            return new MonomorphicRecordMappingOperation(typeEntry);
        }
        
        private ApiMappingOperation createMonoToPolyRecordMappingOperation(RecordType<?, ?, ?> recordType) {
            RecordTypeEntry typeEntry = this.resolveTypeEntryFor(recordType);
            return new MonoToPolyRecordMappingOperation(typeEntry);
        }
        
        private ApiMappingOperation createPolyToMonoRecordMappingOperation(RecordType<?, ?, ?> recordType) {
            RecordTypeEntry typeEntry = this.resolveTypeEntryFor(recordType);
            
            // The type itself and all its subtypes are mappable 
            RecordType<?, ?, ?> sourceType = this.mappingInfoProvider.toSourceType(recordType);
            Set<RecordType<?, ?, ?>> possibleSourceTypes = collectAllConcreteSubtypes(sourceType);
            Set<Integer> mappableTypeIds = possibleSourceTypes.stream()
                .map(RecordType::getTypeId)
                .collect(Collectors.toSet());
            
            return new PolyToMonoRecordMappingOperation(mappableTypeIds, typeEntry);
        }
        
        private ApiMappingOperation createPolymorphicRecordMappingOperation(RecordType<?, ?, ?> recordType) {
            Set<RecordType<?, ?, ?>> possibleTypes = collectAllConcreteSubtypes(recordType);            
            return this.createPolymorphicRecordMappingOperation(possibleTypes);
        }
        
        private ApiMappingOperation createPolymorphicRecordMappingOperation(Set<RecordType<?, ?, ?>> possibleTypes) {
            if (possibleTypes.isEmpty()) {
                throw new IllegalArgumentException("The set of possible types may not be empty.");
            }
            
            Set<PolymorphicRecordMapping> recordMappings = new HashSet<>(possibleTypes.size());
            for (RecordType<?, ?, ?> targetType : possibleTypes) {
                RecordType<?, ?, ?> sourceType = this.mappingInfoProvider.toSourceType(targetType);

                int sourceTypeId = sourceType.getTypeId();
                int targetTypeId = targetType.getTypeId();
                RecordTypeEntry typeEntry = this.resolveTypeEntryFor(targetType);

                PolymorphicRecordMapping recordMapping = new PolymorphicRecordMapping(sourceTypeId, targetTypeId, typeEntry);
                recordMappings.add(recordMapping);
            }

            return new PolymorphicRecordMappingOperation(recordMappings);
        }
                
        @SuppressWarnings("unchecked")
        private static Set<RecordType<?, ?, ?>> collectAllConcreteSubtypes(RecordType<?, ?, ?> recordType) {
            return (Set<RecordType<?, ?, ?>>) recordType.collectAllSubtypes(RecordType::isConcrete);
        }
        
        public ApiMappingOperation createParameterMappingOperation(Operation<?, ?, ?> operation) {
            RecordType<?, ?, ?> parameterType = operation.getParameterType();
            return this.handleRecordType(parameterType);
        }
                
        private static Set<RecordType<?, ?, ?>> possibleResultTypesOf(Operation<?, ?, ?> operation) {
            // Possible result types are all subtypes of the return type and all possible exceptions
            RecordType<?, ?, ?> formalReturnType = operation.getReturnType();
            Set<RecordType<?, ?, ?>> possibleResultTypes = new HashSet<>();
            possibleResultTypes.addAll(collectAllConcreteSubtypes(formalReturnType));

            for (RecordType<?, ?, ?> exceptionType : operation.getThrownExceptions()) {
                possibleResultTypes.addAll(collectAllConcreteSubtypes(exceptionType));
            }
            
            return possibleResultTypes;
        }
        
        private static boolean hasPolymorphicResult(Operation<?, ?, ?> operation) {
            return (operation.hasExceptions() || operation.getReturnType().hasSubTypes());
        }
        
        public ApiMappingOperation createResultMappingOperation(Operation<?, ?, ?> operation) {
            Operation<?, ?, ?> sourceOperation = this.mappingInfoProvider.toSourceOperation(operation);
                        
            boolean sourceIsPolymorphic = hasPolymorphicResult(sourceOperation);
            boolean targetIsPolymorphic = hasPolymorphicResult(operation);
            
            if (sourceIsPolymorphic) {
                if (targetIsPolymorphic) {
                    // If both source and target are polymorphic, collect all possible result types (including exceptions)
                    // and build a polymorphic mapping operation
                    Set<RecordType<?, ?, ?>> possibleTypes = possibleResultTypesOf(operation);
                    return this.createPolymorphicRecordMappingOperation(possibleTypes);
                } else {
                    // If only the source is polymorphic, create a poly-to-mono mapping operation
                    return this.createPolyToMonoRecordMappingOperation(operation.getReturnType());
                }
            } else {
                if (targetIsPolymorphic) {
                    // If only the target is polymorphic, create a mono-to-poly mapping operation
                    return this.createMonoToPolyRecordMappingOperation(operation.getReturnType());
                } else {
                    // If neither source nor target is polymorphic, create a monomorphic mapping operation
                    return this.createMonomorphicRecordMappingOperation(operation.getReturnType());
                }
            }            
        }
    }

    private Map<Type, TypeInfo<?>> createTypeInfos(Collection<Type> types) {
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
                size = (INT32_SIZE + FLAGS_SIZE);
                break;

            case INT_64:
                size = (INT64_SIZE + FLAGS_SIZE);
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
            int size = (boundedListType.getBound() * elementTypeInfo.getSize()) + NUMBER_OF_ELEMENTS_INDICATOR_SIZE + FLAGS_SIZE;

            return new TypeInfo<Type>(boundedListType, size);
        }

        @Override
        public TypeInfo<?> handleBoundedStringType(BoundedStringType boundedStringType) {
            int size = (boundedStringType.getBound() + FLAGS_SIZE);
            return new TypeInfo<>(boundedStringType, size);
        }

        @Override
        public TypeInfo<?> handleEnumType(EnumType<?, ?, ?> enumType) {
            // Enums are encoded as int32 values
            return new EnumTypeInfo<>(enumType, ENUM_SIZE + FLAGS_SIZE);
        }

        @Override
        public TypeInfo<?> handleNumericType(NumericType numericType) {
            // Numeric types are encoded as strings with leading sign
            int size = (numericType.getIntegerPlaces() + numericType.getFractionalPlaces() + 1 + FLAGS_SIZE);
            return new TypeInfo<>(numericType, size);
        }

        @Override
        public TypeInfo<?> handleRecordType(RecordType<?, ?, ?> recordType) {
            int offset = 0;

            // Build field infos for both inherited and declared types
            List<FieldInfo> fieldInfos = new ArrayList<>();
            for (Field<?, ?> field : recordType) {
                FieldInfo fieldInfo = this.handleField(field, offset);

                fieldInfos.add(fieldInfo);
                offset += fieldInfo.getSize();
            }

            // Determine the maximum size of the type
            int size;
            if (recordType.hasSubTypes()) {
                // If the type has subtypes, its maximum size is the size of its largest subtype plus
                // a discriminator field (the flags size is already included)
                int largestSubTypeSize = this.determineSizeOfLargestSubtype(recordType);
                size = (largestSubTypeSize + DISCRIMINATOR_SIZE);
            } else {
                // If the type is a leaf type, it's size is simply the offset "after" the last field
                size = (offset + FLAGS_SIZE);
            }

            return new RecordTypeInfo<>(recordType, size, fieldInfos);
        }

        private int determineSizeOfLargestSubtype(RecordType<?, ?, ?> recordType) {
            int largestSubTypeSize = 0;

            for (RecordType<?, ?, ?> subType : recordType.getSubTypes()) {
                TypeInfo<?> subTypeInfo = this.determineInfoForType(subType);
                if (subTypeInfo.getSize() > largestSubTypeSize) {
                    largestSubTypeSize = subTypeInfo.getSize();
                }
            }

            return largestSubTypeSize;
        }

        private FieldInfo handleField(Field<?, ?> field, int offset) {
            Type fieldType = field.getType();

            if (fieldType.isUnbounded()) {
                throw new ScriptGenerationException("The type of field '" + field + "' is unbounded.");
            }

            TypeInfo<?> fieldTypeInfo = this.determineInfoForType(fieldType);

            return new FieldInfo(field, offset, fieldTypeInfo.getSize());
        }

    }

    /**
     * Enumeration of possible mapping directions.
     *
     */
    public enum MappingDirection {
        /**
         * Represents a mapping from consumer to provider.
         */
        CONSUMER_TO_PROVIDER,
        /**
         * Represents a mapping from provider to consumer.
         */
        PROVIDER_TO_CONSUMER
    }

}
