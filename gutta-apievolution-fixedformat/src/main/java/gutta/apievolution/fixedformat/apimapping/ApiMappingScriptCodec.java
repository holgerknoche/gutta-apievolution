package gutta.apievolution.fixedformat.apimapping;

import gutta.apievolution.fixedformat.apimapping.PolymorphicRecordMappingOperation.PolymorphicRecordMapping;

import java.io.ByteArrayOutputStream;
import java.io.DataOutputStream;
import java.io.IOException;
import java.nio.ByteBuffer;
import java.nio.charset.Charset;
import java.nio.charset.StandardCharsets;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

/**
 * This class provides operations to encode or decode an API mapping script to/from its binary representation.
 */
public class ApiMappingScriptCodec {

    private static final Charset CHARSET = StandardCharsets.UTF_8;

    private static final byte OPCODE_COPY = 0x01;

    private static final byte OPCODE_SKIP = 0x02;

    private static final byte OPCODE_MAP_ENUM = 0x03;

    private static final byte OPCODE_MAP_RECORD = 0x04;

    private static final byte OPCODE_MAP_LIST = 0x05;

    private static final byte OPCODE_MAP_POLYMORPHIC_RECORD = 0x06;

    private static final byte OPCODE_MAP_MONO_TO_POLY_RECORD = 0x07;

    private static final byte OPCODE_MAP_POLY_TO_MONO_RECORD = 0x08;

    private static final byte ENTRY_TYPE_ENUM = 0x01;

    private static final byte ENTRY_TYPE_RECORD = 0x02;

    /**
     * Encodes the given script and returns the result as a byte buffer.
     * 
     * @param script The script to encode
     * @return The encoded script
     */
    public byte[] encodeScript(ApiMappingScript script) {
        try (ByteArrayOutputStream byteStream = new ByteArrayOutputStream(); DataOutputStream dataStream = new DataOutputStream(byteStream)) {

            // Write the actual script
            ScriptOffsets offsets = this.writeScriptToStream(script, dataStream);

            // Fill the offset table at the beginning of the encoded script
            byte[] encodedScript = byteStream.toByteArray();
            ByteBuffer scriptBuffer = ByteBuffer.wrap(encodedScript);

            scriptBuffer.position(0);
            // Offset of the type list
            scriptBuffer.putInt(offsets.typeListOffset);
            // Offset of the operations list
            scriptBuffer.putInt(offsets.operationListOffset);

            // Fill the type entry offset table
            scriptBuffer.position(offsets.typeListOffset + 4);
            for (int offset : offsets.typeEntryOffsets) {
                scriptBuffer.putInt(offset);
            }

            // Fill the operation offset table
            scriptBuffer.position(offsets.operationListOffset + 4);
            for (int offset : offsets.operationEntryOffsets) {
                scriptBuffer.putInt(offset);
            }

            return encodedScript;
        } catch (IOException e) {
            throw new RuntimeException(e);
        }
    }

    private ScriptOffsets writeScriptToStream(ApiMappingScript script, DataOutputStream stream) throws IOException {
        List<TypeEntry> typeEntries = script.getTypeEntries();
        int numberOfTypeEntries = typeEntries.size();
        int[] typeEntryOffsets = new int[numberOfTypeEntries];

        // Insert placeholders for the type list offset and operation list offset
        stream.writeInt(0);
        stream.writeInt(0);

        // Prepare the offset table, but zero the offsets for the time being. They are filled later on.
        int typeEntryListOffset = stream.size();
        stream.writeInt(numberOfTypeEntries);
        for (int entry = 0; entry < numberOfTypeEntries; entry++) {
            stream.writeInt(0);
        }

        // Write the type entries
        TypeEntryWriter writer = new TypeEntryWriter(stream);
        for (TypeEntry typeEntry : typeEntries) {
            typeEntryOffsets[typeEntry.getEntryIndex()] = stream.size();
            writer.writeEntry(typeEntry);
        }

        List<OperationEntry> operationEntries = script.getOperationEntries();
        int numberOfOperationEntries = operationEntries.size();

        // Write the operations entries
        int operationsOffset = stream.size();
        int[] operationEntryOffsets = new int[numberOfOperationEntries];

        // Prepare the offset table for the operation entries
        stream.writeInt(operationEntries.size());
        for (int entry = 0; entry < numberOfOperationEntries; entry++) {
            stream.writeInt(0);
        }

        // Write the operation entries
        for (OperationEntry operationEntry : operationEntries) {
            operationEntryOffsets[operationEntry.getEntryIndex()] = stream.size();
            this.writeOperationEntry(operationEntry, stream);
        }

        return new ScriptOffsets(typeEntryListOffset, typeEntryOffsets, operationsOffset, operationEntryOffsets);
    }

    private void writeOperationEntry(OperationEntry entry, DataOutputStream stream) {
        try {
            byte[] nameBytes = entry.getName().getBytes(CHARSET);

            stream.writeInt(nameBytes.length);
            stream.write(nameBytes);

            OperationWriter operationWriter = new OperationWriter(stream);
            operationWriter.writeOperation(entry.getParameterMappingOperation());
            operationWriter.writeOperation(entry.getResultMappingOperation());
        } catch (IOException e) {
            throw new ScriptEncodingException("Error writing operation entry '" + entry + "'.", e);
        }
    }

    /**
     * Decodes an encoded API mapping script.
     * 
     * @param encodedScript The encoded script stored in a byte array
     * @return The decoded API mapping script
     */
    public ApiMappingScript decodeScript(byte[] encodedScript) {
        ByteBuffer scriptBuffer = ByteBuffer.wrap(encodedScript);

        // Read offsets for the type list and the operations list
        int typesOffset = scriptBuffer.getInt();
        int operationsOffset = scriptBuffer.getInt();

        // Read the type entry table
        scriptBuffer.position(typesOffset);
        int[] typeEntryOffsets = this.readOffsetTable(scriptBuffer);

        // Read the type entries
        int numberOfEntries = typeEntryOffsets.length;
        TypeEntry[] typeEntries = new TypeEntry[numberOfEntries];
        for (int entryIndex = 0; entryIndex < numberOfEntries; entryIndex++) {
            this.getOrReadTypeEntry(entryIndex, typeEntryOffsets, typeEntries, scriptBuffer);
        }

        // Position the buffer at the correct offset, as the type entries may not be read
        // in order due to nesting and the offset may therefore be off
        scriptBuffer.position(operationsOffset);

        // Read the operation offsets
        int[] operationEntryOffsets = this.readOffsetTable(scriptBuffer);
        int numberOfOperations = operationEntryOffsets.length;
        List<OperationEntry> operationEntries = new ArrayList<>(numberOfOperations);

        // Read the operations themselves
        for (int operationIndex = 0; operationIndex < numberOfOperations; operationIndex++) {
            OperationEntry operationEntry = this.readOperationEntry(operationIndex, operationEntryOffsets, typeEntryOffsets, typeEntries, scriptBuffer);
            operationEntries.add(operationEntry);
        }

        return new ApiMappingScript(Arrays.asList(typeEntries), operationEntries);
    }

    private int[] readOffsetTable(ByteBuffer buffer) {
        int numberOfEntries = buffer.getInt();

        int[] offsets = new int[numberOfEntries];
        for (int index = 0; index < numberOfEntries; index++) {
            offsets[index] = buffer.getInt();
        }

        return offsets;
    }

    private OperationEntry readOperationEntry(int operationIndex, int[] operationEntryOffsets, int[] typeEntryOffsets, TypeEntry[] typeEntries,
            ByteBuffer buffer) {

        buffer.position(operationEntryOffsets[operationIndex]);

        int nameLength = buffer.getInt();
        byte[] nameBytes = new byte[nameLength];
        buffer.get(nameBytes);
        String operationName = new String(nameBytes, CHARSET);

        ApiMappingOperation parameterMappingOperation = this.readApiMappingOperation(typeEntryOffsets, typeEntries, buffer);
        ApiMappingOperation resultMappingOperation = this.readApiMappingOperation(typeEntryOffsets, typeEntries, buffer);

        return new OperationEntry(operationIndex, operationName, parameterMappingOperation, resultMappingOperation);
    }

    private TypeEntry getOrReadTypeEntry(int entryIndex, int[] entryOffsets, TypeEntry[] typeEntries, ByteBuffer buffer) {
        TypeEntry candidate = typeEntries[entryIndex];
        if (candidate != null) {
            return candidate;
        }

        candidate = readTypeEntry(entryIndex, entryOffsets, typeEntries, buffer);
        typeEntries[entryIndex] = candidate;

        return candidate;
    }

    @SuppressWarnings("unchecked")
    private <T extends TypeEntry> T getOrReadTypeEntryEmbedded(int entryIndex, int[] entryOffsets, TypeEntry[] typeEntries, ByteBuffer buffer) {
        int currentOffset = buffer.position();
        T entry = (T) this.getOrReadTypeEntry(entryIndex, entryOffsets, typeEntries, buffer);
        buffer.position(currentOffset);

        return entry;
    }

    private TypeEntry readTypeEntry(int entryIndex, int[] entryOffsets, TypeEntry[] typeEntries, ByteBuffer buffer) {
        // Set the position of the buffer to the offset of the entry
        int entryOffset = entryOffsets[entryIndex];
        buffer.position(entryOffset);

        // Dispatch based on the entry type
        byte entryType = buffer.get();

        switch (entryType) {
        case ENTRY_TYPE_ENUM:
            return this.readEnumTypeEntry(entryIndex, buffer);

        case ENTRY_TYPE_RECORD:
            return this.readRecordTypeEntry(entryIndex, entryOffsets, typeEntries, buffer);

        default:
            throw new IllegalStateException("Unknown entry type " + entryType + " at offset " + buffer.position() + ".");
        }
    }

    private EnumTypeEntry readEnumTypeEntry(int entryIndex, ByteBuffer buffer) {
        int typeId = buffer.getInt();
        int numberOfEntries = buffer.getInt();
        int[] indexMap = new int[numberOfEntries];

        for (int index = 0; index < numberOfEntries; index++) {
            int targetIndex = buffer.getInt();
            indexMap[index] = targetIndex;
        }

        return new EnumTypeEntry(entryIndex, typeId, indexMap);
    }

    private RecordTypeEntry readRecordTypeEntry(int entryIndex, int[] entryOffsets, TypeEntry[] typeEntries, ByteBuffer buffer) {
        int typeId = buffer.getInt();
        int dataLength = buffer.getInt();
        int numberOfFields = buffer.getInt();

        List<FieldMapping> fieldMappings = new ArrayList<>(numberOfFields);
        for (int fieldIndex = 0; fieldIndex < numberOfFields; fieldIndex++) {
            int offset = buffer.getInt();
            ApiMappingOperation operation = this.readApiMappingOperation(entryOffsets, typeEntries, buffer);

            FieldMapping fieldMapping = new FieldMapping(offset, operation);
            fieldMappings.add(fieldMapping);
        }

        return new RecordTypeEntry(entryIndex, typeId, dataLength, fieldMappings);
    }

    private ApiMappingOperation readApiMappingOperation(int[] entryOffsets, TypeEntry[] typeEntries, ByteBuffer buffer) {
        byte opcode = buffer.get();

        switch (opcode) {
        case OPCODE_COPY:
            return this.readCopyOperation(buffer);

        case OPCODE_SKIP:
            return this.readSkipOperation(buffer);

        case OPCODE_MAP_ENUM:
            return this.readMapEnumOperation(entryOffsets, typeEntries, buffer);

        case OPCODE_MAP_RECORD:
            return this.readMapRecordOperation(entryOffsets, typeEntries, buffer);

        case OPCODE_MAP_POLYMORPHIC_RECORD:
            return this.readMapPolymorphicRecordOperation(entryOffsets, typeEntries, buffer);

        case OPCODE_MAP_LIST:
            return this.readMapListOperation(entryOffsets, typeEntries, buffer);

        case OPCODE_MAP_MONO_TO_POLY_RECORD:
            return this.readMapMonoToPolyRecordOperation(entryOffsets, typeEntries, buffer);

        case OPCODE_MAP_POLY_TO_MONO_RECORD:
            return this.readMapPolyToMonoRecordOperation(entryOffsets, typeEntries, buffer);

        default:
            throw new IllegalStateException("Unknown opcode " + opcode + " at offset " + buffer.position() + ".");
        }
    }

    private ApiMappingOperation readCopyOperation(ByteBuffer buffer) {
        int bytesToCopy = buffer.getInt();
        return new CopyOperation(bytesToCopy);
    }

    private ApiMappingOperation readSkipOperation(ByteBuffer buffer) {
        int bytesToSkip = buffer.getInt();
        return new SkipOperation(bytesToSkip);
    }

    private ApiMappingOperation readMapEnumOperation(int[] entryOffsets, TypeEntry[] typeEntries, ByteBuffer buffer) {
        int entryIndex = buffer.getInt();
        EnumTypeEntry typeEntry = this.getOrReadTypeEntryEmbedded(entryIndex, entryOffsets, typeEntries, buffer);
        return new EnumMappingOperation(typeEntry);
    }

    private ApiMappingOperation readMapRecordOperation(int[] entryOffsets, TypeEntry[] typeEntries, ByteBuffer buffer) {
        int entryIndex = buffer.getInt();
        RecordTypeEntry typeEntry = this.getOrReadTypeEntryEmbedded(entryIndex, entryOffsets, typeEntries, buffer);
        return new MonomorphicRecordMappingOperation(typeEntry);
    }

    private ApiMappingOperation readMapPolymorphicRecordOperation(int[] entryOffsets, TypeEntry[] typeEntries, ByteBuffer buffer) {
        int numberOfMappings = buffer.getInt();

        List<PolymorphicRecordMapping> mappings = new ArrayList<>(numberOfMappings);
        for (int mappingIndex = 0; mappingIndex < numberOfMappings; mappingIndex++) {
            int sourceTypeId = buffer.getInt();
            int targetTypeId = buffer.getInt();
            int typeIndex = buffer.getInt();

            RecordTypeEntry typeEntry = this.getOrReadTypeEntryEmbedded(typeIndex, entryOffsets, typeEntries, buffer);
            PolymorphicRecordMapping mapping = new PolymorphicRecordMapping(sourceTypeId, targetTypeId, typeEntry);
            mappings.add(mapping);
        }

        return new PolymorphicRecordMappingOperation(mappings);
    }

    private ApiMappingOperation readMapListOperation(int[] entryOffsets, TypeEntry[] typeEntries, ByteBuffer buffer) {
        int maxElements = buffer.getInt();
        int sourceElementSize = buffer.getInt();
        int targetElementSize = buffer.getInt();
        ApiMappingOperation elementMappingOperation = this.readApiMappingOperation(entryOffsets, typeEntries, buffer);

        return new ListMappingOperation(maxElements, sourceElementSize, targetElementSize, elementMappingOperation);
    }

    private ApiMappingOperation readMapMonoToPolyRecordOperation(int[] entryOffsets, TypeEntry[] typeEntries, ByteBuffer buffer) {
        int typeIndex = buffer.getInt();

        RecordTypeEntry typeEntry = this.getOrReadTypeEntryEmbedded(typeIndex, entryOffsets, typeEntries, buffer);
        return new MonoToPolyRecordMappingOperation(typeEntry);
    }

    private ApiMappingOperation readMapPolyToMonoRecordOperation(int[] entryOffsets, TypeEntry[] typeEntries, ByteBuffer buffer) {
        int typeIndex = buffer.getInt();
        RecordTypeEntry typeEntry = this.getOrReadTypeEntryEmbedded(typeIndex, entryOffsets, typeEntries, buffer);
        
        int numberOfIds = buffer.getInt();

        Set<Integer> mappableTypeIds = new HashSet<>(numberOfIds);
        for (int idIndex = 0; idIndex < numberOfIds; idIndex++) {
            int mappableTypeId = buffer.getInt();
            mappableTypeIds.add(mappableTypeId);
        }

        return new PolyToMonoRecordMappingOperation(mappableTypeIds, typeEntry);
    }

    private static class TypeEntryWriter implements TypeEntryVisitor<Void> {

        private final DataOutputStream dataStream;

        private final OperationWriter operationWriter;

        public TypeEntryWriter(DataOutputStream dataStream) {
            this.dataStream = dataStream;
            this.operationWriter = new OperationWriter(dataStream);
        }

        public void writeEntry(TypeEntry typeEntry) {
            typeEntry.accept(this);
        }

        @Override
        public Void handleEnumTypeEntry(EnumTypeEntry enumTypeEntry) {
            try {
                DataOutputStream outputStream = this.dataStream;
                int[] indexMap = enumTypeEntry.getIndexMap();

                outputStream.writeByte(ENTRY_TYPE_ENUM);
                outputStream.writeInt(enumTypeEntry.getTypeId());

                // Write the index map
                outputStream.writeInt(indexMap.length);
                for (int targetIndex : indexMap) {
                    outputStream.writeInt(targetIndex);
                }

                return null;
            } catch (IOException e) {
                throw new ScriptEncodingException("Error writing enum type entry to the script.", e);
            }
        }

        @Override
        public Void handleRecordTypeEntry(RecordTypeEntry recordTypeEntry) {
            try {
                DataOutputStream outputStream = this.dataStream;

                outputStream.writeByte(ENTRY_TYPE_RECORD);
                outputStream.writeInt(recordTypeEntry.getTypeId());
                outputStream.writeInt(recordTypeEntry.getDataSize());

                List<FieldMapping> fieldMappings = recordTypeEntry.getFieldMappings();

                // Write the individual field mapping operations
                outputStream.writeInt(fieldMappings.size());
                for (FieldMapping fieldMapping : fieldMappings) {
                    this.writeFieldMapping(fieldMapping);
                }

                return null;
            } catch (IOException e) {
                throw new ScriptEncodingException("Error writing record type entry to the script.", e);
            }
        }

        private void writeFieldMapping(FieldMapping fieldMapping) {
            try {
                this.dataStream.writeInt(fieldMapping.getOffset());
                this.operationWriter.writeOperation(fieldMapping.getMappingOperation());
            } catch (IOException e) {
                throw new ScriptEncodingException("Error writing field mapping to the script.", e);
            }
        }

    }

    private static class OperationWriter implements ApiMappingOperationVisitor<Void> {

        private final DataOutputStream dataStream;

        public OperationWriter(DataOutputStream dataStream) {
            this.dataStream = dataStream;
        }

        public void writeOperation(ApiMappingOperation operation) {
            operation.accept(this);
        }

        @Override
        public Void handleCopyOperation(CopyOperation copyOperation) {
            try {
                DataOutputStream outputStream = this.dataStream;

                outputStream.writeByte(OPCODE_COPY);
                outputStream.writeInt(copyOperation.getLength());

                return null;
            } catch (IOException e) {
                throw new ScriptEncodingException("Error writing copy operation to the script.", e);
            }
        }

        @Override
        public Void handleEnumMappingOperation(EnumMappingOperation enumMappingOperation) {
            try {
                DataOutputStream outputStream = this.dataStream;

                outputStream.writeByte(OPCODE_MAP_ENUM);
                outputStream.writeInt(enumMappingOperation.getEntryIndex());

                return null;
            } catch (IOException e) {
                throw new ScriptEncodingException("Error writing enum mapping operation to the script.", e);
            }
        }

        @Override
        public Void handleListMappingOperation(ListMappingOperation listMappingOperation) {
            try {
                DataOutputStream outputStream = this.dataStream;

                outputStream.writeByte(OPCODE_MAP_LIST);
                outputStream.writeInt(listMappingOperation.maxElements);
                outputStream.writeInt(listMappingOperation.sourceElementSize);
                outputStream.writeInt(listMappingOperation.targetElementSize);

                this.writeOperation(listMappingOperation.elementMappingOperation);

                return null;
            } catch (IOException e) {
                throw new ScriptEncodingException("Error writing list mapping operation to the script.", e);
            }
        }

        @Override
        public Void handleMonomorphicRecordMappingOperation(MonomorphicRecordMappingOperation recordMappingOperation) {
            try {
                DataOutputStream outputStream = this.dataStream;

                outputStream.writeByte(OPCODE_MAP_RECORD);
                outputStream.writeInt(recordMappingOperation.getEntryIndex());

                return null;
            } catch (IOException e) {
                throw new ScriptEncodingException("Error writing monomorphic record mapping operation to the script.", e);
            }
        }

        @Override
        public Void handleMonoToPolyRecordMappingOperation(MonoToPolyRecordMappingOperation recordMappingOperation) {
            try {
                DataOutputStream outputStream = this.dataStream;

                outputStream.writeByte(OPCODE_MAP_MONO_TO_POLY_RECORD);
                outputStream.writeInt(recordMappingOperation.getEntryIndex());

                return null;
            } catch (IOException e) {
                throw new ScriptEncodingException("Error writing mono-to-poly record mapping operation to the script.", e);
            }
        }

        @Override
        public Void handlePolyToMonoRecordMappingOperation(PolyToMonoRecordMappingOperation recordMappingOperation) {
            try {
                DataOutputStream outputStream = this.dataStream;

                outputStream.writeByte(OPCODE_MAP_POLY_TO_MONO_RECORD);

                outputStream.writeInt(recordMappingOperation.getEntryIndex());
                
                List<Integer> mappableIds = new ArrayList<>(recordMappingOperation.getMappableTypeIds());
                Collections.sort(mappableIds);

                outputStream.writeInt(mappableIds.size());
                for (Integer mappableId : mappableIds) {
                    outputStream.writeInt(mappableId);
                }

                return null;
            } catch (IOException e) {
                throw new ScriptEncodingException("Error writing poly-to-mono record mapping operation to the script.", e);
            }
        }

        @Override
        public Void handlePolymorphicRecordMappingOperation(PolymorphicRecordMappingOperation polymorphicRecordMappingOperation) {
            try {
                DataOutputStream outputStream = this.dataStream;

                outputStream.writeByte(OPCODE_MAP_POLYMORPHIC_RECORD);

                List<PolymorphicRecordMapping> mappings = new ArrayList<>(polymorphicRecordMappingOperation.getRecordMappings());
                outputStream.writeInt(mappings.size());
                for (PolymorphicRecordMapping mapping : mappings) {
                    this.writePolymorphicRecordMapping(mapping, outputStream);
                }

                return null;
            } catch (IOException e) {
                throw new ScriptEncodingException("Error writing polymorphic record mapping operation to the script.", e);
            }
        }

        private void writePolymorphicRecordMapping(PolymorphicRecordMapping mapping, DataOutputStream outputStream) throws IOException {
            outputStream.writeInt(mapping.getSourceTypeId());
            outputStream.writeInt(mapping.getTargetTypeId());
            outputStream.writeInt(mapping.getTypeEntry().getEntryIndex());
        }

        @Override
        public Void handleSkipOperation(SkipOperation skipOperation) {
            try {
                DataOutputStream outputStream = this.dataStream;

                outputStream.writeByte(OPCODE_SKIP);
                outputStream.writeInt(skipOperation.getAmount());

                return null;
            } catch (IOException e) {
                throw new ScriptEncodingException("Error writing skip operation to the script.", e);
            }
        }

    }

    private static class ScriptOffsets {

        public final int typeListOffset;

        public final int[] typeEntryOffsets;

        public final int operationListOffset;

        public final int[] operationEntryOffsets;

        public ScriptOffsets(int typeListOffset, int[] typeEntryOffsets, int operationListOffset, int[] operationEntryOffsets) {
            this.typeListOffset = typeListOffset;
            this.typeEntryOffsets = typeEntryOffsets;
            this.operationListOffset = operationListOffset;
            this.operationEntryOffsets = operationEntryOffsets;
        }

    }

    private static class ScriptEncodingException extends RuntimeException {

        private static final long serialVersionUID = 3095641281335853371L;

        public ScriptEncodingException(String message, Throwable cause) {
            super(message, cause);
        }

    }

}
