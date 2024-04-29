#include <byteswap.h>
#include <fcntl.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include <sys/stat.h>
#include <sys/types.h>

#define SUCCESS 0
#define FAILURE 1

typedef char byte;
typedef int i32;

enum {
    CONSUMER_TO_PROVIDER = 0,
    PROVIDER_TO_CONSUMER = 1
} MappingDirection;

enum {
    PARAMETER = 0,
    RESULT = 1
} MappingType;

enum {
    OPCODE_COPY = 0x01,
    OPCODE_SKIP = 0x02,
    OPCODE_MAP_ENUM = 0x03,
    OPCODE_MAP_RECORD = 0x04,
    OPCODE_MAP_LIST = 0x05,
    OPCODE_MAP_POLYMORPHIC_RECORD = 0x06,
    OPCODE_MAP_MONO_TO_POLY_RECORD = 0x07,
    OPCODE_MAP_POLY_TO_MONO_RECORD = 0x08
} Opcode;

enum {
    ENTRY_TYPE_ENUM = 0x01,
    ENTRY_TYPE_RECORD = 0x02
} EntryType;

enum {
    VALUE_ABSENT = 0,
    VALUE_PRESENT = 1,
    VALUE_UNREPRESENTABLE = 2
} ValueFlags;

typedef struct _offsetlist {
    i32 length;
    void* basePtr;
    i32* offsets;
} offsetlist;

typedef struct _RecordTypeEntry {
    i32 typeId;
    i32 dataSize;
    i32 numberOfFieldMappings;
} RecordTypeEntry;

typedef struct _PolymorphicRecordMapping {
    i32 sourceTypeId;
    i32 targetTypeId;
    i32 typeEntryIndex;
} PolymorphicRecordMapping;

typedef struct _DataBuffer {
    void* startPosition;
    void* currentPosition;
} DataBuffer;

static void* consumerScript = NULL;
static void* providerScript = NULL;

char errorMessage[80];

int convertStructureForOperation(void* sourceData, void* targetData, void* script, offsetlist typeList, offsetlist operationList, int operationIndex, int mappingType);

int performMappingOperation(DataBuffer* sourceData, DataBuffer* targetData, DataBuffer* scriptPosition, offsetlist typeList);

int reportError(char* message) {
    printf("Error: %s\n", message);
    return FAILURE;
}

i32 bigEndianIntToPlatform(i32 value) {
#if __BYTE_ORDER__ == __ORDER_LITTLE_ENDIAN__
    return bswap_32(value);
#else
    return value;
#endif
}

i32 platformIntToBigEndian(i32 value) {
#if __BYTE_ORDER__ == __ORDER_LITTLE_ENDIAN__
    return bswap_32(value);
#else
    return value;
#endif
}

void* elementFromList(offsetlist list, i32 index) {
    if (index >= list.length) {
        snprintf(errorMessage, sizeof(errorMessage), "Invalid index %d (max index is %d).", index, list.length);
        reportError(errorMessage);
        return NULL;
    }
    
    int offset = bigEndianIntToPlatform(list.offsets[index]);
    return (void*) (list.basePtr + offset);
}

byte readByte(DataBuffer* buffer) {
    byte value = *((byte*) buffer->currentPosition);
    buffer->currentPosition += sizeof(byte);
    
    return value;
}

i32 readInt32(DataBuffer* buffer) {
    i32 value = bigEndianIntToPlatform(*((i32*) buffer->currentPosition));
    buffer->currentPosition += sizeof(i32);
    
    return value;
}

void* readStruct(DataBuffer* buffer, size_t length) {
    void* value = buffer->currentPosition;
    buffer->currentPosition += length;
    
    return value;
}

void writeByte(DataBuffer* buffer, byte value) {
    *((byte*) buffer->currentPosition) = value;
    buffer->currentPosition += sizeof(byte);
}

void writeInt32(DataBuffer* buffer, i32 value) {
    *((i32*) buffer->currentPosition) = platformIntToBigEndian(value);
    buffer->currentPosition += sizeof(i32);
}

void writeInt32BigEndian(DataBuffer* buffer, i32 value) {
    *((i32*) buffer->currentPosition) = value;
    buffer->currentPosition += sizeof(i32);
}

void setToNull(DataBuffer* buffer, i32 amount) {
    memset(buffer->currentPosition, 0, (size_t) amount);
    buffer->currentPosition += amount;
}

void copyDataToBuffer(DataBuffer* source, size_t size, DataBuffer* target) {
    memcpy(target->currentPosition, source->currentPosition, size);
    target->currentPosition += size;
    source->currentPosition += size;
}

void setStartPosition(DataBuffer *buffer, void* position) {
    buffer->startPosition = position;
    buffer->currentPosition = position;
}

void skipData(DataBuffer *buffer, i32 amount) {
    buffer->currentPosition += amount;
}

void moveToOffset(DataBuffer* buffer, int offset) {
    buffer->currentPosition = (void*) (buffer->startPosition + offset);
}

void setCurrentPosition(DataBuffer *buffer, void* position) {
    buffer->currentPosition = position;
}

void* getCurrentPosition(DataBuffer* buffer) {
    return buffer->currentPosition;
}

int apimapper() {
#ifdef DEBUG
    printf("Initializing API mapper\n");
#endif
    
    return SUCCESS;
}

void cobolToC(char* source, char* target, int maxLength) {
    char* sourcePosition = (char*) (source + maxLength - 1);
    char* targetPosition = (char*) (target + maxLength);

    // Add a null char at the end of the C string 
    *targetPosition = '\0';
    targetPosition--;

    // Replace all trailing spaces with null chars 
    while (sourcePosition >= source) {
        char currentCharacter = *sourcePosition;
                
        if (currentCharacter == ' ') {
            *targetPosition = '\0';
        } else {
            break;
        }
        
        sourcePosition--;
        targetPosition--;
    }
    
    // Copy the remaining characters as-is
    while (sourcePosition >= source) {
        *targetPosition = *sourcePosition;

        sourcePosition--;
        targetPosition--;
    }
}

int loadScript(char* scriptName, void** scriptLocation) {
    struct stat fileStats;
    int returnCode;
    size_t scriptSize;
    size_t bytesRead;
    size_t remainingBytes;
    FILE* inputFile;
    void* scriptData;
        
    returnCode = stat(scriptName, &fileStats);
    if (returnCode != SUCCESS) {
        return reportError("Error determining script size.");
    }

    // Determine the required buffer size and allocate the buffer
    scriptSize = fileStats.st_size;
    scriptData = malloc(scriptSize);
    if (scriptData == NULL) {
        return reportError("Error allocating memory for the script."); 
    }
    
    inputFile = fopen(scriptName, "rb");
    if (inputFile == NULL) {
        return reportError("Error opening script file.");
    }
    
    bytesRead = fread(scriptData, 1, scriptSize, inputFile);    
    fclose(inputFile);
    
    if (bytesRead != scriptSize) {
        return reportError("Could not read entire script.");
    }
    
    *scriptLocation = scriptData;
    return SUCCESS;
}

int loadScripts(char* givenConsumerScriptName, char* givenProviderScriptName) {
    char consumerScriptName[31];
    char providerScriptName[31];
    int returnCode;

    cobolToC(givenConsumerScriptName, consumerScriptName, 30);
    cobolToC(givenProviderScriptName, providerScriptName, 30);

#ifdef DEBUG
    printf("cs: %s, ps: %s\n", consumerScriptName, providerScriptName);
#endif

    returnCode = loadScript(consumerScriptName, &consumerScript);
    if (returnCode != SUCCESS) {
        return reportError("Error loading consumer script.");
    }
    
    returnCode = loadScript(providerScriptName, &providerScript);
    if (returnCode != SUCCESS) {
        return reportError("Error loading provider script.");
    }
    
#ifdef DEBUG
    printf("cs at %p, ps at %p\n", consumerScript, providerScript);
#endif

    return SUCCESS;
}

int readList(void* position, void* basePtr, offsetlist* list) {
    int* currentPosition = position;
    
    list->length = bigEndianIntToPlatform(*currentPosition);
    currentPosition++;
    list->basePtr = basePtr;
    list->offsets = (int*) currentPosition;
    
    return SUCCESS;
}

int convertData(int operationIndex, int direction, int mappingType, void* sourceData, void* targetData) {
#ifdef DEBUG
    printf("Converting using operation %d, direction %d, from %p to %p..\n", operationIndex, direction, sourceData, targetData);
#endif

    void* script;
    if (direction == CONSUMER_TO_PROVIDER) {
        script = consumerScript;
    } else {
        script = providerScript;
    }
    
    // Determine offsets for type and operation lists
    i32* listPtr = (i32*) script;
    i32 typeListOffset = bigEndianIntToPlatform(listPtr[0]);
    i32 operationListOffset = bigEndianIntToPlatform(listPtr[1]);
        
    // Prepare type and operation lists    
    offsetlist typeList;
    readList((void*) script + typeListOffset, script, &typeList);
    offsetlist operationList;
    readList((void*) script + operationListOffset, script, &operationList);

    return convertStructureForOperation(sourceData, targetData, script, typeList, operationList, operationIndex, mappingType);
}

int skipMappingOperation(DataBuffer* scriptPosition) {
    byte opcode = readByte(scriptPosition);
    
    switch (opcode) {
    case OPCODE_COPY:
        // A copy operation only consists of the number of bytes to copy
        skipData(scriptPosition, sizeof(i32));
        break;
    
    case OPCODE_SKIP:
        // A skip operation only consists of the number of bytes to skip
        skipData(scriptPosition, sizeof(i32));
        break;
        
    case OPCODE_MAP_ENUM:
        // An enum mapping operation only consists of the index of the type entry
        skipData(scriptPosition, sizeof(i32));
        break;
    
    case OPCODE_MAP_RECORD:
        // A record mapping operation only consists of the index of the type entry
        skipData(scriptPosition, sizeof(i32));
        break;
        
    case OPCODE_MAP_LIST:
        // A list mapping operation consists of three i32 values plus the element mapping operation
        skipData(scriptPosition, 3 * sizeof(i32));
        skipMappingOperation(scriptPosition);
        break;
    
    case OPCODE_MAP_POLYMORPHIC_RECORD:
        // A polymorphic record mapping consists of a list of record mappings
        i32 numberOfMappings = readInt32(scriptPosition);
        skipData(scriptPosition, numberOfMappings * sizeof(PolymorphicRecordMapping));
        break;
        
    case OPCODE_MAP_MONO_TO_POLY_RECORD:
        // Mono-to-poly mapping consists only of the target index
        skipData(scriptPosition, sizeof(i32));
        break;
    
    case OPCODE_MAP_POLY_TO_MONO_RECORD:
        // Poly-to-mono mapping consists of the target index as well as alist of type ids
        skipData(scriptPosition, sizeof(i32));
        i32 numberOfIds = readInt32(scriptPosition);
        skipData(scriptPosition, (numberOfIds * sizeof(i32)));
        break;
    
    default:
        snprintf(errorMessage, sizeof(errorMessage), "Unsupported opcode %d to skip.", (int) opcode);
        return reportError(errorMessage);
    }
    
    return SUCCESS;
}

int writeNulls(DataBuffer *targetData, i32 amount) {
#ifdef DEBUG
    printf("Writing %d null bytes at %p.\n", amount, getCurrentPosition(targetData));
#endif

    setToNull(targetData, amount);
    return SUCCESS;
}

int performCopyOperation(DataBuffer* sourceData, DataBuffer *targetData, DataBuffer* scriptPosition) {
    i32 bytesToCopy = readInt32(scriptPosition);
#ifdef DEBUG
    printf("copy %d bytes\n", bytesToCopy);
#endif
    
    copyDataToBuffer(sourceData, (size_t) bytesToCopy, targetData);

    return SUCCESS;
}

int performSkipOperation(DataBuffer* targetData, DataBuffer* scriptPosition) {
    i32 bytesToSkip = readInt32(scriptPosition);
#ifdef DEBUG
    printf("skip %d bytes\n", bytesToSkip);
#endif
    
    writeNulls(targetData, bytesToSkip);
    
    return SUCCESS;
}

int performMapEnumOperation(DataBuffer* sourceData, DataBuffer* targetData, DataBuffer* scriptPosition, offsetlist typeList) {
    i32 typeIndex = readInt32(scriptPosition);
    void* savedPosition = getCurrentPosition(scriptPosition);
    
#ifdef DEBUG
    printf("Mapping enum type with index %d\n", typeIndex);
#endif

    // Obtain a pointer to the referenced type entry
    void* typeEntryPtr = elementFromList(typeList, typeIndex);
    if (typeEntryPtr == NULL) {
        return FAILURE;
    }

    // Adjust the script position
    setCurrentPosition(scriptPosition, typeEntryPtr);
    
    byte entryType = readByte(scriptPosition);
    if (entryType != ENTRY_TYPE_ENUM) {
        snprintf(errorMessage, sizeof(errorMessage), "Unexpected entry type %d in enum mapping.", (int) entryType);
        return reportError(errorMessage);
    }

    int result;
    byte flags = readByte(sourceData);

    switch (flags) {
    case VALUE_ABSENT:
    case VALUE_UNREPRESENTABLE:
        writeByte(targetData, flags);
        writeByte(targetData, 0);
        result = SUCCESS;
        break;
        
    case VALUE_PRESENT:
        i32 sourceValue = readInt32(sourceData);
        
        // Skip the type id
        readInt32(scriptPosition);
        i32 numberOfValues = readInt32(scriptPosition);
        
#ifdef DEBUG
        printf("Mapping value %d\n", sourceValue);
#endif
        
        if (sourceValue > numberOfValues) {
            snprintf(errorMessage, sizeof(errorMessage), "Unsupported enum value %d.", (int) sourceValue);
            return reportError(errorMessage);    
        }
        
        i32* mappedValues = (i32*) getCurrentPosition(scriptPosition);
        i32 mappedValue = bigEndianIntToPlatform(mappedValues[sourceValue]);
        
        if (mappedValue >= 0) {
#ifdef DEBUG
            printf("Writing mapped value %d\n", mappedValue);
#endif
            writeByte(targetData, VALUE_PRESENT);
            writeInt32(targetData, mappedValue);
        } else {
            writeByte(targetData, VALUE_UNREPRESENTABLE);
            writeInt32(targetData, 0);
        }
        
        result = SUCCESS;
        break;

    default:
        snprintf(errorMessage, sizeof(errorMessage), "Unsupported value flags %d.", (int) flags);
        result = reportError(errorMessage);
        break;
    }

    // Restore the script position
    setCurrentPosition(scriptPosition, savedPosition);
    return result;
}

int mapField(DataBuffer* sourceData, DataBuffer* targetData, DataBuffer* scriptPosition, offsetlist typeList) {
    i32 sourceFieldOffset = readInt32(scriptPosition);        
#ifdef DEBUG
    printf("Field mapping at source offset: %d\n", sourceFieldOffset);
#endif
    
    moveToOffset(sourceData, sourceFieldOffset);    
    if (performMappingOperation(sourceData, targetData, scriptPosition, typeList) != SUCCESS) {
        return FAILURE;
    }
    
    return SUCCESS;
}

int mapRecordFields(DataBuffer* sourceData, DataBuffer* targetData, RecordTypeEntry* typeEntry, DataBuffer* scriptPosition, offsetlist typeList) {
    i32 numberOfFieldMappings = bigEndianIntToPlatform(typeEntry->numberOfFieldMappings);
#ifdef DEBUG
    printf("Number of field mappings: %d\n", numberOfFieldMappings);
#endif    
    
    for (i32 fieldIndex = 0; fieldIndex < numberOfFieldMappings; fieldIndex++) {
        if (mapField(sourceData, targetData, scriptPosition, typeList) != SUCCESS) {
            return FAILURE;
        }
    }

    return SUCCESS;
}

RecordTypeEntry* getTypeEntry(i32 typeIndex, DataBuffer* scriptPosition, offsetlist typeList) {
    // Obtain a pointer to the referenced type entry
    void* typeEntryPtr = elementFromList(typeList, typeIndex);
    if (typeEntryPtr == NULL) {
        return NULL;
    }

    // Adjust the script position
    setCurrentPosition(scriptPosition, typeEntryPtr);

    byte entryType = readByte(scriptPosition);
    if (entryType != ENTRY_TYPE_RECORD) {
        snprintf(errorMessage, sizeof(errorMessage), "Unexpected entry type %d in record mapping.", (int) entryType);
        reportError(errorMessage);
        return NULL;
    }
    
    return (RecordTypeEntry*) readStruct(scriptPosition, sizeof(RecordTypeEntry));
}

int performMapRecordOperation(DataBuffer* sourceData, DataBuffer* targetData, DataBuffer* scriptPosition, offsetlist typeList) {
    i32 typeIndex = readInt32(scriptPosition);
    void* savedPosition = getCurrentPosition(scriptPosition);
 
#ifdef DEBUG
    printf("Mapping record type index %d.\n", typeIndex);
#endif 
    
    RecordTypeEntry* typeEntry = getTypeEntry(typeIndex, scriptPosition, typeList);
    if (typeEntry == NULL) {
        return FAILURE;
    }
    
    byte flags = readByte(sourceData);
    int result;

    switch (flags) {
    case VALUE_ABSENT:
    case VALUE_UNREPRESENTABLE:
        writeByte(targetData, flags);
        
        i32 dataSize = bigEndianIntToPlatform(typeEntry->dataSize);
        result = writeNulls(targetData, dataSize);
        break;
    
    case VALUE_PRESENT:
        writeByte(targetData, VALUE_PRESENT);
        
        DataBuffer newSourceData;
        setStartPosition(&newSourceData, getCurrentPosition(sourceData));
        
        result = mapRecordFields(&newSourceData, targetData, typeEntry, scriptPosition, typeList);
        break;
    
    default:
        snprintf(errorMessage, sizeof(errorMessage), "Unsupported value flags %d.", (int) flags);
        result = reportError(errorMessage);
        break;
    }
    
    // Restore the script position
    setCurrentPosition(scriptPosition, savedPosition);
    return result;
}

int performListMappingOperation(DataBuffer* sourceData, DataBuffer* targetData, DataBuffer* scriptPosition, offsetlist typeList) {
    int returnCode;

    i32 maxElementCount = readInt32(scriptPosition);
    i32 sourceElementSize = readInt32(scriptPosition);
    i32 targetElementSize = readInt32(scriptPosition);
    
#ifdef DEBUG
    printf("List mapping: max elements %d, source size %d, target size %d\n", maxElementCount, sourceElementSize, targetElementSize);
#endif

    byte listFlags = readByte(sourceData);
    if (listFlags != VALUE_PRESENT) {
        // If no value is present, copy the flags as-is and set the target data to null
        writeByte(targetData, listFlags);
        // Set target element count to zero
        writeInt32(targetData, 0);      
        writeNulls(targetData, (maxElementCount * targetElementSize));
    } else {
        // If a value is present, convert values individually using the given operation
        i32 actualElementCount = readInt32(sourceData);
#ifdef DEBUG
        printf("Actual element count: %d\n", actualElementCount);
#endif
        writeByte(targetData, VALUE_PRESENT);
        writeInt32(targetData, actualElementCount);

        // Save the pointer to the mapping operation, as we to reset it for each element
        void* operationPtr = getCurrentPosition(scriptPosition);
        void* currentSourceElementPtr = getCurrentPosition(sourceData);
        for (i32 elementIndex = 0; elementIndex < actualElementCount; elementIndex++) {
#ifdef DEBUG
        printf("Mapping element %d, %p->%p\n", elementIndex, getCurrentPosition(sourceData), getCurrentPosition(targetData));
#endif
            setCurrentPosition(scriptPosition, operationPtr);            
            // Map the individual elements
            returnCode = performMappingOperation(sourceData, targetData, scriptPosition, typeList);
            if (returnCode != SUCCESS) {
                return FAILURE;
            }
            
            // Position the source buffer at the beginning of the next element
            currentSourceElementPtr += sourceElementSize;
            setCurrentPosition(sourceData, currentSourceElementPtr);
        }
        
        int remainingElements = maxElementCount - actualElementCount;
        writeNulls(targetData, (remainingElements * targetElementSize));
    }

    return SUCCESS;
}

int performMonoToPolyMappingOperation(DataBuffer* sourceData, DataBuffer* targetData, DataBuffer* scriptPosition, offsetlist typeList) {
    i32 typeIndex = readInt32(scriptPosition);
    void* savedPosition = getCurrentPosition(scriptPosition);
    
    RecordTypeEntry* typeEntry = getTypeEntry(typeIndex, scriptPosition, typeList);
    if (typeEntry == NULL) {
        return FAILURE;
    }
    
    int targetTypeId = bigEndianIntToPlatform(typeEntry->typeId);

#ifdef DEBUG
    printf("Mapping type index %d to type id %d\n", typeIndex, targetTypeId);
#endif

    byte flags = readByte(sourceData);
    int result;

    switch (flags) {
    case VALUE_ABSENT:
    case VALUE_UNREPRESENTABLE:
        writeByte(targetData, flags);
        
        i32 dataSize = bigEndianIntToPlatform(typeEntry->dataSize);
        result = writeNulls(targetData, dataSize);
        break;
    
    case VALUE_PRESENT:
        writeByte(targetData, VALUE_PRESENT);
        writeInt32(targetData, targetTypeId);
        
        DataBuffer newSourceData;
        setStartPosition(&newSourceData, getCurrentPosition(sourceData));
        
        result = mapRecordFields(&newSourceData, targetData, typeEntry, scriptPosition, typeList);
        break;
    
    default:
        snprintf(errorMessage, sizeof(errorMessage), "Unsupported value flags %d.", (int) flags);
        result = reportError(errorMessage);
        break;
    }
    
    // Restore the script position
    setCurrentPosition(scriptPosition, savedPosition);
    return result;
}

int performPolyToMonoMappingOperation(DataBuffer* sourceData, DataBuffer* targetData, DataBuffer* scriptPosition, offsetlist typeList) {
    i32 typeEntryIndex = readInt32(scriptPosition);
    i32 numberOfTypeIds = readInt32(scriptPosition);
    void* typeMappingPosition = getCurrentPosition(scriptPosition);
    void* scriptPositionAfterOperation = typeMappingPosition + (numberOfTypeIds * sizeof(i32));

    RecordTypeEntry* typeEntry = getTypeEntry(typeEntryIndex, scriptPosition, typeList);

    byte flags = readByte(sourceData);
    int result;
    
    switch (flags) {
    case VALUE_ABSENT:
    case VALUE_UNREPRESENTABLE:
        writeByte(targetData, flags);
        
        i32 dataSize = bigEndianIntToPlatform(typeEntry->dataSize);
        result = writeNulls(targetData, dataSize);
        break;
    
    case VALUE_PRESENT:
        i32 sourceTypeId = readInt32(sourceData);

#ifdef DEBUG
    printf("Mapping poly-to-mono record for type id %d\n", sourceTypeId);
#endif
        // Save the current script position, which points to the field mapping operations
        // to be performed
        void* operationPtr = getCurrentPosition(scriptPosition);
        
        // Set the script position to where the admissible type ids are stored
        setCurrentPosition(scriptPosition, typeMappingPosition);
        
        // Check whether the id is mappable
        bool mappableId = false;
        for (i32 typeIndex = 0; typeIndex < numberOfTypeIds; typeIndex++) {
            i32 currentTypeId = readInt32(scriptPosition);
                        
            if (currentTypeId == sourceTypeId) {
                mappableId = true;
                break;
            }
        }
    
        if (mappableId) {
            // If the ID is mappable, proceed as for a monomorphic type
            writeByte(targetData, VALUE_PRESENT);
        
            DataBuffer newSourceData;
            setStartPosition(&newSourceData, getCurrentPosition(sourceData));
            
            // Restore the script position to the field mapping operation
            setCurrentPosition(scriptPosition, operationPtr);
        
            result = mapRecordFields(&newSourceData, targetData, typeEntry, scriptPosition, typeList);
        } else {
            // If the ID is not mappable, proceed as for an unmapped value
            writeByte(targetData, VALUE_UNREPRESENTABLE);
        
            i32 dataSize = bigEndianIntToPlatform(typeEntry->dataSize);
            result = writeNulls(targetData, dataSize);
        }
        break;
    
    default:
        snprintf(errorMessage, sizeof(errorMessage), "Unsupported value flags %d.", (int) flags);
        result = reportError(errorMessage);
        break;
    }

    setCurrentPosition(scriptPosition, scriptPositionAfterOperation);
    return result;
}

i32 determineMaxTargetSize(i32 numberOfMappings, PolymorphicRecordMapping* mappings, offsetlist typeList) {
    i32 maxSize = 0;
    DataBuffer tempBuffer;

    for (i32 mappingIndex = 0; mappingIndex < numberOfMappings; mappingIndex++) {
        PolymorphicRecordMapping mapping = mappings[mappingIndex];
        i32 typeIndex = bigEndianIntToPlatform(mapping.typeEntryIndex);
        
        void* typeEntryPtr = elementFromList(typeList, typeIndex);
        if (typeEntryPtr == NULL) {
            continue;
        }
        
        setCurrentPosition(&tempBuffer, typeEntryPtr);
        byte entryType = readByte(&tempBuffer);
        if (entryType != ENTRY_TYPE_RECORD) {
            continue;
        }
        
        RecordTypeEntry* recordEntry = (RecordTypeEntry*) getCurrentPosition(&tempBuffer);
        i32 typeSize = bigEndianIntToPlatform(recordEntry->dataSize);
                
        if (typeSize > maxSize) {
            maxSize = typeSize;
        }
    }
  
#ifdef DEBUG
    printf("Max size of poly type is %d bytes.\n", maxSize);
#endif
  
    return maxSize;
}

PolymorphicRecordMapping* findMatchingMapping(i32 sourceTypeId, i32 numberOfMappings, PolymorphicRecordMapping* mappings) {
    for (i32 mappingIndex = 0; mappingIndex < numberOfMappings; mappingIndex++) {
        PolymorphicRecordMapping mapping = mappings[mappingIndex];
        
        i32 currentSourceTypeId = bigEndianIntToPlatform(mapping.sourceTypeId);
        if (currentSourceTypeId == sourceTypeId) {
            return &mappings[mappingIndex];
        }
    }
    
    return NULL;
}

int performMapPolymorphicRecordOperation(DataBuffer* sourceData, DataBuffer* targetData, DataBuffer* scriptPosition, offsetlist typeList) {
#ifdef DEBUG
    printf("Poly mapping operation at %p.\n", getCurrentPosition(scriptPosition));
#endif
    
    i32 numberOfMappings = readInt32(scriptPosition);
    void* currentScriptPosition = getCurrentPosition(scriptPosition);
    PolymorphicRecordMapping* mappings = (PolymorphicRecordMapping*) currentScriptPosition;
    void* scriptPositionAfterOperation = currentScriptPosition + (numberOfMappings * sizeof(PolymorphicRecordMapping));

    int result = SUCCESS;
    byte flags = readByte(sourceData);
    
    switch (flags) {
    case VALUE_ABSENT:
    case VALUE_UNREPRESENTABLE:
        // If no (representable) value is present, copy the flags as-is, set the type id to zero and
        // fill the data area with nulls
        writeByte(targetData, flags);
        writeInt32(targetData, 0);
        i32 maxSize = determineMaxTargetSize(numberOfMappings, mappings, typeList);
        writeNulls(targetData, maxSize);        
        break;
    
    case VALUE_PRESENT:
        i32 sourceTypeId = readInt32(sourceData);
#ifdef DEBUG
        printf("Mapping source type id %d\n", sourceTypeId);
#endif
        PolymorphicRecordMapping* mapping = findMatchingMapping(sourceTypeId, numberOfMappings, mappings);
        if (mapping != NULL) {
            // Corresponding mapping found => value is representable
            writeByte(targetData, VALUE_PRESENT);
            writeInt32BigEndian(targetData, mapping->targetTypeId);
                        
            // Resolve the type entry id and map the fields 
            i32 typeIndex = bigEndianIntToPlatform(mapping->typeEntryIndex);
            void* typeEntryPtr = elementFromList(typeList, typeIndex);
            if (typeEntryPtr == NULL) {
                return FAILURE;
            }

            // Adjust the script position
            setCurrentPosition(scriptPosition, typeEntryPtr);

            byte entryType = readByte(scriptPosition);
            if (entryType != ENTRY_TYPE_RECORD) {
                snprintf(errorMessage, sizeof(errorMessage), "Unexpected entry type %d in polymorphic record mapping.", (int) entryType);
                return reportError(errorMessage);
            }
    
            RecordTypeEntry* typeEntry = (RecordTypeEntry*) readStruct(scriptPosition, sizeof(RecordTypeEntry));    

            DataBuffer newSourceData;
            setStartPosition(&newSourceData, getCurrentPosition(sourceData));
        
            result = mapRecordFields(&newSourceData, targetData, typeEntry, scriptPosition, typeList);
            
            // Pad the remainder of the record with nulls, if necessary
            i32 maxSize = determineMaxTargetSize(numberOfMappings, mappings, typeList);
            i32 actualSize = bigEndianIntToPlatform(typeEntry->dataSize);

            if (actualSize < maxSize) {
                writeNulls(targetData, (maxSize - actualSize));
            }
        } else {
            // No corresponding mapping => value is unrepresentable
            writeByte(targetData, VALUE_UNREPRESENTABLE);
            writeInt32(targetData, 0);
            i32 maxSize = determineMaxTargetSize(numberOfMappings, mappings, typeList);
            writeNulls(targetData, maxSize);
        }
        break;
    
    default:
        snprintf(errorMessage, sizeof(errorMessage), "Unsupported value flags %d.", (int) flags);
        return reportError(errorMessage);
        break;
    }

    // Move the script position to the byte after the mapping operation
    setCurrentPosition(scriptPosition, scriptPositionAfterOperation);
    return result;
}

int performMappingOperation(DataBuffer* sourceData, DataBuffer* targetData, DataBuffer* scriptPosition, offsetlist typeList) {
    byte opcode = readByte(scriptPosition);
    
    switch (opcode) {
        case OPCODE_COPY:
            return performCopyOperation(sourceData, targetData, scriptPosition);
            
        case OPCODE_SKIP:
            return performSkipOperation(targetData, scriptPosition);
            
        case OPCODE_MAP_ENUM:
            return performMapEnumOperation(sourceData, targetData, scriptPosition, typeList);
    
        case OPCODE_MAP_RECORD:
            return performMapRecordOperation(sourceData, targetData, scriptPosition, typeList);
        
        case OPCODE_MAP_LIST:
            return performListMappingOperation(sourceData, targetData, scriptPosition, typeList);
            
        case OPCODE_MAP_POLYMORPHIC_RECORD:
            return performMapPolymorphicRecordOperation(sourceData, targetData, scriptPosition, typeList);
            
        case OPCODE_MAP_MONO_TO_POLY_RECORD:
            return performMonoToPolyMappingOperation(sourceData, targetData, scriptPosition, typeList);
        
        case OPCODE_MAP_POLY_TO_MONO_RECORD:
            return performPolyToMonoMappingOperation(sourceData, targetData, scriptPosition, typeList);
        
        default:
            snprintf(errorMessage, sizeof(errorMessage), "Invalid opcode %d at %p.", (int) opcode, getCurrentPosition(scriptPosition) - 1);
            return reportError(errorMessage);
    }

    return SUCCESS;
}

int convertStructureForOperation(void* sourceData, void* targetData, void* script, offsetlist typeList, offsetlist operationList, int operationIndex, int mappingType) {
    
    void* currentPosition = elementFromList(operationList, operationIndex); 
    if (currentPosition == NULL) {
        return FAILURE;
    }
    
    i32 nameLength = bigEndianIntToPlatform(*((int*) currentPosition));
    currentPosition += sizeof(i32);    
    // Skip the name
    currentPosition += nameLength;

    DataBuffer scriptPosition;
    setStartPosition(&scriptPosition, currentPosition);
    
    DataBuffer sourceBuffer;
    setStartPosition(&sourceBuffer, sourceData);
    
    DataBuffer targetBuffer;
    setStartPosition(&targetBuffer, targetData);
    
    if (mappingType == PARAMETER) {
        // The current position is already at the correct location for parameter mapping
        return performMappingOperation(&sourceBuffer, &targetBuffer, &scriptPosition, typeList);
    } else {
        // For result mapping, we need to skip the parameter mapping operation
        skipMappingOperation(&scriptPosition);
        
        return performMappingOperation(&sourceBuffer, &targetBuffer, &scriptPosition, typeList);
    }
}

int unloadScripts() {
#ifdef DEBUG
     printf("Unloading scripts.\n");
#endif

    if (consumerScript != NULL) {
        free(consumerScript);
        consumerScript = NULL;
    }
    if (providerScript != NULL) {
        free(providerScript);
        providerScript = NULL;
    }
    
    return SUCCESS;
}

