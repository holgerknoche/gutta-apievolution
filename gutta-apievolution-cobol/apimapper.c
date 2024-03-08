#include <byteswap.h>
#include <fcntl.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include <sys/stat.h>
#include <sys/types.h>

#define TRUE 1
#define FALSE 0

#define SUCCESS 0
#define FAILURE 1

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
    OPCODE_MAP_POLYMORPHIC_RECORD = 0x06
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
    int length;
    void* basePtr;
    int* offsets;
} offsetlist;

typedef struct _RecordTypeEntry {
    int typeId;
    int dataSize;
    int numberOfFieldMappings;
} RecordTypeEntry;

typedef struct _DataBuffer {
    void* startPosition;
    void* currentPosition;
} DataBuffer;

typedef char byte;

static void* consumerScript = NULL;
static void* providerScript = NULL;

char errorMessage[80];

int convertStructureForOperation(void* sourceData, void* targetData, void* script, offsetlist typeList, offsetlist operationList, int operationIndex, int mappingType);

int performMappingOperation(DataBuffer* sourceData, DataBuffer* targetData, DataBuffer* scriptPosition, offsetlist typeList);

int reportError(char* message) {
    printf("Error: %s\n", message);
    return FAILURE;
}

int bigEndianIntToPlatform(int value) {
#if __BYTE_ORDER__ == __ORDER_LITTLE_ENDIAN__
    return bswap_32(value);
#else
    return value;
#endif
}

void* elementFromList(offsetlist list, int index) {
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

int readInt(DataBuffer* buffer) {
    int value = bigEndianIntToPlatform(*((int*) buffer->currentPosition));
    buffer->currentPosition += sizeof(int);
    
    return value;
}

void writeByte(DataBuffer* buffer, byte value) {
    *((byte*) buffer->currentPosition) = value;
    buffer->currentPosition += sizeof(byte);
}

void setToNull(DataBuffer* buffer, int amount) {
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

void skipData(DataBuffer *buffer, int amount) {
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
    printf("Converting using operation %d, direction %d.\n", operationIndex, direction);
#endif
    
    void* script;
    if (direction == CONSUMER_TO_PROVIDER) {
        script = consumerScript;
    } else {
        script = providerScript;
    }
    
    // Determine offsets for type and operation lists
    int* listPtr = (int*) script;
    int typeListOffset = bigEndianIntToPlatform(listPtr[0]);
    int operationListOffset = bigEndianIntToPlatform(listPtr[1]);
        
    // Prepare type and operation lists    
    offsetlist typeList;
    readList((void*) script + typeListOffset, script, &typeList);
    offsetlist operationList;
    readList((void*) script + operationListOffset, script, &operationList);
    
    return convertStructureForOperation(sourceData, targetData, script, typeList, operationList, operationIndex, mappingType);
}

int determineSizeOfMappingOperation(void* operationData) {
    // TODO
    return 0;
}

int writeNulls(DataBuffer *targetData, int amount) {
#ifdef DEBUG
    printf("Writing %d null bytes\n", amount);
#endif

    setToNull(targetData, amount);
    return SUCCESS;
}

int performCopyOperation(DataBuffer* sourceData, DataBuffer *targetData, DataBuffer* scriptPosition) {
    int bytesToCopy = readInt(scriptPosition);
#ifdef DEBUG
    printf("copy %d bytes\n", bytesToCopy);
#endif
    
    copyDataToBuffer(sourceData, (size_t) bytesToCopy, targetData);

    return SUCCESS;
}

int performSkipOperation(DataBuffer* targetData, DataBuffer* scriptPosition) {
    int bytesToSkip = readInt(scriptPosition);
#ifdef DEBUG
    printf("skip %d bytes\n", bytesToSkip);
#endif
    
    skipData(targetData, bytesToSkip);
    
    return SUCCESS;
}

int mapField(DataBuffer* sourceData, DataBuffer* targetData, DataBuffer* scriptPosition, offsetlist typeList) {
    int sourceFieldOffset = readInt(scriptPosition);        
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
    int numberOfFieldMappings = bigEndianIntToPlatform(typeEntry->numberOfFieldMappings);
#ifdef DEBUG
    printf("Number of field mappings: %d\n", numberOfFieldMappings);
#endif    
    
    for (int fieldIndex = 0; fieldIndex < numberOfFieldMappings; fieldIndex++) {
        if (mapField(sourceData, targetData, scriptPosition, typeList) != SUCCESS) {
            return FAILURE;
        }
    }

    return SUCCESS;
}

int performMapRecordOperation(DataBuffer* sourceData, DataBuffer* targetData, DataBuffer* scriptPosition, offsetlist typeList) {
    int typeIndex = readInt(scriptPosition);
    void* savedPosition = getCurrentPosition(scriptPosition);
 
#ifdef DEBUG
    printf("Mapping record type index %d.\n", typeIndex);
#endif 
    
    // Obtain a pointer to the referenced type entry
    void* currentPosition = elementFromList(typeList, typeIndex);
    if (currentPosition == NULL) {
        return FAILURE;
    }

    byte entryType = *((byte*) currentPosition);
    currentPosition += sizeof(byte);
    if (entryType != ENTRY_TYPE_RECORD) {
        snprintf(errorMessage, sizeof(errorMessage), "Unexpected entry type %d in record mapping.", (int) entryType);
        return reportError(errorMessage);
    }
    
    RecordTypeEntry* typeEntry = (RecordTypeEntry*) currentPosition;    
    currentPosition += sizeof(RecordTypeEntry);

    byte flags = readByte(sourceData);
    int result;

    switch (flags) {
    case VALUE_ABSENT:
    case VALUE_UNREPRESENTABLE:
        writeByte(targetData, flags);
        
        int dataSize = bigEndianIntToPlatform(typeEntry->dataSize);
        result = writeNulls(targetData, dataSize);
        break;
    
    case VALUE_PRESENT:
        writeByte(targetData, VALUE_PRESENT);
    
        // Adjust the script position
        setCurrentPosition(scriptPosition, currentPosition);
    
        DataBuffer newSourceData;
        setStartPosition(&newSourceData, getCurrentPosition(sourceData));
        
        result = mapRecordFields(&newSourceData, targetData, typeEntry, scriptPosition, typeList);
        break;
    
    default:
        snprintf(errorMessage, sizeof(errorMessage), "Unsupported value flags %d.", (int) flags);
        result = reportError(errorMessage);
        break;
    }
    
    // Restore the script position if necessary
    setCurrentPosition(scriptPosition, savedPosition);
    return result;
}

int performMappingOperation(DataBuffer* sourceData, DataBuffer* targetData, DataBuffer* scriptPosition, offsetlist typeList) {
    byte opcode = readByte(scriptPosition);
    
    switch (opcode) {
        case OPCODE_COPY:
            return performCopyOperation(sourceData, targetData, scriptPosition);
            
        case OPCODE_SKIP:
            return performSkipOperation(targetData, scriptPosition);
    
        case OPCODE_MAP_RECORD:
            return performMapRecordOperation(sourceData, targetData, scriptPosition, typeList);
        
        default:
            snprintf(errorMessage, sizeof(errorMessage), "Invalid opcode %d.", (int) opcode);
            return reportError(errorMessage);
    }

    return SUCCESS;
}

int convertStructureForOperation(void* sourceData, void* targetData, void* script, offsetlist typeList, offsetlist operationList, int operationIndex, int mappingType) {
    
    void* currentPosition = elementFromList(operationList, operationIndex); 
    if (currentPosition == NULL) {
        return FAILURE;
    }
    
    int nameLength = bigEndianIntToPlatform(*((int*) currentPosition));
    currentPosition += sizeof(int);    
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
        int parameterOperationSize = determineSizeOfMappingOperation(currentPosition);
        skipData(&scriptPosition, parameterOperationSize);
        
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
}

