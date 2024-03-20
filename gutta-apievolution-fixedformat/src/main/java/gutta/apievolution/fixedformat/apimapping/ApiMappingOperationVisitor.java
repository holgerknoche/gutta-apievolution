package gutta.apievolution.fixedformat.apimapping;

interface ApiMappingOperationVisitor<R> {
    
    default R handleCopyOperation(CopyOperation copyOperation) {
        return null;
    }

    default R handleSkipOperation(SkipOperation skipOperation) {
        return null;
    }
    
    default R handleListMappingOperation(ListMappingOperation listMappingOperation) {
        return null;
    }
    
    default R handleEnumMappingOperation(EnumMappingOperation enumMappingOperation) {
        return null;
    }
    
    default R handleMonomorphicRecordMappingOperation(MonomorphicRecordMappingOperation recordMappingOperation) {
        return null;
    }
    
    default R handleMonoToPolyRecordMappingOperation(MonoToPolyRecordMappingOperation recordMappingOperation) {
        return null;
    }
    
    default R handlePolyToMonoRecordMappingOperation(PolyToMonoRecordMappingOperation recordMappingOperation) {
        return null;
    }
    
    default R handlePolymorphicRecordMappingOperation(PolymorphicRecordMappingOperation 
            polymorphicRecordMappingOperation) {
        return null;
    }
    
}
