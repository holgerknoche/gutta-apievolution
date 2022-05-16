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
    
    default R handleRecordMappingOperation(RecordMappingOperation recordMappingOperation) {
        return null;
    }
    
    default R handlePolymorphicRecordMappingOperation(PolymorphicRecordMappingOperation polymorphicRecordMappingOperation) {
    	return null;
    }
    
}
