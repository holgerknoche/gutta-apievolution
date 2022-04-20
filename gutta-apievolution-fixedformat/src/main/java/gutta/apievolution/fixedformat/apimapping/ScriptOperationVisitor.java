package gutta.apievolution.fixedformat.apimapping;

interface ScriptOperationVisitor<R> {
    
    default R handleCopyOperation(CopyOperation copyOperation) {
        return null;
    }

    default R handleSkipOperation(SkipOperation skipOperation) {
        return null;
    }
    
    default R handleLoopOperation(LoopOperation loopOperation) {
        return null;
    }
    
    default R handleEnumMappingOperation(EnumMappingOperation enumMappingOperation) {
        return null;
    }
    
}
