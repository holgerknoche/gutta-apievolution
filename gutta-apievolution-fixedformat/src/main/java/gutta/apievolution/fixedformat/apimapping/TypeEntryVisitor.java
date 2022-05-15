package gutta.apievolution.fixedformat.apimapping;

interface TypeEntryVisitor<R> {
    
    default R handleEnumTypeEntry(EnumTypeEntry enumTypeEntry) {
        return null;
    }
    
    default R handleRecordTypeEntry(RecordTypeEntry recordTypeEntry) {
        return null;
    }

}
