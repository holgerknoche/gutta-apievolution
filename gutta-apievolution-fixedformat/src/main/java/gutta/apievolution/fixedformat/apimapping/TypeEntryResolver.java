package gutta.apievolution.fixedformat.apimapping;

interface TypeEntryResolver {
    
    <T extends TypeEntry> T resolveEntry(int index);

}
