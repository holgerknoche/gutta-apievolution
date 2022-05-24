package gutta.apievolution.fixedformat.apimapping;

import gutta.apievolution.core.apimodel.UserDefinedType;

import java.nio.ByteBuffer;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.function.Function;
import java.util.stream.Collectors;

/**
 * This class represents an API mapping script for converting a fixed-format representation of an API into another.
 */
public class ApiMappingScript implements Iterable<TypeEntry> {

    private final List<TypeEntry> typeEntries;

    private final Map<Integer, TypeEntry> typeToEntry;

    ApiMappingScript(List<TypeEntry> typeEntries) {
        this.typeEntries = typeEntries;
        this.typeToEntry = typeEntries.stream().collect(Collectors.toMap(op -> op.getTypeId(), Function.identity()));
    }

    /**
     * Maps data of a given type according using this script.
     * @param type The type of the data in the source buffer
     * @param source A buffer containing the source data to map
     * @param target A buffer to write the mapped data to
     */
    public void mapType(UserDefinedType<?> type, ByteBuffer source, ByteBuffer target) {
        this.mapType(type.getTypeId(), source, target);
    }

    /**
     * Maps data of a given type according using this script.
     * @param typeId The type id of the data in the source buffer
     * @param source A buffer containing the source data to map
     * @param target A buffer to write the mapped data to
     */
    public void mapType(Integer typeId, ByteBuffer source, ByteBuffer target) {
        TypeEntry typeEntry = this.typeToEntry.get(typeId);
        if (typeEntry == null) {
            throw new IllegalArgumentException("No entry for type id " + typeId + ".");
        }

        ApiMappingOperation mappingOperation = typeEntry.createMappingOperation();
        mappingOperation.apply(0, this::resolveTypeEntry, source, target);
    }

    @SuppressWarnings("unchecked")
    private <T extends TypeEntry> T resolveTypeEntry(int entryIndex) {
        return (T) this.typeEntries.get(entryIndex);
    }

    @Override
    public Iterator<TypeEntry> iterator() {
        return this.typeEntries.iterator();
    }

    int size() {
        return this.typeEntries.size();
    }

}
