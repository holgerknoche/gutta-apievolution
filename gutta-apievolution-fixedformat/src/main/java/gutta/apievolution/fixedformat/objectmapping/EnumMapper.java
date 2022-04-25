package gutta.apievolution.fixedformat.objectmapping;

import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;

class EnumMapper implements TypeMapper<Object> {
    
    private final Object[] ordinalLookup;
    
    public EnumMapper(Class<?> enumType) {
        this.ordinalLookup = createOrdinalLookup(enumType);
    }
    
    private static Object[] createOrdinalLookup(Class<?> enumType) {
        if (!enumType.isEnum()) {
            throw new IllegalArgumentException("Type " + enumType + " is not an enum.");
        }
        
        try {
            Method valuesMethod = enumType.getMethod("values");
            Object[] values = (Object[]) valuesMethod.invoke(null);
            
            // Make sure that the values are sorted by ordinal, although this is probably
            // the case anyway
            Object[] sortedValues = new Object[values.length];
            for (Object value : values) {
                Enum<?> enumValue = (Enum<?>) value;
                sortedValues[enumValue.ordinal()] = enumValue;
            }
            
            return sortedValues;
        } catch (NoSuchMethodException | InvocationTargetException | IllegalAccessException e) {
            throw new RuntimeException(e);
        }
    }

    @Override
    public boolean isCacheable() {
        return true;
    }
    
    @Override
    public int getMaxLength() {
        return 4;
    }

    @Override
    public Object readValue(FixedFormatData data) {
        int ordinal = data.readInt32();
        return this.ordinalLookup[ordinal];
    }
    
    @Override
    public void writeValue(Object value, FixedFormatData data) {
        int ordinal = ((Enum<?>) value).ordinal();
        data.writeInt32(ordinal);
    }

}
