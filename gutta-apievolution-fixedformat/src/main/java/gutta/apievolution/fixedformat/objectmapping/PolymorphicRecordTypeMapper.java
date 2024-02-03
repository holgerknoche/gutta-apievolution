package gutta.apievolution.fixedformat.objectmapping;

import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.util.Collection;
import java.util.Map;

class PolymorphicRecordTypeMapper extends TypeMapper<Object> {

    private static final int DISCRIMINATOR_SIZE = 4;

    private final Class<?> representedType;
    
    private final int dataLength;

    private final Map<Integer, RecordTypeMapper> subTypeMappers;

    public PolymorphicRecordTypeMapper(Class<?> representedType, Map<Integer, RecordTypeMapper> subTypeMappers) {
        this.representedType = representedType;
        this.subTypeMappers = subTypeMappers;
        this.dataLength = determineMaxDataLength(subTypeMappers.values());
    }

    private static int determineMaxDataLength(Collection<RecordTypeMapper> mappers) {
        int maxLength = 0;

        for (RecordTypeMapper mapper : mappers) {
            if (mapper.getMaxLength() > maxLength) {
                maxLength = mapper.getDataLength();
            }
        }

        return (maxLength + DISCRIMINATOR_SIZE);
    }

    @Override
    public boolean isCacheable() {
        return true;
    }

    @Override
    protected int getDataLength() {
        return this.dataLength;
    }

    private RecordTypeMapper getTypeMapperFor(int typeId) {
        RecordTypeMapper typeMapper = this.subTypeMappers.get(typeId);

        if (typeMapper == null) {
            throw new InvalidDataException("No type mapper for type id " + typeId + ".");
        }

        return typeMapper;
    }

    @Override()
    protected Object readRegularValue(FixedFormatData data) {
        int typeId = data.readInt32();

        RecordTypeMapper typeMapper = this.getTypeMapperFor(typeId);
        // Null checks have already been performed, therefore invoke readRegularValue
        return typeMapper.readRegularValue(data);
    }

    private Method findUnrepresentableValueHandler() {
        for (Method method : this.representedType.getMethods()) {
            if (method.isAnnotationPresent(UnrepresentableValue.class)) {
                return method;
            }
        }
        
        return null;
    }
    
    @Override
    public Object handleUnrepresentableValue() {
        Method unrepresentableValueHandler = this.findUnrepresentableValueHandler();
        if (unrepresentableValueHandler != null) {
            try {
                return unrepresentableValueHandler.invoke(null);
            } catch (IllegalAccessException | IllegalArgumentException | InvocationTargetException e) {
                throw new RuntimeException("An error occurred invoking the unrepresentable value handler on class '" + this.representedType + "'.", e);
            }
        } else {
            throw new UnrepresentableValueException("An unrepresentable subtype of '" + this.representedType +
                    "' was encountered, and no handler was defined.");
        }
    }

    @Override
    protected void writeRegularValue(Object value, FixedFormatData data) {
        TypeId typeIdAnnotation = value.getClass().getAnnotation(TypeId.class);
        if (typeIdAnnotation == null) {
            throw new InvalidRepresentationElementException("Missing type id on type " + value.getClass() + ".");
        }

        int typeId = typeIdAnnotation.value();
        data.writeInt32(typeId);

        RecordTypeMapper typeMapper = this.getTypeMapperFor(typeId);
        // Non-null flag has already been written
        typeMapper.writeRegularValue(value, data);
    }
    
}
