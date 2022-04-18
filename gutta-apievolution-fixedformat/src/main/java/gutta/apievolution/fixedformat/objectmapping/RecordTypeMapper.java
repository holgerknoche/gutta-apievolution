package gutta.apievolution.fixedformat.objectmapping;

import java.util.List;

class RecordTypeMapper implements TypeMapper {
    
    public final int maxLength;
    
    public final List<FieldMapper> fieldMappers;
  
    public RecordTypeMapper(int maxLength, List<FieldMapper> fieldMappers) {
        this.maxLength = maxLength;
        this.fieldMappers = fieldMappers;
    }
    
    @Override
    public boolean isCacheable() {
        return true;
    }
    
    public int getMaxLength() {
        return this.maxLength;
    }
    
    @Override
    public void writeValue(Object value, FixedFormatData data) {
        this.fieldMappers.forEach(mapper -> mapper.writeValue(value, data));
    }

}
