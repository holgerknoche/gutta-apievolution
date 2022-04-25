package gutta.apievolution.fixedformat.objectmapping;

import static java.lang.Math.min;

import java.util.ArrayList;
import java.util.List;

class ListMapper implements TypeMapper<Object> {

    private final int maxElements;
    
    private final TypeMapper<?> elementMapper;
    
    private final int maxLength;
    
    public ListMapper(int maxElements, TypeMapper<?> elementMapper) {
        this.maxElements = maxElements;
        this.elementMapper = elementMapper;
        
        this.maxLength = (maxElements * elementMapper.getMaxLength()) + 4;
    }
    
    @Override
    public boolean isCacheable() {
        // Mappers for parameterized types are not cacheable
        return false;
    }
    
    @Override
    public int getMaxLength() {
        return this.maxLength;
    }

    @Override
    public Object readValue(FixedFormatData data) {
        int actualElementCount = data.readInt32();
        actualElementCount = min(actualElementCount, this.maxElements);
        
        List<Object> list = new ArrayList<>(actualElementCount);
        
        for (int elementIndex = 0; elementIndex < actualElementCount; elementIndex++) {
            Object element = this.elementMapper.readValue(data);
            list.add(element);
        }
        
        // Skip remaining bytes, if any
        int remainingElementCount = (this.maxElements - actualElementCount);
        data.skipBytes(remainingElementCount * this.elementMapper.getMaxLength());
        
        return list;
    }
    
    @Override
    public void writeValue(Object value, FixedFormatData data) {
        List<?> list = (List<?>) value;
                
        // Write at most maxElements elements
        int elementsToWrite = min(list.size(), this.maxElements);
        
        // Write number of elements as an int32
        data.writeInt32(elementsToWrite);
        for (int elementIndex = 0; elementIndex < elementsToWrite; elementIndex++) {
            Object element = list.get(elementIndex);
            this.elementMapper.writeValue(element, data);
        }
        
        if (elementsToWrite < this.maxElements) {
            int paddingLength = ((this.maxElements - elementsToWrite) * this.elementMapper.getMaxLength());
            data.writePadding(paddingLength);
        }
    }

}
