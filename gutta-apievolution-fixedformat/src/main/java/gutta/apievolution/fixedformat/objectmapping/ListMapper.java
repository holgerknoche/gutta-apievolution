package gutta.apievolution.fixedformat.objectmapping;

import static java.lang.Math.min;

import java.util.List;

class ListMapper implements TypeMapper {

    private final int maxElements;
    
    private final TypeMapper elementMapper;
    
    private final int maxLength;
    
    public ListMapper(int maxElements, TypeMapper elementMapper) {
        this.maxElements = maxElements;
        this.elementMapper = elementMapper;
        
        this.maxLength = (maxElements * elementMapper.getMaxLength());
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
    public void writeValue(Object value, FixedFormatData data) {
        List<?> list = (List<?>) value;
                
        // Write at most maxElements elements
        int elementsToWrite = min(list.size(), this.maxElements);

        // TODO Write number of written elements so that we know how many valid entries there are
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
