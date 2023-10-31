package gutta.apievolution.inprocess.dynproxy;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

class ListTypeValueMapper implements ValueMapper {

    private final ValueMapper elementMapper;
    
    public ListTypeValueMapper(ValueMapper elementMapper) {
        this.elementMapper = elementMapper;
    }
    
    @Override
    public Object mapValue(Object value) {
        List<?> sourceList = (List<?>) value;
        
        if (sourceList.isEmpty()) {
            return Collections.emptyList();
        }
        
        List<Object> targetList = new ArrayList<>(sourceList.size());
        for (Object sourceElement : sourceList) {
            Object targetElement = this.elementMapper.mapValue(sourceElement);
            targetList.add(targetElement);
        }
        
        return targetList;
    }

}
