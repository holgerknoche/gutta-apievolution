package gutta.apievolution.inprocess;

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
        if (value == null) {
            return null;
        }

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
