package gutta.apievolution.inprocess;

import java.util.Map;

class EnumTypeValueMapper implements ValueMapper {

    private final Map<Enum<?>, Enum<?>> memberMap;

    public EnumTypeValueMapper(Map<Enum<?>, Enum<?>> memberMap) {
        this.memberMap = memberMap;
    }

    @Override
    public Object mapValue(Object value) {
        return this.memberMap.get(value);
    }

}
