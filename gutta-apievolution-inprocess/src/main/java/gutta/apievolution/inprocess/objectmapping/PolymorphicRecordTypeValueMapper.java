package gutta.apievolution.inprocess.objectmapping;

import gutta.apievolution.inprocess.AbstractRecordTypeValueMapper;
import gutta.apievolution.inprocess.InvalidInvocationException;

import java.util.Map;

class PolymorphicRecordTypeValueMapper extends AbstractRecordTypeValueMapper {

    private Map<Class<?>, AbstractRecordTypeValueMapper> subTypeMappers;

    public PolymorphicRecordTypeValueMapper(Class<?> targetType, Map<Class<?>, AbstractRecordTypeValueMapper> subTypeMappers) {
        super(targetType);

        this.subTypeMappers = subTypeMappers;
    }

    @Override
    public Object mapRepresentableValue(Object value) {
        Class<?> valueType = value.getClass();
        AbstractRecordTypeValueMapper delegateMapper = this.subTypeMappers.get(valueType);
        if (delegateMapper == null) {
            throw new InvalidInvocationException("No subtype mapper for type '" + valueType + "'.");
        }

        return delegateMapper.mapRepresentableValue(value);
    }

}
