package gutta.apievolution.inprocess.dynproxy;

import gutta.apievolution.inprocess.AbstractRecordTypeValueMapper;
import gutta.apievolution.inprocess.InvalidInvocationException;

import java.util.Map;

class PolymorphicRecordTypeValueMapper extends AbstractRecordTypeValueMapper {

    private Map<Class<?>, AbstractRecordTypeValueMapper> subTypeMappers;

    public PolymorphicRecordTypeValueMapper(Class<?> targetType, Map<Class<?>, AbstractRecordTypeValueMapper> subTypeMappers) {
        super(targetType);

        this.subTypeMappers = subTypeMappers;
    }

    private AbstractRecordTypeValueMapper findAppropriateMapperFor(Class<?> type) {
        AbstractRecordTypeValueMapper mapper = this.subTypeMappers.get(type);
        if (mapper != null) {
            return mapper;
        }
        
        Class<?> superType = type.getSuperclass();
        if (superType != null) {
            AbstractRecordTypeValueMapper superTypeMapper = this.findAppropriateMapperFor(superType);
            if (superTypeMapper != null) {
                return superTypeMapper;
            }
        }
        
        for (Class<?> implementedInterface : type.getInterfaces()) {
            AbstractRecordTypeValueMapper ifMapper = this.findAppropriateMapperFor(implementedInterface);
            if (ifMapper != null) {
                return ifMapper;
            }
        }
        
        return null;
    }
    
    @Override
    public Object mapRepresentableValue(Object value) {
        Class<?> valueType = value.getClass();        
        
        AbstractRecordTypeValueMapper delegateMapper = this.findAppropriateMapperFor(valueType);
        if (delegateMapper == null) {
            throw new InvalidInvocationException("No subtype mapper for type '" + valueType + "'.");
        }

        return delegateMapper.mapRepresentableValue(value);
    }

}
