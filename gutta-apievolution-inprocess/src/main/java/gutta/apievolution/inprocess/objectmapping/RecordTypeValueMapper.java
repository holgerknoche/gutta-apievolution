package gutta.apievolution.inprocess.objectmapping;

import gutta.apievolution.inprocess.FieldMapper;
import gutta.apievolution.inprocess.ImplementedBy;
import gutta.apievolution.inprocess.InvalidApiException;
import gutta.apievolution.inprocess.InvalidInvocationException;
import gutta.apievolution.inprocess.ValueMapper;

import java.lang.reflect.Constructor;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.lang.reflect.Modifier;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;

class RecordTypeValueMapper implements ValueMapper {

    private ObjectCreator<?> recordCreator;

    private final List<FieldValueTransferrer> transferrers;

    public RecordTypeValueMapper(Class<?> targetType, Map<Method, FieldMapper> fieldMappers) {
        this.recordCreator = new ObjectCreator<>(implementorOf(targetType));

        List<FieldValueTransferrer> transferrers = new ArrayList<>(fieldMappers.size());
        fieldMappers.forEach((accessor, mapper) -> transferrers.add(new FieldValueTransferrer(accessor, mapper)));

        this.transferrers = transferrers;
    }

    private static Class<?> implementorOf(Class<?> type) {
        if (Modifier.isAbstract(type.getModifiers())) {
            ImplementedBy implementorAnnotation = type.getAnnotation(ImplementedBy.class);
            if (implementorAnnotation == null) {
                throw new InvalidApiException("Unable to determine implementor class for '" + type + "'.");
            }

            return implementorAnnotation.value();
        } else {
            return type;
        }
    }

    @Override
    public Object mapValue(Object value) {
        if (value == null) {
            return null;
        }

        Object record = this.recordCreator.createObject();
        this.transferrers.forEach(transferrer -> transferrer.transferValue(value, record));

        return record;
    }

    private static class ObjectCreator<T> {

        private final Constructor<T> constructor;

        public ObjectCreator(Class<T> createdType) {
            try {
                this.constructor = createdType.getConstructor();
            } catch (NoSuchMethodException | SecurityException e) {
                throw new InvalidApiException("No usable constructor on type '" + createdType + "'.", e);
            }
        }

        public T createObject() {
            try {
                return this.constructor.newInstance();
            } catch (IllegalAccessException | InstantiationException | InvocationTargetException e) {
                throw new InvalidInvocationException("Error creating a record type instance.", e);
            }
        }

    }

    private static class FieldValueTransferrer {

        private Method targetAccessor;

        private FieldMapper fieldMapper;

        public FieldValueTransferrer(Method targetAccessor, FieldMapper fieldMapper) {
            this.targetAccessor = targetAccessor;
            this.fieldMapper = fieldMapper;
        }

        public void transferValue(Object sourceObject, Object targetObject) {
            Object mappedValue = this.fieldMapper.mapField(sourceObject);
            try {
                this.targetAccessor.invoke(targetObject, mappedValue);
            } catch (IllegalAccessException | IllegalArgumentException | InvocationTargetException e) {
                throw new InvalidInvocationException("Error transferring a value.", e);
            }
        }

    }

}
