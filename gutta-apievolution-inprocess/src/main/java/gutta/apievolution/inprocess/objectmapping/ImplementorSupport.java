package gutta.apievolution.inprocess.objectmapping;

import gutta.apievolution.inprocess.ImplementedBy;
import gutta.apievolution.inprocess.InvalidApiException;

import java.lang.reflect.Modifier;
import java.util.Optional;

class ImplementorSupport {

    private static boolean isAbstract(Class<?> type) {
        return Modifier.isAbstract(type.getModifiers());
    }

    public static Optional<Class<?>> determineImplementorOf(Class<?> type) {
        if (!isAbstract(type)) {
            return Optional.of(type);
        }

        ImplementedBy implementorAnnotation = type.getAnnotation(ImplementedBy.class);
        if (implementorAnnotation == null) {
            return Optional.empty();
        } else {
            return Optional.of(implementorAnnotation.value());
        }
    }

    public static Class<?> determineMandatoryImplementorOf(Class<?> type) {
        return determineImplementorOf(type).orElseThrow(() -> new InvalidApiException("Unable to determine implementor type for type '" + type + "'."));
    }

}
