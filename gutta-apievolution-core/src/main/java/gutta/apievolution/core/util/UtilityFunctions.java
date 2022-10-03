package gutta.apievolution.core.util;

import java.util.function.Consumer;

public class UtilityFunctions {
    
    public static <T> void ifPresent(T value, Consumer<T> action) {
        if (value != null) {
            action.accept(value);
        }
    }

}
