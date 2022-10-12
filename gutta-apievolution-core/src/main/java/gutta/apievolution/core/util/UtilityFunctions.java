package gutta.apievolution.core.util;

import java.util.function.Consumer;

/**
 * This class contains generic utility functions. 
 */
public class UtilityFunctions {
    
    /**
     * Runs the given action on the given value provided it exists.
     * @param <T> The type of the value
     * @param value The value to operate on
     * @param action The action to perform
     */
    public static <T> void ifPresent(T value, Consumer<T> action) {
        if (value != null) {
            action.accept(value);
        }
    }

}
