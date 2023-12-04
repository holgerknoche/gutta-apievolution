package gutta.apievolution.inprocess;

/**
 * This class contains default values and behavior.
 */
class Defaults {
    
    /**
     * Default behavior when an unrepresentable value is encountered.
     * 
     * @return Default value for unrepresentable values, unless an exception is thrown
     */
    public static Object onUnrepresentableValue() {
        return null;
    }

}
