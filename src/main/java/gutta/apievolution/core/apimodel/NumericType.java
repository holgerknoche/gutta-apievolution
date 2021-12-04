package gutta.apievolution.core.apimodel;

/**
 * A numeric type represents a fixed-length decimal with integer and fractional places.
 */
public class NumericType implements BoundedType {

    private final int integerPlaces;

    private final int fractionalPlaces;

    /**
     * Creates a new bounded numeric type.
     * @param integerPlaces The type's number of integer places
     * @param fractionalPlaces The type's number of fractional places
     * @return The numeric type
     */
    public static NumericType bounded(int integerPlaces, int fractionalPlaces) {
        return new NumericType(integerPlaces, fractionalPlaces);
    }

    private NumericType(int integerPlaces, int fractionalPlaces) {
        this.integerPlaces = integerPlaces;
        this.fractionalPlaces = fractionalPlaces;
    }

    /**
     * Returns this type's number of integer places.
     * @return see above
     */
    public int getIntegerPlaces() {
        return this.integerPlaces;
    }

    /**
     * Returns this type's number of fractional places.
     * @return see above
     */
    public int getFractionalPlaces() {
        return this.fractionalPlaces;
    }

}
