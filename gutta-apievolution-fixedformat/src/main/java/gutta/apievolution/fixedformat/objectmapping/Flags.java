package gutta.apievolution.fixedformat.objectmapping;

/**
 * Constants for flags for values, such as absent and unrepresentable values.
 */
public class Flags {

    public static final int FLAGS_SIZE = 1;

    public static final byte IS_ABSENT = (byte) 0x00;

    public static final byte IS_PRESENT = (byte) 0x01;

    public static final byte IS_UNREPRESENTABLE = (byte) 0x02;

}
