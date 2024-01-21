package gutta.apievolution.fixedformat.objectmapping;

public class Flags {       
    
	public static final int FLAGS_SIZE = 1;
	
    static final byte IS_ABSENT = (byte) 0x00;
    
    static final byte IS_PRESENT = (byte) 0x01;
    
    static final byte IS_UNREPRESENTABLE = (byte) 0x02;

}
