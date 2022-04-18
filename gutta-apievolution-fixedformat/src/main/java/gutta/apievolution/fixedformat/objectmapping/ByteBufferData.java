package gutta.apievolution.fixedformat.objectmapping;

import java.nio.ByteBuffer;
import java.nio.ByteOrder;
import java.nio.charset.Charset;
import java.util.Arrays;

public class ByteBufferData implements FixedFormatData {
    
    private static final int WORK_ARRAY_SIZE = 512;
    
    private final Charset charset;
    
    private final ByteBuffer buffer;
    
    private final byte[] workArray = new byte[WORK_ARRAY_SIZE];
    
    protected ByteBufferData(Charset charset, ByteBuffer buffer) {
        assertValidCharset(charset);
        
        this.charset = charset;
        this.buffer = buffer;
        
        this.buffer.order(ByteOrder.BIG_ENDIAN);
    }

    private static void assertValidCharset(Charset charset) {
        if (charset.newEncoder().maxBytesPerChar() > 1) {
            throw new IllegalArgumentException(charset + " is not a one-byte-per-character charset.");
        }
    }
    
    private byte[] getWorkArray(int size) {
        // Avoid allocating new arrays if not necessary
        return (size <= WORK_ARRAY_SIZE) ? this.workArray : new byte[size];
    }
    
    @Override
    public void writeInt32(int value) {
        this.buffer.putInt(value);
    }

    @Override
    public void writeBoundedString(String value, int maxLength) {
        int actualLength = value.length();
        
        if (actualLength < maxLength) {
            // If the value is shorter than the field, it must be padded to the
            // correct length
            byte[] encodedString = value.getBytes(this.charset);
            this.buffer.put(encodedString, 0, actualLength);
            
            int paddingLength = (maxLength - actualLength);
            this.writePadding(paddingLength);
        } else {
            byte[] encodedString = value.substring(0, maxLength).getBytes(this.charset);
            this.buffer.put(encodedString);
        }
    }
    
    @Override
    public void writePadding(int length) {
        byte[] workArray = this.getWorkArray(length);
        Arrays.fill(workArray, 0, length, (byte) 0);

        this.buffer.put(workArray, 0, length);
    }

}
