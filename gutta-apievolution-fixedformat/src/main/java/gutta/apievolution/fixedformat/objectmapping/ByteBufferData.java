package gutta.apievolution.fixedformat.objectmapping;

import java.nio.ByteBuffer;
import java.nio.ByteOrder;
import java.nio.charset.Charset;
import java.util.Arrays;

/**
 * Specific implementation of fixed-format data contained in a byte buffer.
 */
public class ByteBufferData implements FixedFormatData {
    
    private static final int WORK_ARRAY_SIZE = 512;
    
    private static final byte PADDING_BYTE = 0x00;
    
    private final Charset charset;
    
    private final ByteBuffer buffer;
    
    private final byte[] workArray = new byte[WORK_ARRAY_SIZE];
    
    /**
     * Creates a new object from the given data.
     * @param charset The charset to use (must be a one-byte-per-character charset)
     * @param buffer The buffer to work with
     */
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
    public byte readFlagsByte() {
        return this.buffer.get();
    }
    
    @Override
    public void writeFlagsByte(byte flags) {
        this.buffer.put(flags);
    }
    
    @Override
    public int readInt32() {
        return this.buffer.getInt();
    }
    
    @Override
    public void writeInt32(int value) {
        this.buffer.putInt(value);
    }

    private int determineStringLength(byte[] encodedString, int maxLength) {
        int currentIndex = (maxLength - 1);
        
        while (currentIndex >= 0) {
            if (encodedString[currentIndex] != PADDING_BYTE) {
                return (currentIndex + 1);
            }
            currentIndex--;
        }
        
        return 0;
    }
    
    @Override
    public String readBoundedString(int maxLength) {
        byte[] encodedString = this.getWorkArray(maxLength);
        this.buffer.get(encodedString, 0, maxLength);
        
        int stringLength = this.determineStringLength(encodedString, maxLength);
        if (stringLength > 0) {
            return new String(encodedString, 0, stringLength, this.charset);
        } else {
            return "";
        }
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
    public void skipBytes(int amount) {
        int currentOffset = this.buffer.position();
        this.buffer.position(currentOffset + amount);
    }
    
    @Override
    public void writePadding(int length) {
        byte[] workArray = this.getWorkArray(length);
        Arrays.fill(workArray, 0, length, PADDING_BYTE);

        this.buffer.put(workArray, 0, length);
    }

}
