package gutta.apievolution.fixedformat.objectmapping;

import java.nio.ByteBuffer;
import java.nio.charset.Charset;

/**
 * Interface to work with fixed-format data. 
 */
public interface FixedFormatData {
    
    /**
     * Convenience operation to create a fixed-format data object using a byte buffer.
     * @param buffer The buffer to work with
     * @param charset The charset for the data (must be a one-byte-per-character charset)
     * @return The data object
     */
    static FixedFormatData of(ByteBuffer buffer, Charset charset) {
        return new ByteBufferData(charset, buffer);
    }
    
    /**
     * Reads a 32-bit integer at the current position.
     * @return The read value
     */
    int readInt32();
    
    /**
     * Writes a 32-bit integer at the current position.
     * @param value The value to write
     */
    void writeInt32(int value);
    
    /**
     * Reads a bounded string of the given length at the current position.
     * @param maxLength The maximal length of the string to read
     * @return The read string (stripped)
     */
    String readBoundedString(int maxLength);
    
    /**
     * Writes the given string at the current position, padding or truncating it to the given length if needed.
     * @param value The value to write
     * @param maxLength The maximal length of the string
     */
    void writeBoundedString(String value, int maxLength);
    
    /**
     * Skips the given number of bytes in the data.
     * @param amount The number of bytes to skip
     */
    void skipBytes(int amount);
    
    /**
     * Writes the given number of padding bytes at the current position.
     * @param length The number of padding bytes to write
     */
    void writePadding(int length);

}
