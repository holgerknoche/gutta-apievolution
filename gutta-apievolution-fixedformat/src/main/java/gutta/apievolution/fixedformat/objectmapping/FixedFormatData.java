package gutta.apievolution.fixedformat.objectmapping;

import java.nio.ByteBuffer;
import java.nio.charset.Charset;

public interface FixedFormatData {
    
    static FixedFormatData of(ByteBuffer buffer, Charset charset) {
        return new ByteBufferData(charset, buffer);
    }
    
    void writeInt32(int value);
    
    void writeBoundedString(String value, int maxLength);
    
    void writePadding(int length);

}
