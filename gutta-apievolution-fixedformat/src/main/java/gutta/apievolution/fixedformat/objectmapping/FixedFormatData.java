package gutta.apievolution.fixedformat.objectmapping;

import java.nio.ByteBuffer;
import java.nio.charset.Charset;

public interface FixedFormatData {
    
    static FixedFormatData of(ByteBuffer buffer, Charset charset) {
        return new ByteBufferData(charset, buffer);
    }
    
    int readInt32();
    
    void writeInt32(int value);
    
    String readBoundedString(int maxLength);
    
    void writeBoundedString(String value, int maxLength);
    
    void skipBytes(int amount);
    
    void writePadding(int length);

}
