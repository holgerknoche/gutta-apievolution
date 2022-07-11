package gutta.apievolution.repository;

/**
 * This class represents a mapping representation, consisting of the raw data and the appropriate media type. 
 */
public class MappingRepresentation {

    private final String mediaType;
    
    private final byte[] data;
    
    MappingRepresentation(String mediaType, byte[] data) {
        this.mediaType = mediaType;
        this.data = data;
    }
    
    /**
     * Returns the media type of this mapping representation.
     * @return see above
     */
    public String getMediaType() {
        return this.mediaType;
    }
    
    /**
     * Returns the raw data of this mapping representation.
     * @return see above
     */
    public byte[] getData() {
        return this.data;
    }
    
}
