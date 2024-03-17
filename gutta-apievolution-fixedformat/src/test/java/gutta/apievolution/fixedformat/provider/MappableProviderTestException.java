package gutta.apievolution.fixedformat.provider;

import gutta.apievolution.fixedformat.apimapping.provider.MappableException;

public class MappableProviderTestException extends MappableException {
        
    private static final long serialVersionUID = 6620196346829405663L;

    public MappableProviderTestException(ProviderTestException data) {
        super(data);
    }
    
}
