package gutta.apievolution.json.provider;

import gutta.apievolution.json.MappableException;

public class MappableProviderTestException extends MappableException {

    private static final long serialVersionUID = 1901704190392860380L;
    
    public MappableProviderTestException(ProviderTestException data) {
        super(data);
    }

}
