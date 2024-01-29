package gutta.apievolution.json;

public class UnrepresentableValueException extends RuntimeException {
    
    private static final long serialVersionUID = 4985373813354358402L;

    UnrepresentableValueException(String message) {
        super(message);
    }

}
