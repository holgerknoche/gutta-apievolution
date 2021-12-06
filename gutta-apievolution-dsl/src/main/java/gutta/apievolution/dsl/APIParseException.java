package gutta.apievolution.dsl;

import org.antlr.v4.runtime.Token;

public class APIParseException extends RuntimeException {
	
	private static final long serialVersionUID = 4220185620253787830L;

	private static String createMessage(final Token token, final String message) {
		return token.getLine() + ":" + token.getCharPositionInLine() + ": " + message;
	}
	
	public APIParseException(final Token location, final String message) {
		super(createMessage(location, message));
	}
	
	public APIParseException(final Token location, final String message, final Throwable cause) {
		super(createMessage(location, message), cause);
	}

}
