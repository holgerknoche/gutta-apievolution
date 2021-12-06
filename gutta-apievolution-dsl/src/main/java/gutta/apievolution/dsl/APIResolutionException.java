package gutta.apievolution.dsl;

import org.antlr.v4.runtime.Token;

public class APIResolutionException extends RuntimeException {
	
	private static final long serialVersionUID = -2560140692689234150L;

	private final Token token;
	
	public APIResolutionException(final Token token, final String message) {
		super(message);
		
		this.token = token;
	}
	
	public APIResolutionException(final Token token, final String message, final Throwable cause) {
		super(message, cause);
		
		this.token = token;
	}
	
	public boolean isAtPosition(final int line, final int positionInLine) {
		return (this.token.getLine() == line && this.token.getCharPositionInLine() == positionInLine);
	}
	
	@Override
	public String toString() {
		return this.token + ": " + this.getMessage();
	}
	
}
