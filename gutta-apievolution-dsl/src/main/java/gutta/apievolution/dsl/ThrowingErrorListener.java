package gutta.apievolution.dsl;

import org.antlr.v4.runtime.BaseErrorListener;
import org.antlr.v4.runtime.RecognitionException;
import org.antlr.v4.runtime.Recognizer;
import org.antlr.v4.runtime.Token;

/**
 * Error listener that throws an exception if a syntax error occurs.
 */
class ThrowingErrorListener
        extends BaseErrorListener {

    @Override
    public void syntaxError(Recognizer<?, ?> recognizer, Object offendingSymbol, int line, int charPositionInLine,
            String message, RecognitionException e) {
        throw new APIParseException((Token) offendingSymbol, message);
    }

}
