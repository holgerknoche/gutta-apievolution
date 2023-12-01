package gutta.apievolution.dsl;

import gutta.apievolution.dsl.parser.ApiRevisionLexer;
import gutta.apievolution.dsl.parser.ApiRevisionParser;
import org.antlr.v4.runtime.CharStream;
import org.antlr.v4.runtime.CharStreams;
import org.antlr.v4.runtime.CommonTokenStream;
import org.antlr.v4.runtime.TokenStream;

import java.io.IOException;
import java.io.InputStream;

class ApiDefinitionLoader {

    protected static ApiRevisionParser.ApiDefinitionContext parseStream(InputStream inputStream) throws IOException {
        CharStream charStream = CharStreams.fromStream(inputStream);
        return parse(charStream);
    }

    protected static ApiRevisionParser.ApiDefinitionContext parseString(String input) {
        CharStream charStream = CharStreams.fromString(input);
        return parse(charStream);
    }

    private static ApiRevisionParser.ApiDefinitionContext parse(CharStream input) {
        ApiRevisionLexer lexer = new ApiRevisionLexer(input);
        TokenStream tokenStream = new CommonTokenStream(lexer);
        ApiRevisionParser parser = new ApiRevisionParser(tokenStream);

        parser.removeErrorListeners();
        parser.addErrorListener(new ThrowingErrorListener());

        return parser.apiDefinition();
    }

    protected ApiDefinitionLoader() {
        // Protected default constructor
    }

}
