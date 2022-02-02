package gutta.apievolution.dsl;

import gutta.apievolution.dsl.parser.ApiRevisionLexer;
import gutta.apievolution.dsl.parser.ApiRevisionParser;
import org.antlr.v4.runtime.*;

import java.io.IOException;
import java.io.InputStream;

class ApiDefinitionLoader {

    protected static ApiRevisionParser.ApiDefinitionContext parseStream(InputStream inputStream) throws IOException {
        CharStream charStream = CharStreams.fromStream(inputStream);
        ApiRevisionLexer lexer = new ApiRevisionLexer(charStream);
        TokenStream tokenStream = new CommonTokenStream(lexer);
        ApiRevisionParser parser = new ApiRevisionParser(tokenStream);

        //parser.setErrorHandler(new BailErrorStrategy());

        return parser.apiDefinition();
    }

    protected ApiDefinitionLoader() {
        // Protected default constructor
    }

}
