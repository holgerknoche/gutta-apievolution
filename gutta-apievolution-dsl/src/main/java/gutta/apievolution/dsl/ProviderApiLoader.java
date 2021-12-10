package gutta.apievolution.dsl;

import java.io.IOException;
import java.io.InputStream;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.Iterator;
import java.util.List;
import java.util.Optional;
import java.util.function.Supplier;

import gutta.apievolution.core.apimodel.provider.ProviderApiDefinition;
import gutta.apievolution.dsl.parser.ApiRevisionLexer;
import gutta.apievolution.dsl.parser.ApiRevisionParser;
import org.antlr.v4.runtime.BailErrorStrategy;
import org.antlr.v4.runtime.CharStream;
import org.antlr.v4.runtime.CharStreams;
import org.antlr.v4.runtime.CommonTokenStream;
import org.antlr.v4.runtime.TokenStream;

public class ProviderApiLoader {
	
	public static ProviderApiDefinition loadFromStream(final int revision, final InputStream inputStream) throws IOException {
		return loadFromStream(revision, inputStream, Optional.empty());
	}
	
	public static ProviderApiDefinition loadFromStream(final int revision, final InputStream inputStream, final Optional<ProviderApiDefinition> optionalPredecessor) throws IOException {
		CharStream stream = CharStreams.fromStream(inputStream);
		ApiRevisionLexer lexer = new ApiRevisionLexer(stream);
		TokenStream tokenStream = new CommonTokenStream(lexer);
		ApiRevisionParser parser = new ApiRevisionParser(tokenStream);
		
		parser.setErrorHandler(new BailErrorStrategy());
		
		ApiRevisionParser.ApiDefinitionContext specification = parser.apiDefinition();
		return new ProviderApiRevisionModelBuilder().buildProviderRevision(revision, specification, optionalPredecessor);
	}

	public static List<ProviderApiDefinition> loadHistoryFromStreams(final Iterable<Integer> revisionIds,
																	 final Collection<? extends InputStream> streams)
			throws IOException {
		if (streams.isEmpty()) {
			return Collections.emptyList();
		}
		
		int streamCount = streams.size();	
		List<ProviderApiDefinition> revisions = new ArrayList<ProviderApiDefinition>(streamCount);
		Iterator<Integer> revisionsIdsIterator = revisionIds.iterator();
		Iterator<? extends InputStream> streamIterator = streams.iterator();
		
		// Convert the first stream (i.e. first revision in the history)
		ProviderApiDefinition currentRevision;
		try (InputStream stream = streamIterator.next()) {
			currentRevision = loadFromStream(revisionsIdsIterator.next(), stream);
			revisions.add(currentRevision);
		}
		
		// Convert the remaining streams		
		while (streamIterator.hasNext()) {
			try (InputStream stream = streamIterator.next()) {
				Optional<ProviderApiDefinition> optionalPredecessor = Optional.of(currentRevision);
				currentRevision = loadFromStream(revisionsIdsIterator.next(), stream, optionalPredecessor);
				revisions.add(currentRevision);
			}
		}
		
		return revisions;
	}	
	
}
