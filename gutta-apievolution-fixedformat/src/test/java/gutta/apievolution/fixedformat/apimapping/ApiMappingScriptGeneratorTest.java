package gutta.apievolution.fixedformat.apimapping;

import static org.junit.jupiter.api.Assertions.assertEquals;

import java.util.Collections;
import java.util.Optional;

import org.junit.jupiter.api.Test;

import gutta.apievolution.core.apimodel.consumer.ConsumerApiDefinition;
import gutta.apievolution.core.apimodel.provider.ProviderApiDefinition;
import gutta.apievolution.core.apimodel.provider.RevisionHistory;
import gutta.apievolution.core.resolution.DefinitionResolution;
import gutta.apievolution.core.resolution.DefinitionResolver;
import gutta.apievolution.dsl.ConsumerApiLoader;
import gutta.apievolution.dsl.ProviderApiLoader;
import gutta.apievolution.fixedformat.apimapping.ApiMappingScriptGenerator.MappingDirection;

class ApiMappingScriptGeneratorTest {	
	
	/**
	 * Test case: A simple polymorphic type mapping is generated where necessary.
	 */
	@Test
	void simplePolymorphicTypeMapping() {
		String providerApi = "api test { record A {} record B extends A {} record C extends A {} record D { A testField } }";
		// Reorder the types to get a non-trivial mapping
		String consumerApi = "api test { record A {} record C extends A {} record B extends A {} record D { A testField } }";
		
		ApiMappingScript mappingScript = this.createMappingScript(providerApi, consumerApi, MappingDirection.CONSUMER_TO_PRODUCER);
		
		String expectedScript = "record 0\n"
				+ "record 1\n"
				+ "record 2\n"
				+ "record 3\n"
				+ "@0: map poly record 0->(0@0), 1->(2@2), 2->(1@1)\n";
		
		String actualScript = new ApiMappingScriptPrinter().printMappingScript(mappingScript); 
		
		assertEquals(expectedScript, actualScript);
	}

	private ApiMappingScript createMappingScript(String providerApi, String consumerApi, MappingDirection mappingDirection) {
		ProviderApiDefinition providerDefinition = ProviderApiLoader.loadFromString(0, providerApi, false, Optional.empty());
		ConsumerApiDefinition consumerDefinition = ConsumerApiLoader.loadFromString(consumerApi, 0);
		
		RevisionHistory revisionHistory = new RevisionHistory(providerDefinition);
		DefinitionResolution resolution = new DefinitionResolver().resolveConsumerDefinition(revisionHistory, Collections.singleton(0), consumerDefinition);
		
		return new ApiMappingScriptGenerator().generateMappingScript(resolution, mappingDirection);
	}
	
}
