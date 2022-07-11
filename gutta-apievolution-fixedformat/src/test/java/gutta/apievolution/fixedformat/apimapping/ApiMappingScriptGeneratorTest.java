package gutta.apievolution.fixedformat.apimapping;

import gutta.apievolution.core.apimodel.consumer.ConsumerApiDefinition;
import gutta.apievolution.core.apimodel.provider.ProviderApiDefinition;
import gutta.apievolution.core.apimodel.provider.RevisionHistory;
import gutta.apievolution.core.resolution.DefinitionResolution;
import gutta.apievolution.core.resolution.DefinitionResolver;
import gutta.apievolution.dsl.ConsumerApiLoader;
import gutta.apievolution.dsl.ProviderApiLoader;
import gutta.apievolution.fixedformat.apimapping.ApiMappingScriptGenerator.MappingDirection;
import org.junit.jupiter.api.Test;

import java.util.Collections;
import java.util.Optional;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

class ApiMappingScriptGeneratorTest {    
    
    /**
     * Test case: Record type mapping operations are generated as expected (consumer to provider).
     */
    @Test
    void recordTypeMappingC2P() {
        String providerApi = "api test { enum TestEnum { MEMBER_A MEMBER_B } record TestRecord { string(30) fieldA optin string(10) unmappedField TestEnum fieldB numeric(6,4) fieldC } }";
        // Shuffle fields and members a bit to get a non-trivial mapping
        String consumerApi = "api test { enum TestEnum { MEMBER_B MEMBER_A } record TestRecord { TestEnum fieldB string(30) fieldA numeric(6,4) fieldC } }";
        
        ApiMappingScript mappingScript = this.createMappingScript(providerApi, consumerApi, MappingDirection.CONSUMER_TO_PROVIDER);
        
        String expectedScript = "enum 0 [0 -> 1, 1 -> 0]\n"
                + "record 1\n"
                + "@4: copy 30 bytes\n"
                + "@0: skip 10 bytes\n"
                + "@0: map enum 0\n"
                + "@34: copy 11 bytes\n";
        
        String actualScript = new ApiMappingScriptPrinter().printMappingScript(mappingScript); 
        
        assertEquals(expectedScript, actualScript);
    }
    
    /**
     * Test case: Record type mapping operations are generated as expected (provider to consumer).
     */
    @Test
    void recordTypeMappingP2C() {
        String providerApi = "api test { enum TestEnum { MEMBER_A MEMBER_B } record TestRecord { string(30) fieldA optin string(10) unmappedField TestEnum fieldB numeric(6,4) fieldC } }";
        // Shuffle fields and members a bit to get a non-trivial mapping
        String consumerApi = "api test { enum TestEnum { MEMBER_B MEMBER_A } record TestRecord { TestEnum fieldB string(30) fieldA numeric(6,4) fieldC } }";
        
        ApiMappingScript mappingScript = this.createMappingScript(providerApi, consumerApi, MappingDirection.PROVIDER_TO_CONSUMER);
        
        String expectedScript = "enum 0 [0 -> 1, 1 -> 0]\n"
                + "record 1\n"
                + "@40: map enum 0\n"
                + "@0: copy 30 bytes\n"
                + "@44: copy 11 bytes\n";
        
        String actualScript = new ApiMappingScriptPrinter().printMappingScript(mappingScript); 
        
        assertEquals(expectedScript, actualScript);
    }
    
    /**
     * Test case: The polymorphic type mapping operation is generated as expected (consumer to provider).
     */
    @Test
    void polymorphicTypeMappingC2P() {
        String providerApi = "api test { record A {} record B extends A {} record C extends A {} record D { A testField } }";
        // Reorder the types to get a non-trivial mapping
        String consumerApi = "api test { record A {} record C extends A {} record B extends A {} record D { A testField } }";
        
        ApiMappingScript mappingScript = this.createMappingScript(providerApi, consumerApi, MappingDirection.CONSUMER_TO_PROVIDER);
        
        String expectedScript = "record 0\n"
                + "record 1\n"
                + "record 2\n"
                + "record 3\n"
                + "@0: map poly record 0->(0@0), 1->(2@2), 2->(1@1)\n";
        
        String actualScript = new ApiMappingScriptPrinter().printMappingScript(mappingScript); 
        
        assertEquals(expectedScript, actualScript);
    }

    /**
     * Test case: The polymorphic type mapping operation is generated as expected (provider to consumer).
     */
    @Test
    void polymorphicTypeMappingP2C() {
        String providerApi = "api test { record A {} record B extends A {} record C extends A {} record D { A testField } }";
        // Reorder the types to get a non-trivial mapping
        String consumerApi = "api test { record A {} record C extends A {} record B extends A {} record D { A testField } }";
        
        ApiMappingScript mappingScript = this.createMappingScript(providerApi, consumerApi, MappingDirection.PROVIDER_TO_CONSUMER);
        
        String expectedScript = "record 0\n"
                + "record 1\n"
                + "record 2\n"
                + "record 3\n"
                + "@0: map poly record 0->(0@0), 1->(2@2), 2->(1@1)\n";
        
        String actualScript = new ApiMappingScriptPrinter().printMappingScript(mappingScript); 
        
        assertEquals(expectedScript, actualScript);
    }
    
    /**
     * Test case: List mapping operations are generated as expected (consumer to provider).
     */
    @Test
    void listMappingC2P() {
        String providerApi = "api test { record A { int32 intField optin int64 unmappedField string(10) stringField } record B { A[10] listField } }";        
        String consumerApi = "api test { record A { int32 intField string(10) stringField } record B { A[10] listField } }";
        
        ApiMappingScript mappingScript = this.createMappingScript(providerApi, consumerApi, MappingDirection.CONSUMER_TO_PROVIDER);
        
        String expectedScript = "record 0\n"
                + "@0: copy 4 bytes\n"
                + "@0: skip 8 bytes\n"
                + "@4: copy 10 bytes\n"
                + "record 1\n"
                + "@0: map list 10 elements, source size=14, target size=22, element mapping=map record 0\n";
        
        String actualScript = new ApiMappingScriptPrinter().printMappingScript(mappingScript); 
        
        assertEquals(expectedScript, actualScript);
    }
    
    /**
     * Test case: List mapping operations are generated as expected (provider to consumer).
     */
    @Test
    void listMappingP2C() {
        String providerApi = "api test { record A { int32 intField optin int64 unmappedField string(10) stringField } record B { A[10] listField } }";        
        String consumerApi = "api test { record A { int32 intField string(10) stringField } record B { A[10] listField } }";
        
        ApiMappingScript mappingScript = this.createMappingScript(providerApi, consumerApi, MappingDirection.PROVIDER_TO_CONSUMER);
        
        String expectedScript = "record 0\n"
                + "@0: copy 4 bytes\n"
                + "@12: copy 10 bytes\n"
                + "record 1\n"
                + "@0: map list 10 elements, source size=22, target size=14, element mapping=map record 0\n";
        
        String actualScript = new ApiMappingScriptPrinter().printMappingScript(mappingScript); 
        
        assertEquals(expectedScript, actualScript);
    }
    
    /**
     * Test case: Mapping of an operation with a unique result type (i.e., non-polymorphic result and no exceptions).
     */
    @Test
    void operationMappingWithUniqueResultType() {
    	String providerApi = "api test { record A {} operation testOperation (A) : A }";
    	String consumerApi = "api test { record A {} operation testOperation (A) : A }";
    	
    	ApiMappingScript mappingScript = this.createMappingScript(providerApi, consumerApi, MappingDirection.CONSUMER_TO_PROVIDER);
    	
    	String actualScript = new ApiMappingScriptPrinter().printMappingScript(mappingScript);
    	System.out.println(actualScript);
    }
    
    /**
     * Test case: Script generation fails for an unbounded string field.
     */
    @Test
    void failOnUnboundedString() {
        String providerApi = "api test { record A { string unboundedString } }";
        String consumerApi = providerApi;
        
        ScriptGenerationException exception = assertThrows(ScriptGenerationException.class, () -> this.createMappingScript(providerApi, consumerApi, MappingDirection.CONSUMER_TO_PROVIDER));
        assertTrue(exception.getMessage().contains("is unbounded"));
    }
    
    /**
     * Test case: Script generation fails for an unbounded list field.
     */
    @Test
    void failOnUnboundedList() {
        String providerApi = "api test { record A { int32* unboundedList } }";
        String consumerApi = providerApi;
        
        ScriptGenerationException exception = assertThrows(ScriptGenerationException.class, () -> this.createMappingScript(providerApi, consumerApi, MappingDirection.CONSUMER_TO_PROVIDER));
        assertTrue(exception.getMessage().contains("is unbounded"));
    }
    
    private ApiMappingScript createMappingScript(String providerApi, String consumerApi, MappingDirection mappingDirection) {
        ProviderApiDefinition providerDefinition = ProviderApiLoader.loadFromString(0, providerApi, false, Optional.empty());
        ConsumerApiDefinition consumerDefinition = ConsumerApiLoader.loadFromString(consumerApi, 0);
        
        RevisionHistory revisionHistory = new RevisionHistory(providerDefinition);
        DefinitionResolution resolution = new DefinitionResolver().resolveConsumerDefinition(revisionHistory, Collections.singleton(0), consumerDefinition);
        
        return new ApiMappingScriptGenerator().generateMappingScript(resolution, mappingDirection);
    }
    
}
