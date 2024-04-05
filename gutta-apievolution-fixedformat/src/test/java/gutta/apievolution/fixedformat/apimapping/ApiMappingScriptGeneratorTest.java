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
        
        String expectedScript = "type index 0:\n"
                + "enum 0 [0 -> 1, 1 -> 0]\n"
                + "type index 1:\n"
                + "record 1\n"
                + "@5: copy 31 bytes\n"
                + "@0: skip 11 bytes\n"
                + "@0: map enum 0\n"
                + "@36: copy 12 bytes\n";
        
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
        
        String expectedScript = "type index 0:\n" 
                + "enum 0 [0 -> 1, 1 -> 0]\n"
                + "type index 1:\n"
                + "record 1\n"
                + "@42: map enum 0\n"
                + "@0: copy 31 bytes\n"
                + "@47: copy 12 bytes\n";
        
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
        
        String expectedScript = "type index 0:\n" 
                + "record 0\n"
                + "type index 1:\n"
                + "record 1\n"
                + "type index 2:\n"
                + "record 2\n"
                + "type index 3:\n"
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
        
        String expectedScript = "type index 0:\n" 
                + "record 0\n"
                + "type index 1:\n"
                + "record 1\n"
                + "type index 2:\n"
                + "record 2\n"
                + "type index 3:\n"
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
        
        String expectedScript = "type index 0:\n" 
                + "record 0\n"
                + "@0: copy 5 bytes\n"
                + "@0: skip 9 bytes\n"
                + "@5: copy 11 bytes\n"
                + "type index 1:\n"
                + "record 1\n"
                + "@0: map list 10 elements, source size=17, target size=26, element mapping=map record 0\n";
        
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
        
        String expectedScript = "type index 0:\n" 
                + "record 0\n"
                + "@0: copy 5 bytes\n"
                + "@14: copy 11 bytes\n"
                + "type index 1:\n"
                + "record 1\n"
                + "@0: map list 10 elements, source size=26, target size=17, element mapping=map record 0\n";
        
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
    	
    	String expectedScript = "type index 0:\n" + 
    	        "record 0\n" + 
    	        "operation testOperation param: map record 0 result: map record 0\n";
    	
    	String actualScript = new ApiMappingScriptPrinter().printMappingScript(mappingScript);
    	assertEquals(expectedScript, actualScript);
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

    /**
     * Test case: Script generation for an operation with exceptions.
     */
    @Test
    void operationMappingWithExceptions() {
        String providerApi = "api test { record Parameter {} record Result {} exception SuperException { int32 fieldA } exception SubException extends SuperException { int32 fieldB } exception OtherException { string(10) otherValue } operation op(Parameter): Result throws SuperException, OtherException }";
        String consumerApi = providerApi;
        
        ApiMappingScript mappingScript = this.createMappingScript(providerApi, consumerApi, MappingDirection.CONSUMER_TO_PROVIDER);
        
        String expectedScript = "type index 0:\n" + 
                "record 0\n" + 
                "type index 1:\n" + 
                "record 1\n" + 
                "type index 2:\n" + 
                "record 2\n" + 
                "@0: copy 5 bytes\n" + 
                "type index 3:\n" + 
                "record 3\n" + 
                "@0: copy 5 bytes\n" + 
                "@5: copy 5 bytes\n" + 
                "type index 4:\n" + 
                "record 4\n" + 
                "@0: copy 11 bytes\n" + 
                "operation op param: map record 0 result: map poly record 1->(1@1), 2->(2@2), 3->(3@3), 4->(4@4)\n";
        
        String actualScript = new ApiMappingScriptPrinter().printMappingScript(mappingScript);
        assertEquals(expectedScript, actualScript);
    }
    
    private ApiMappingScript createMappingScript(String providerApi, String consumerApi, MappingDirection mappingDirection) {
        ProviderApiDefinition providerDefinition = ProviderApiLoader.loadFromString(0, providerApi, false, Optional.empty());
        ConsumerApiDefinition consumerDefinition = ConsumerApiLoader.loadFromString(consumerApi, providerDefinition.getName().toString(), 0);
        
        RevisionHistory revisionHistory = new RevisionHistory(providerDefinition);
        DefinitionResolution resolution = new DefinitionResolver().resolveConsumerDefinition(revisionHistory, Collections.singleton(0), consumerDefinition);
        
        return new ApiMappingScriptGenerator().generateMappingScript(resolution, mappingDirection);
    }
    
}
