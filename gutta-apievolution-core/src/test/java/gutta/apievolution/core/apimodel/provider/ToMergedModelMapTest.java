package gutta.apievolution.core.apimodel.provider;

import static gutta.apievolution.core.apimodel.Conventions.noInternalName;
import static gutta.apievolution.core.apimodel.Conventions.noPredecessor;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.util.List;
import java.util.Map;
import java.util.Set;

import org.junit.jupiter.api.Test;

import gutta.apievolution.core.apimodel.Abstract;
import gutta.apievolution.core.apimodel.TypeMap;
import gutta.apievolution.core.validation.ValidationMessage;

/**
 * Test cases for the map to the merged provider model.
 */
class ToMergedModelMapTest {
    
    /**
     * Test case: A mapping of types without supertypes is accepted.
     */
    @Test
    void noSuperTypes() {
        var sourceDefinition = ProviderApiDefinition.create("test", 0);
        
        var recordA = sourceDefinition.newRecordType("Test", 0);
        
        var targetDefinition = ProviderApiDefinition.create("target", 0);
        
        var recordB = targetDefinition.newRecordType("Test", 0);
        
        var typeMap = new TypeMap<ProviderUserDefinedType, ProviderUserDefinedType>(Map.of(recordA, recordB));
        var map = new ToMergedModelMap(sourceDefinition, targetDefinition, typeMap, Map.of(), Map.of(), Map.of());
    
        var result = map.checkConsistency();
        assertFalse(result.hasError());
    }

    /**
     * Test case: A mapping of types is accepted where the source type has no super type, but the target type
     * has.
     */
    @Test
    void noSourceSuperType() {
        var sourceDefinition = ProviderApiDefinition.create("test", 0);
        
        var recordA = sourceDefinition.newRecordType("Test", 0);
        
        var targetDefinition = ProviderApiDefinition.create("target", 0);
        
        var superRecordB = targetDefinition.newRecordType("SuperType", 0);
        var recordB = targetDefinition.newRecordType("Test", noInternalName(), 1, Abstract.NO,
                Set.of(superRecordB), noPredecessor());
        
        var typeMap = new TypeMap<ProviderUserDefinedType, ProviderUserDefinedType>(Map.of(recordA, recordB));
        var map = new ToMergedModelMap(sourceDefinition, targetDefinition, typeMap, Map.of(), Map.of(), Map.of());
    
        var result = map.checkConsistency();
        assertFalse(result.hasError());
    }
    
    /**
     * Test case: A consistent mapping of super types is accepted.
     */
    @Test
    void consistentSuperTypes() {
        var sourceDefinition = ProviderApiDefinition.create("test", 0);
        
        var superRecordA = sourceDefinition.newRecordType("SuperType", 0);
        var recordA = sourceDefinition.newRecordType("Test", noInternalName(), 1, Abstract.NO,
                Set.of(superRecordA), noPredecessor());
        
        var targetDefinition = ProviderApiDefinition.create("target", 0);
        
        var superRecordB = targetDefinition.newRecordType("SuperType", 0);
        var recordB = targetDefinition.newRecordType("Test", noInternalName(), 1, Abstract.NO,
                Set.of(superRecordB), noPredecessor());
        
        var typeMap = new TypeMap<ProviderUserDefinedType, ProviderUserDefinedType>(Map.of(recordA, recordB,
                superRecordA, superRecordB));
        var map = new ToMergedModelMap(sourceDefinition, targetDefinition, typeMap, Map.of(), Map.of(), Map.of());
    
        var result = map.checkConsistency();
        assertFalse(result.hasError());
    }
    
    /**
     * Test case: An unmapped supertype is detected and reported.
     */
    @Test
    void unmappedSuperType() {
        var sourceDefinition = ProviderApiDefinition.create("test", 0);
        
        var superRecordA = sourceDefinition.newRecordType("SuperType", 0);
        var recordA = sourceDefinition.newRecordType("Test", noInternalName(), 1, Abstract.NO,
                Set.of(superRecordA), noPredecessor());
        
        var targetDefinition = ProviderApiDefinition.create("target", 0);
        
        var superRecordB = targetDefinition.newRecordType("SuperType", 0);
        var recordB = targetDefinition.newRecordType("Test", noInternalName(), 1, Abstract.NO,
                Set.of(superRecordB), noPredecessor());
        
        var typeMap = new TypeMap<ProviderUserDefinedType, ProviderUserDefinedType>(Map.of(recordA, recordB));
        var map = new ToMergedModelMap(sourceDefinition, targetDefinition, typeMap, Map.of(), Map.of(), Map.of());
    
        var result = map.checkConsistency();
        assertTrue(result.hasError());
        assertEquals(List.of(ValidationMessage.error("Supertype 'SuperType@revision 0' of 'Test@revision 0' is not mapped.")),
                result.getMessages());
    }

    /**
     * An inconsistent super type mapping is detected and reported.
     */
    @Test
    void inconsistentSuperType() {
        var sourceDefinition = ProviderApiDefinition.create("test", 0);
        
        var superRecordA = sourceDefinition.newRecordType("SuperType", 0);
        var recordA = sourceDefinition.newRecordType("Test", noInternalName(), 1, Abstract.NO,
                Set.of(superRecordA), noPredecessor());
        
        var targetDefinition = ProviderApiDefinition.create("target", 0);
        
        var superRecordB = targetDefinition.newRecordType("SuperType", 0);
        var recordB = targetDefinition.newRecordType("Test", noInternalName(), 1, Abstract.NO,
                Set.of(superRecordB), noPredecessor());
        var otherRecordB = targetDefinition.newRecordType("OtherType", 2);
        
        var typeMap = new TypeMap<ProviderUserDefinedType, ProviderUserDefinedType>(Map.of(recordA, recordB,
                superRecordA, otherRecordB));
        var map = new ToMergedModelMap(sourceDefinition, targetDefinition, typeMap, Map.of(), Map.of(), Map.of());
    
        var result = map.checkConsistency();
        assertTrue(result.hasError());
        assertEquals(List.of(ValidationMessage.error("Mapped supertype 'OtherType@revision 0' of 'Test@revision 0' is not a supertype of 'Test@revision 0'.")),
                result.getMessages());
    }
    
}
