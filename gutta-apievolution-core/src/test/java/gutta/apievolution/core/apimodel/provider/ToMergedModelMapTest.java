package gutta.apievolution.core.apimodel.provider;

import gutta.apievolution.core.apimodel.Abstract;
import gutta.apievolution.core.apimodel.TypeMap;
import gutta.apievolution.core.validation.ValidationMessage;
import gutta.apievolution.core.validation.ValidationResult;
import org.junit.jupiter.api.Test;

import java.util.Collections;

import static gutta.apievolution.core.apimodel.Conventions.noInternalName;
import static gutta.apievolution.core.apimodel.Conventions.noPredecessor;
import static gutta.apievolution.core.util.MapUtil.mapOf;
import static java.util.Arrays.asList;
import static java.util.Collections.emptyMap;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

/**
 * Test cases for the map to the merged provider model.
 */
class ToMergedModelMapTest {
    
    /**
     * Test case: A mapping of types without supertypes is accepted.
     */
    @Test
    void noSuperTypes() {
        ProviderApiDefinition sourceDefinition = ProviderApiDefinition.create("test", 0);
        
        ProviderRecordType recordA = sourceDefinition.newRecordType("Test", 0);
        
        ProviderApiDefinition targetDefinition = ProviderApiDefinition.create("target", 0);
        
        ProviderRecordType recordB = targetDefinition.newRecordType("Test", 0);
        
        TypeMap<ProviderUserDefinedType, ProviderUserDefinedType> typeMap = new TypeMap<>(mapOf(recordA, recordB));
        ToMergedModelMap map = new ToMergedModelMap(sourceDefinition, targetDefinition, typeMap, emptyMap(), emptyMap(), emptyMap());
    
        ValidationResult result = map.checkConsistency();
        assertFalse(result.hasError());
    }

    /**
     * Test case: A mapping of types is accepted where the source type has no super type, but the target type
     * has.
     */
    @Test
    void noSourceSuperType() {
        ProviderApiDefinition sourceDefinition = ProviderApiDefinition.create("test", 0);
        
        ProviderRecordType recordA = sourceDefinition.newRecordType("Test", 0);
        
        ProviderApiDefinition targetDefinition = ProviderApiDefinition.create("target", 0);
        
        ProviderRecordType superRecordB = targetDefinition.newRecordType("SuperType", 0);
        ProviderRecordType recordB = targetDefinition.newRecordType("Test", noInternalName(), 1, Abstract.NO,
                Collections.singleton(superRecordB), noPredecessor());
        
        TypeMap<ProviderUserDefinedType, ProviderUserDefinedType> typeMap = new TypeMap<>(mapOf(recordA, recordB));
        ToMergedModelMap map = new ToMergedModelMap(sourceDefinition, targetDefinition, typeMap, emptyMap(), emptyMap(), emptyMap());
    
        ValidationResult result = map.checkConsistency();
        assertFalse(result.hasError());
    }
    
    /**
     * Test case: A consistent mapping of super types is accepted.
     */
    @Test
    void consistentSuperTypes() {
        ProviderApiDefinition sourceDefinition = ProviderApiDefinition.create("test", 0);
        
        ProviderRecordType superRecordA = sourceDefinition.newRecordType("SuperType", 0);
        ProviderRecordType recordA = sourceDefinition.newRecordType("Test", noInternalName(), 1, Abstract.NO,
                Collections.singleton(superRecordA), noPredecessor());
        
        ProviderApiDefinition targetDefinition = ProviderApiDefinition.create("target", 0);
        
        ProviderRecordType superRecordB = targetDefinition.newRecordType("SuperType", 0);
        ProviderRecordType recordB = targetDefinition.newRecordType("Test", noInternalName(), 1, Abstract.NO,
                Collections.singleton(superRecordB), noPredecessor());
        
        TypeMap<ProviderUserDefinedType, ProviderUserDefinedType> typeMap = new TypeMap<>(mapOf(recordA, recordB,
                superRecordA, superRecordB));
        ToMergedModelMap map = new ToMergedModelMap(sourceDefinition, targetDefinition, typeMap, emptyMap(), emptyMap(), emptyMap());
    
        ValidationResult result = map.checkConsistency();
        assertFalse(result.hasError());
    }
    
    /**
     * Test case: An unmapped supertype is detected and reported.
     */
    @Test
    void unmappedSuperType() {
        ProviderApiDefinition sourceDefinition = ProviderApiDefinition.create("test", 0);
        
        ProviderRecordType superRecordA = sourceDefinition.newRecordType("SuperType", 0);
        ProviderRecordType recordA = sourceDefinition.newRecordType("Test", noInternalName(), 1, Abstract.NO,
                Collections.singleton(superRecordA), noPredecessor());
        
        ProviderApiDefinition targetDefinition = ProviderApiDefinition.create("target", 0);
        
        ProviderRecordType superRecordB = targetDefinition.newRecordType("SuperType", 0);
        ProviderRecordType recordB = targetDefinition.newRecordType("Test", noInternalName(), 1, Abstract.NO,
                Collections.singleton(superRecordB), noPredecessor());
        
        TypeMap<ProviderUserDefinedType, ProviderUserDefinedType> typeMap = new TypeMap<>(mapOf(recordA, recordB));
        ToMergedModelMap map = new ToMergedModelMap(sourceDefinition, targetDefinition, typeMap, emptyMap(), emptyMap(), emptyMap());
    
        ValidationResult result = map.checkConsistency();
        assertTrue(result.hasError());
        assertEquals(asList(ValidationMessage.error("Supertype 'SuperType@revision 0' of 'Test@revision 0' is not mapped.")),
                result.getMessages());
    }

    /**
     * An inconsistent super type mapping is detected and reported.
     */
    @Test
    void inconsistentSuperType() {
        ProviderApiDefinition sourceDefinition = ProviderApiDefinition.create("test", 0);
        
        ProviderRecordType superRecordA = sourceDefinition.newRecordType("SuperType", 0);
        ProviderRecordType recordA = sourceDefinition.newRecordType("Test", noInternalName(), 1, Abstract.NO,
                Collections.singleton(superRecordA), noPredecessor());
        
        ProviderApiDefinition targetDefinition = ProviderApiDefinition.create("target", 0);
        
        ProviderRecordType superRecordB = targetDefinition.newRecordType("SuperType", 0);
        ProviderRecordType recordB = targetDefinition.newRecordType("Test", noInternalName(), 1, Abstract.NO,
                Collections.singleton(superRecordB), noPredecessor());
        ProviderRecordType otherRecordB = targetDefinition.newRecordType("OtherType", 2);
        
        TypeMap<ProviderUserDefinedType, ProviderUserDefinedType> typeMap = new TypeMap<>(mapOf(recordA, recordB,
                superRecordA, otherRecordB));
        ToMergedModelMap map = new ToMergedModelMap(sourceDefinition, targetDefinition, typeMap, emptyMap(), emptyMap(), emptyMap());
    
        ValidationResult result = map.checkConsistency();
        assertTrue(result.hasError());
        assertEquals(asList(ValidationMessage.error("Mapped supertype 'OtherType@revision 0' of 'Test@revision 0' is not a supertype of 'Test@revision 0'.")),
                result.getMessages());
    }
    
}
