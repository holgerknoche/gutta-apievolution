package gutta.apievolution.core.apimodel.provider;

import gutta.apievolution.core.apimodel.AtomicType;
import gutta.apievolution.core.apimodel.Optionality;
import org.junit.jupiter.api.Test;

import static gutta.apievolution.core.apimodel.Conventions.noAnnotations;
import static gutta.apievolution.core.apimodel.Conventions.noInternalName;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

/**
 * Test cases for the {@link RevisionHistory} class.
 */
class RevisionHistoryTest {

    /**
     * Test that inconsistent names within a revision history are detected.
     */
    @Test
    void testInconsistentNames() {
        ProviderApiDefinition revision1 = ProviderApiDefinition.create("a.b", 0);
        ProviderApiDefinition revision2 = ProviderApiDefinition.create("b.c", 1);

        RevisionHistory revisionHistory = new RevisionHistory(revision1, revision2);
        InconsistentHistoryException exception = assertThrows(InconsistentHistoryException.class,
                revisionHistory::checkConsistency);

        assertTrue(exception.getMessage().contains("Different API names"));
    }

    /**
     * Test that non-monotonic revision numbers are detected.
     */
    @Test
    void testNonMonotonicRevisionNumbers() {
        ProviderApiDefinition revision1 = ProviderApiDefinition.create("a.b", 1);
        ProviderApiDefinition revision2 = ProviderApiDefinition.create("a.b", 0);

        RevisionHistory revisionHistory = new RevisionHistory(revision1, revision2);
        InconsistentHistoryException exception = assertThrows(InconsistentHistoryException.class,
                revisionHistory::checkConsistency);

        assertTrue(exception.getMessage().contains("ascending revision numbers"));
    }

    /**
     * Test that illegal type changes for fields are detected.
     */
    @Test
    void testIllegalFieldTypeChange() {

        ProviderApiDefinition revision1 = ProviderApiDefinition.create("a.b", 0);
        
        ProviderRecordType recordTypeV1 = revision1.newRecordType("Test", 0);

        ProviderField fieldV1 = recordTypeV1.newField("test", AtomicType.INT_32, Optionality.MANDATORY);

        ProviderApiDefinition revision2 = new ProviderApiDefinition("a.b", noAnnotations(), 1, revision1);

        ProviderRecordType recordTypeV2 = revision2.newRecordType("Test", noInternalName(), 0, recordTypeV1);

        recordTypeV2.newField("test", noInternalName(), AtomicType.INT_64, Optionality.MANDATORY, fieldV1);

        RevisionHistory revisionHistory = new RevisionHistory(revision1, revision2);
        InconsistentHistoryException exception = assertThrows(InconsistentHistoryException.class,
                revisionHistory::checkConsistency);

        assertTrue(exception.getMessage().contains("Illegal type change"));
    }

}
