package gutta.apievolution.core.apimodel.provider;

import gutta.apievolution.core.apimodel.AtomicType;
import gutta.apievolution.core.apimodel.Optionality;
import gutta.apievolution.core.apimodel.QualifiedName;
import org.junit.jupiter.api.Test;

import java.util.Arrays;
import java.util.Collections;
import java.util.Optional;

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
        ProviderApiDefinition revision1 = new ProviderApiDefinition(QualifiedName.of("a.b"),
                Collections.emptySet(),
                0,
                Optional.empty());

        ProviderApiDefinition revision2 = new ProviderApiDefinition(QualifiedName.of("b.c"),
                Collections.emptySet(),
                1,
                Optional.empty());

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
        ProviderApiDefinition revision1 = new ProviderApiDefinition(QualifiedName.of("a.b"),
                Collections.emptySet(),
                1,
                Optional.empty());

        ProviderApiDefinition revision2 = new ProviderApiDefinition(QualifiedName.of("a.b"),
                Collections.emptySet(),
                0,
                Optional.empty());

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

        ProviderApiDefinition revision1 = new ProviderApiDefinition(QualifiedName.of("a.b"),
                Collections.emptySet(),
                0,
                Optional.empty());

        ProviderRecordType recordTypeV1 = new ProviderRecordType("Test",
                Optional.empty(),
                0,
                revision1,
                false,
                Optional.empty(),
                Optional.empty());

        ProviderField fieldV1 = new ProviderField("test",
                Optional.empty(),
                recordTypeV1,
                AtomicType.INT_32,
                Optionality.MANDATORY);

        ProviderApiDefinition revision2 = new ProviderApiDefinition(QualifiedName.of("a.b"),
                Collections.emptySet(),
                1,
                Optional.empty());

        ProviderRecordType recordTypeV2 = new ProviderRecordType("Test",
                Optional.empty(),
                0,
                revision2,
                false,
                Optional.empty(),
                Optional.of(recordTypeV1));

        new ProviderField("test",
                Optional.empty(),
                recordTypeV2,
                AtomicType.INT_64,
                Optionality.MANDATORY,
                false,
                Arrays.asList(fieldV1),
                Optional.of(fieldV1));

        RevisionHistory revisionHistory = new RevisionHistory(revision1, revision2);
        InconsistentHistoryException exception = assertThrows(InconsistentHistoryException.class,
                revisionHistory::checkConsistency);

        assertTrue(exception.getMessage().contains("Illegal type change"));
    }

}
