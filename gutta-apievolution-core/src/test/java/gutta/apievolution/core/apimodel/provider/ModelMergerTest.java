package gutta.apievolution.core.apimodel.provider;

import gutta.apievolution.core.apimodel.*;
import org.junit.jupiter.api.Test;

import java.util.*;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;

class ModelMergerTest {

    /**
     * Ensure that merging annotations works as expected.
     */
    @Test
    void testAnnotationMerging() {
        ProviderApiDefinition revision1 = new ProviderApiDefinition(QualifiedName.of("a.b"),
                new HashSet<>(
                        Arrays.asList(new Annotation("a", "b"), new Annotation("b", "c"))),
                0,
                Optional.empty());

        ProviderApiDefinition revision2 = new ProviderApiDefinition(QualifiedName.of("a.b"),
                new HashSet<>(
                        Arrays.asList(new Annotation("b", "x"), new Annotation("c", "d"))),
                1,
                Optional.empty());

        RevisionHistory revisionHistory = new RevisionHistory(revision1, revision2);
        ProviderApiDefinition mergedRevision = new ModelMerger().createMergedDefinition(revisionHistory);

        // We expect that annotations a to c exist, with the value for b from revision 2 as it is the "newer" revision
        Set<Annotation> expectedAnnotations = new HashSet<>(Arrays.asList(
                new Annotation("a", "b"),
                new Annotation("b", "x"),
                new Annotation("c", "d")
        ));

        assertEquals(expectedAnnotations, mergedRevision.getAnnotations());
    }

    /**
     * Tests that merging the fields of a type across revisions works as expected.
     */
    @Test
    void testFieldMerging() {
        ProviderApiDefinition revision1 = new ProviderApiDefinition(QualifiedName.of("test"),
                Collections.emptySet(),
                1,
                Optional.empty());

        ProviderRecordType testTypeV1 = new ProviderRecordType("Test",
                Optional.empty(),
                0,
                revision1,
                false,
                Optional.empty(),
                Optional.empty());

        ProviderField unchangedFieldV1 = new ProviderField("unchangedField",
                Optional.empty(),
                testTypeV1,
                StringType.unbounded(),
                Optionality.MANDATORY,
                Optional.empty());

        ProviderField typeChangeFieldV1 = new ProviderField("typeChangeField",
                Optional.empty(),
                testTypeV1,
                AtomicType.INT_32,
                Optionality.MANDATORY,
                Optional.empty());

        new ProviderField("deletedField",
                Optional.empty(),
                testTypeV1,
                StringType.unbounded(),
                Optionality.MANDATORY,
                Optional.empty());

        ProviderApiDefinition revision2 = new ProviderApiDefinition(QualifiedName.of("test"),
                Collections.emptySet(),
                2,
                Optional.empty());

        ProviderRecordType testTypeV2 = new ProviderRecordType("Test",
                Optional.empty(),
                0,
                revision2,
                false,
                Optional.empty(),
                Optional.of(testTypeV1));

        new ProviderField("typeChangeField",
                Optional.of("newTypeChangeField"),
                testTypeV2,
                AtomicType.INT_64,
                Optionality.MANDATORY,
                Optional.empty());

        new ProviderField("unchangedField",
                Optional.empty(),
                testTypeV2,
                StringType.unbounded(),
                Optionality.MANDATORY,
                Optional.of(unchangedFieldV1));

        new ProviderField("addedField",
                Optional.empty(),
                testTypeV2,
                StringType.unbounded(),
                Optionality.MANDATORY,
                Optional.empty());

        // Merge the test revision history into a single definition
        RevisionHistory revisionHistory = new RevisionHistory(revision1, revision2);
        ProviderApiDefinition mergedDefinition = new ModelMerger().createMergedDefinition(revisionHistory);

        // Compare the created revision against the expected one. All optional fields are opt-in because the type
        // is not used as output
        String expected = "api test [] {\n" +
                " record Test {\n" +
                "  optin typeChangeField(newTypeChangeField):int64\n" + // Must be opt-in due to the type change
                "  mandatory unchangedField(unchangedField):string\n" + // Must be mandatory as it does not change
                "  optin addedField(addedField):string\n" + // Must be opt-in because it is added
                "  optin typeChangeField(typeChangeField):int32\n" + // Must be opt-in due to type change
                "  optin deletedField(deletedField):string\n" + // Must be opt-in as it is deleted
                " }\n" +
                "}\n";

        String actual = new ProviderApiDefinitionPrinter().printApiDefinition(mergedDefinition);
        assertEquals(expected, actual);
    }

    /**
     * Tests that merging the members of an enumeration across revisions works as expected.
     */
    @Test
    public void testEnumMemberMerging() {
        ProviderApiDefinition revision1 = new ProviderApiDefinition(QualifiedName.of("test"),
                Collections.emptySet(),
                1,
                Optional.empty());

        ProviderEnumType testEnumV1 = new ProviderEnumType("Test",
                Optional.empty(),
                0,
                revision1,
                Optional.empty());

        ProviderEnumMember unchangedMember = new ProviderEnumMember("UNCHANGED",
                Optional.empty(),
                testEnumV1,
                Optional.empty());

        new ProviderEnumMember("DELETED",
                Optional.empty(),
                testEnumV1,
                Optional.empty());

        ProviderApiDefinition revision2 = new ProviderApiDefinition(QualifiedName.of("test"),
                Collections.emptySet(),
                2,
                Optional.of(revision1));

        ProviderEnumType testEnumV2 = new ProviderEnumType("Test",
                Optional.empty(),
                0,
                revision2,
                Optional.of(testEnumV1));

        new ProviderEnumMember("UNCHANGED",
                Optional.empty(),
                testEnumV2,
                Optional.of(unchangedMember));

        new ProviderEnumMember("ADDED",
                Optional.empty(),
                testEnumV2,
                Optional.empty());

        // Merge the test revision history into a single definition
        RevisionHistory revisionHistory = new RevisionHistory(revision1, revision2);
        ProviderApiDefinition mergedDefinition = new ModelMerger().createMergedDefinition(revisionHistory);

        // Compare the created revision against the expected one
        String expected = "api test [] {\n" +
                " enum Test {\n" +
                "  UNCHANGED(UNCHANGED)\n" +
                "  ADDED(ADDED)\n" +
                "  DELETED(DELETED)\n" +
                " }\n" +
                "}\n";

        String actual = new ProviderApiDefinitionPrinter().printApiDefinition(mergedDefinition);
        assertEquals(expected, actual);
    }

}
