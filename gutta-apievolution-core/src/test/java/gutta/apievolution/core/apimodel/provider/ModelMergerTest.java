package gutta.apievolution.core.apimodel.provider;

import gutta.apievolution.core.apimodel.*;
import org.junit.jupiter.api.Test;

import java.util.*;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;

class ModelMergerTest {

    /**
     * Ensure that an error occurs when the revisions to merge have inconsistent names.
     */
    @Test
    void testInconsistentRevisionNaming() {
        ProviderApiDefinition revision1 = new ProviderApiDefinition(QualifiedName.of("a.b"),
                Collections.emptySet(),
                0,
                Optional.empty());

        ProviderApiDefinition revision2 = new ProviderApiDefinition(QualifiedName.of("b.c"),
                Collections.emptySet(),
                0,
                Optional.empty());

        assertThrows(ModelMergeException.class, () ->
                new ModelMerger().createMergedDefinition(Arrays.asList(revision1, revision2))
                );
    }

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
                0,
                Optional.empty());

        ProviderApiDefinition mergedRevision = new ModelMerger().createMergedDefinition(Arrays.asList(revision1,
                revision2));

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

        ProviderField deletedField = new ProviderField("deletedField",
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

        ProviderField typeChangeFieldV2 = new ProviderField("typeChangeField",
                Optional.of("newTypeChangeField"),
                testTypeV2,
                AtomicType.INT_64,
                Optionality.MANDATORY,
                Optional.of(typeChangeFieldV1));

        ProviderField unchangedFieldV2 = new ProviderField("unchangedField",
                Optional.empty(),
                testTypeV2,
                StringType.unbounded(),
                Optionality.MANDATORY,
                Optional.of(unchangedFieldV1));

        ProviderField addedField = new ProviderField("addedField",
                Optional.empty(),
                testTypeV2,
                StringType.unbounded(),
                Optionality.MANDATORY,
                Optional.empty());

        // TODO
        ProviderApiDefinition mergedDefinition = new ModelMerger().createMergedDefinition(Arrays.asList(revision1,
                revision2));

        System.out.println(new ProviderApiDefinitionPrinter().printApiDefinition(mergedDefinition));
    }

}
