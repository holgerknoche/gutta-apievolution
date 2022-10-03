package gutta.apievolution.core.apimodel.provider;

import gutta.apievolution.core.apimodel.Optionality;
import gutta.apievolution.core.apimodel.QualifiedName;
import gutta.apievolution.core.apimodel.StringType;
import org.junit.jupiter.api.Test;

import java.util.Arrays;
import java.util.Collections;
import java.util.NoSuchElementException;
import java.util.Optional;

import static org.junit.jupiter.api.Assertions.assertEquals;

/**
 * Test cases for provider inheritance.
 */
class ProviderInheritanceTest {

    /**
     * Test case: Ensure that inheritance works for a simple case.
     */
    @Test
    void basicInheritanceTest() {
        ProviderApiDefinition definition = ProviderApiDefinition.create("test", 0);

        ProviderRecordType typeA = ProviderRecordType.abstractRecord("TypeA", 0, definition);

        ProviderField.create("fieldA", typeA, StringType.unbounded(), Optionality.MANDATORY);

        ProviderRecordType typeB = ProviderRecordType.recordWithSuperType("TypeB", 1, definition, typeA);

        ProviderField.create("fieldB", typeB, StringType.unbounded(), Optionality.MANDATORY);

        ProviderRecordType typeC = ProviderRecordType.recordWithSuperType("TypeC", 2, definition, typeB);

        ProviderField.create("fieldC", typeC, StringType.unbounded(), Optionality.MANDATORY);

        ProviderRecordType.recordWithSuperType("TypeD", 3, definition, typeB);

        // Finalize the API definition
        definition.finalizeDefinition();

        String expected = "api test [] {\n" + " abstract record TypeA(TypeA) {\n" +
                "  mandatory fieldA(fieldA):string\n" + " }\n" + " record TypeB(TypeB) extends TypeA {\n" +
                "  inherited mandatory fieldA(fieldA):string\n" + "  mandatory fieldB(fieldB):string\n" + " }\n" +
                " record TypeC(TypeC) extends TypeB {\n" + "  inherited mandatory fieldA(fieldA):string\n" +
                "  inherited mandatory fieldB(fieldB):string\n" + "  mandatory fieldC(fieldC):string\n" + " }\n" +
                " record TypeD(TypeD) extends TypeB {\n" + "  inherited mandatory fieldA(fieldA):string\n" +
                "  inherited mandatory fieldB(fieldB):string\n" + " }\n" + "}\n";

        // Compare the finalized definition against the expected form
        String actual = new ProviderApiDefinitionPrinter().printApiDefinition(definition);
        assertEquals(expected, actual);
    }

    /**
     * Test case: Attributes are "pulled up" to a supertype. In the initial
     * revision, there are two types A and B, each with a single field. In the
     * second revision, this field is moved to a new supertype C, and a new subtype
     * D is added that receives the inherited field as well.
     */
    @Test
    void moveAttributesUp() {
        ProviderApiDefinition revision1 = ProviderApiDefinition.create("test", 0);

        ProviderRecordType typeA1 = ProviderRecordType.createRecordType("TypeA", 0, revision1);

        ProviderField fieldA1 = ProviderField.create("fieldA", typeA1, StringType.unbounded(),
                Optionality.MANDATORY);

        ProviderRecordType typeB1 = ProviderRecordType.createRecordType("TypeB", 1, revision1);

        ProviderField fieldB1 = ProviderField.create("fieldB", typeB1, StringType.unbounded(),
                Optionality.MANDATORY);

        revision1.finalizeDefinition();

        // Create revision 2
        ProviderApiDefinition revision2 = ProviderApiDefinition.withPredecessor("test", 1, revision1);

        ProviderRecordType typeC = ProviderRecordType.abstractRecord("TypeC", 2, revision2);

        new ProviderField("fieldC", null, typeC, StringType.unbounded(), Optionality.MANDATORY, false,
                Arrays.asList(fieldA1, fieldB1), null);

        ProviderRecordType.recordWithoutInternalName("TypeA", 0, revision2, false, typeC, typeA1);

        ProviderRecordType.recordWithoutInternalName("TypeB", 1, revision2, false, typeC, typeB1);

        ProviderRecordType.recordWithSuperType("TypeD", 3, revision2, typeC);

        // Finalize the API definition
        revision2.finalizeDefinition();

        String expected = "api test [] {\n" + " abstract record TypeC(TypeC) {\n" +
                "  mandatory fieldC(fieldC):string\n" + " }\n" + " record TypeA(TypeA) extends TypeC <- TypeA {\n" +
                "  inherited mandatory fieldC(fieldC):string <- fieldA\n" + " }\n" +
                " record TypeB(TypeB) extends TypeC <- TypeB {\n" +
                "  inherited mandatory fieldC(fieldC):string <- fieldB\n" + " }\n" +
                " record TypeD(TypeD) extends TypeC {\n" + "  inherited mandatory fieldC(fieldC):string\n" + " }\n" +
                "}\n";

        String actual = new ProviderApiDefinitionPrinter().printApiDefinition(revision2);
        assertEquals(expected, actual);
    }

    /**
     * Test case: Move an inherited field down the inheritance hierarchy and ensure
     * that the model is accepted.
     */
    @Test
    void moveAttributeDown() {
        // Create the first revision, in which the field is inherited in all subtypes
        ProviderApiDefinition revision1 = ProviderApiDefinition.create("test", 0);

        ProviderRecordType superTypeV1 = ProviderRecordType.abstractRecord("SuperType", 0, revision1);

        ProviderField.create("fieldA", superTypeV1, StringType.unbounded(), Optionality.MANDATORY);

        ProviderRecordType subTypeAV1 = ProviderRecordType.recordWithSuperType("SubTypeA", 1, revision1, superTypeV1);

        ProviderRecordType subTypeBV1 = new ProviderRecordType("SubTypeB", Optional.empty(), 2, revision1, false,
                Optional.of(superTypeV1), Optional.empty());

        revision1.finalizeDefinition();

        // Create the second revision, in which the inherited field is removed and
        // pushed down to one of the
        // subtypes
        ProviderApiDefinition revision2 = new ProviderApiDefinition(QualifiedName.of("test"), Collections.emptySet(), 1,
                Optional.of(revision1));

        ProviderRecordType superTypeV2 = new ProviderRecordType("SuperType", Optional.empty(), 0, revision2, true,
                Optional.of(superTypeV1));

        ProviderRecordType subTypeAV2 = new ProviderRecordType("SubTypeA", Optional.empty(), 1, revision2, false,
                Optional.of(superTypeV2), Optional.of(subTypeAV1));

        ProviderField previousField = subTypeAV1.resolveField("fieldA").orElseThrow(NoSuchElementException::new);

        new ProviderField("fieldA", Optional.empty(), subTypeAV2, StringType.unbounded(), Optionality.MANDATORY, false,
                Collections.emptyList(), Optional.of(previousField));

        new ProviderRecordType("SubTypeB", Optional.empty(), 2, revision2, false, Optional.of(superTypeV2),
                Optional.of(subTypeBV1));

        revision2.finalizeDefinition();

        // Assert that the models are as expected
        String expected1 = "api test [] {\n" + " abstract record SuperType(SuperType) {\n" +
                "  mandatory fieldA(fieldA):string\n" + " }\n" + " record SubTypeA(SubTypeA) extends SuperType {\n" +
                "  inherited mandatory fieldA(fieldA):string\n" + " }\n" +
                " record SubTypeB(SubTypeB) extends SuperType {\n" + "  inherited mandatory fieldA(fieldA):string\n" +
                " }\n" + "}\n";

        String expected2 = "api test [] {\n" + " abstract record SuperType(SuperType) <- SuperType {\n" + " }\n" +
                " record SubTypeA(SubTypeA) extends SuperType <- SubTypeA {\n" +
                "  mandatory fieldA(fieldA):string <- fieldA\n" + " }\n" +
                " record SubTypeB(SubTypeB) extends SuperType <- SubTypeB {\n" + " }\n" + "}\n";

        ProviderApiDefinitionPrinter printer = new ProviderApiDefinitionPrinter();
        String actual1 = printer.printApiDefinition(revision1);
        String actual2 = printer.printApiDefinition(revision2);

        assertEquals(expected1, actual1);
        assertEquals(expected2, actual2);
    }

}
