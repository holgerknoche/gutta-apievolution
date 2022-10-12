package gutta.apievolution.core.apimodel.provider;

import gutta.apievolution.core.apimodel.Abstract;
import gutta.apievolution.core.apimodel.Inherited;
import gutta.apievolution.core.apimodel.Optionality;
import gutta.apievolution.core.apimodel.StringType;
import org.junit.jupiter.api.Test;

import java.util.Arrays;
import java.util.Collections;
import java.util.NoSuchElementException;

import static gutta.apievolution.core.apimodel.Conventions.noAnnotations;
import static gutta.apievolution.core.apimodel.Conventions.noInternalName;
import static gutta.apievolution.core.apimodel.Conventions.noPredecessor;
import static gutta.apievolution.core.apimodel.Conventions.noSuperTypes;
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

        ProviderRecordType typeA = definition.newRecordType("TypeA", noInternalName(), 0, Abstract.YES,
                noSuperTypes(), noPredecessor());

        typeA.newField("fieldA", StringType.unbounded(), Optionality.MANDATORY);

        ProviderRecordType typeB = definition.newRecordType("TypeB", noInternalName(), 1, Abstract.NO,
                Collections.singleton(typeA), noPredecessor());

        typeB.newField("fieldB", StringType.unbounded(), Optionality.MANDATORY);

        ProviderRecordType typeC = definition.newRecordType("TypeC", noInternalName(), 2, Abstract.NO,
                Collections.singleton(typeB), noPredecessor());

        typeC.newField("fieldC", StringType.unbounded(), Optionality.MANDATORY);

        definition.newRecordType("TypeD", noInternalName(), 3, Abstract.NO, Collections.singleton(typeB),
                noPredecessor());

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

        ProviderRecordType typeA1 = revision1.newRecordType("TypeA", 0);

        ProviderField fieldA1 = typeA1.newField("fieldA", StringType.unbounded(),
                Optionality.MANDATORY);

        ProviderRecordType typeB1 = revision1.newRecordType("TypeB", 1);

        ProviderField fieldB1 = typeB1.newField("fieldB", StringType.unbounded(),
                Optionality.MANDATORY);

        revision1.finalizeDefinition();

        // Create revision 2
        ProviderApiDefinition revision2 = new ProviderApiDefinition("test", noAnnotations(), 1, revision1);

        ProviderRecordType typeC = revision2.newRecordType("TypeC", noInternalName(), 2, Abstract.YES,
                noSuperTypes(), noPredecessor());

        typeC.newField("fieldC", noInternalName(), StringType.unbounded(), Optionality.MANDATORY, Inherited.NO,
                Arrays.asList(fieldA1, fieldB1), noPredecessor());

        revision2.newRecordType("TypeA", noInternalName(), 0, Abstract.NO, Collections.singleton(typeC), typeA1);
        revision2.newRecordType("TypeB", noInternalName(), 1, Abstract.NO, Collections.singleton(typeC), typeB1);
        revision2.newRecordType("TypeD", noInternalName(), 3, Abstract.NO, Collections.singleton(typeC),
                noPredecessor());

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

        ProviderRecordType superTypeV1 = revision1.newRecordType("SuperType", noInternalName(), 0, Abstract.YES,
                noSuperTypes(), noPredecessor());

        superTypeV1.newField("fieldA", StringType.unbounded(), Optionality.MANDATORY);

        ProviderRecordType subTypeAV1 = revision1.newRecordType("SubTypeA", noInternalName(), 1, Abstract.NO,
                Collections.singleton(superTypeV1), noPredecessor());
        ProviderRecordType subTypeBV1 = revision1.newRecordType("SubTypeB", noInternalName(), 2, Abstract.NO,
                Collections.singleton(superTypeV1), noPredecessor());

        revision1.finalizeDefinition();

        // Create the second revision, in which the inherited field is removed and
        // pushed down to one of the subtypes
        ProviderApiDefinition revision2 = new ProviderApiDefinition("test", noAnnotations(), 1, revision1);

        ProviderRecordType superTypeV2 = revision2.newRecordType("SuperType", noInternalName(), 0, Abstract.YES,
                noSuperTypes(), superTypeV1);

        ProviderRecordType subTypeAV2 = revision2.newRecordType("SubTypeA", noInternalName(), 1, Abstract.NO,
                Collections.singleton(superTypeV2), subTypeAV1);

        ProviderField previousField = subTypeAV1.resolveField("fieldA").orElseThrow(NoSuchElementException::new);

        subTypeAV2.newField("fieldA", noInternalName(), StringType.unbounded(), Optionality.MANDATORY, previousField);

        revision2.newRecordType("SubTypeB", noInternalName(), 2, Abstract.NO, Collections.singleton(superTypeV2),
                subTypeBV1);

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
