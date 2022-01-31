package gutta.apievolution.core.apimodel.provider;

import gutta.apievolution.core.apimodel.Optionality;
import gutta.apievolution.core.apimodel.QualifiedName;
import gutta.apievolution.core.apimodel.StringType;
import org.junit.jupiter.api.Test;

import java.util.Arrays;
import java.util.Collections;
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
        ProviderApiDefinition definition = new ProviderApiDefinition(QualifiedName.of("test"),
                Collections.emptySet(),
                0,
                Optional.empty());

        ProviderRecordType typeA = new ProviderRecordType("TypeA",
                Optional.empty(),
                0,
                definition,
                true,
                Optional.empty());

        new ProviderField("fieldA",
                Optional.empty(),
                typeA,
                StringType.unbounded(),
                Optionality.MANDATORY);

        ProviderRecordType typeB = new ProviderRecordType("TypeB",
                Optional.empty(),
                1,
                definition,
                false,
                Optional.of(typeA),
                Optional.empty());

        new ProviderField("fieldB",
                Optional.empty(),
                typeB,
                StringType.unbounded(),
                Optionality.MANDATORY);

        ProviderRecordType typeC = new ProviderRecordType("TypeC",
                Optional.empty(),
                2,
                definition,
                false,
                Optional.of(typeB),
                Optional.empty());

        new ProviderField("fieldC",
                Optional.empty(),
                typeC,
                StringType.unbounded(),
                Optionality.MANDATORY);

        new ProviderRecordType("TypeD",
                Optional.empty(),
                3,
                definition,
                false,
                Optional.of(typeB),
                Optional.empty());

        // Finalize the API definition
        definition.finalizeDefinition();

        String expected = "api test [] {\n" +
                " record TypeA {\n" +
                "  mandatory fieldA(fieldA):string\n" +
                " }\n" +
                " record TypeB {\n" +
                "  inherited mandatory fieldA(fieldA):string\n" +
                "  mandatory fieldB(fieldB):string\n" +
                " }\n" +
                " record TypeC {\n" +
                "  inherited mandatory fieldA(fieldA):string\n" +
                "  inherited mandatory fieldB(fieldB):string\n" +
                "  mandatory fieldC(fieldC):string\n" +
                " }\n" +
                " record TypeD {\n" +
                "  inherited mandatory fieldA(fieldA):string\n" +
                "  inherited mandatory fieldB(fieldB):string\n" +
                " }\n" +
                "}\n"
                ;

        // Compare the finalized definition against the expected form
        String actual = new ProviderApiDefinitionPrinter().printApiDefinition(definition);
        assertEquals(expected, actual);
    }

    /**
     * Test case: Attributes are "pulled up" to a supertype. In the initial revision,
     * there are two types A and B, each with a single field. In the second revision,
     * this field is moved to a new supertype C, and a new subtype D is added that
     * receives the inherited field as well.
     */
    @Test
    void moveAttributesUp() {
        ProviderApiDefinition revision1 = new ProviderApiDefinition(QualifiedName.of("test"),
                Collections.emptySet(),
                0,
                Optional.empty());

        ProviderRecordType typeA1 = new ProviderRecordType("TypeA",
                Optional.empty(),
                0,
                revision1,
                false,
                Optional.empty());

        ProviderField fieldA1 = new ProviderField("fieldA",
                Optional.empty(),
                typeA1,
                StringType.unbounded(),
                Optionality.MANDATORY);

        ProviderRecordType typeB1 = new ProviderRecordType("TypeB",
                Optional.empty(),
                1,
                revision1,
                false,
                Optional.empty());

        ProviderField fieldB1 = new ProviderField("fieldB",
                Optional.empty(),
                typeB1,
                StringType.unbounded(),
                Optionality.MANDATORY);

        revision1.finalizeDefinition();

        // Create revision 2
        ProviderApiDefinition revision2 = new ProviderApiDefinition(QualifiedName.of("test"),
                Collections.emptySet(),
                1,
                Optional.of(revision1));

        ProviderRecordType typeC = new ProviderRecordType("TypeC",
                Optional.empty(),
                2,
                revision2,
                true,
                Optional.empty());

        new ProviderField("fieldC",
                Optional.empty(),
                typeC,
                StringType.unbounded(),
                Optionality.MANDATORY,
                false,
                Arrays.asList(fieldA1, fieldB1),
                Optional.empty());

        new ProviderRecordType("TypeA",
                Optional.empty(),
                0,
                revision2,
                false,
                Optional.of(typeC),
                Optional.of(typeA1));

        new ProviderRecordType("TypeB",
                Optional.empty(),
                1,
                revision2,
                false,
                Optional.of(typeC),
                Optional.of(typeB1));

        new ProviderRecordType("TypeD",
                Optional.empty(),
                3,
                revision2,
                false,
                Optional.of(typeC),
                Optional.empty());

        // Finalize the API definition
        revision2.finalizeDefinition();

        String expected = "api test [] {\n" +
                " record TypeC {\n" +
                "  mandatory fieldC(fieldC):string\n" +
                " }\n" +
                " record TypeA {\n" +
                "  inherited mandatory fieldC(fieldC):string <- fieldA\n" +
                " }\n" +
                " record TypeB {\n" +
                "  inherited mandatory fieldC(fieldC):string <- fieldB\n" +
                " }\n" +
                " record TypeD {\n" +
                "  inherited mandatory fieldC(fieldC):string\n" +
                " }\n" +
                "}\n";

        String actual = new ProviderApiDefinitionPrinter().printApiDefinition(revision2);
        assertEquals(expected, actual);
    }

}
