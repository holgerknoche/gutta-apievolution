package gutta.apievolution.core.apimodel.provider;

import gutta.apievolution.core.apimodel.Abstract;
import gutta.apievolution.core.apimodel.Inherited;
import gutta.apievolution.core.apimodel.Optionality;
import gutta.apievolution.core.apimodel.StringType;
import org.junit.jupiter.api.Test;

import java.util.Arrays;
import java.util.Collections;
import java.util.NoSuchElementException;
import java.util.Set;

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
        var definition = ProviderApiDefinition.create("test", 0);

        var typeA = definition.newRecordType("TypeA", noInternalName(), 0, Abstract.YES, noSuperTypes(), noPredecessor());
        typeA.newField("fieldA", StringType.unbounded(), Optionality.MANDATORY);

        var typeB = definition.newRecordType("TypeB", noInternalName(), 1, Abstract.NO, Collections.singleton(typeA), noPredecessor());
        typeB.newField("fieldB", StringType.unbounded(), Optionality.MANDATORY);

        var typeC = definition.newRecordType("TypeC", noInternalName(), 2, Abstract.NO, Collections.singleton(typeB), noPredecessor());
        typeC.newField("fieldC", StringType.unbounded(), Optionality.MANDATORY);

        definition.newRecordType("TypeD", noInternalName(), 3, Abstract.NO, Collections.singleton(typeB), noPredecessor());

        // Finalize the API definition
        definition.finalizeDefinition();

        var expected = """
api test [] {
 abstract record TypeA(TypeA) {
  mandatory fieldA(fieldA):string
 }
 record TypeB(TypeB) extends TypeA {
  inherited mandatory fieldA(fieldA):string
  mandatory fieldB(fieldB):string
 }
 record TypeC(TypeC) extends TypeB {
  inherited mandatory fieldA(fieldA):string
  inherited mandatory fieldB(fieldB):string
  mandatory fieldC(fieldC):string
 }
 record TypeD(TypeD) extends TypeB {
  inherited mandatory fieldA(fieldA):string
  inherited mandatory fieldB(fieldB):string
 }
}
"""; 
                        
        // Compare the finalized definition against the expected form
        var actual = new ProviderApiDefinitionPrinter().printApiDefinition(definition);
        assertEquals(expected, actual);
    }

    /**
     * Test case: Attributes are "pulled up" to a supertype. In the initial revision, there are two types A and B, each with a single field. In the second
     * revision, this field is moved to a new supertype C, and a new subtype D is added that receives the inherited field as well.
     */
    @Test
    void moveAttributesUp() {
        var revision1 = ProviderApiDefinition.create("test", 0);

        var typeA1 = revision1.newRecordType("TypeA", 0);
        var fieldA1 = typeA1.newField("fieldA", StringType.unbounded(), Optionality.MANDATORY);

        var typeB1 = revision1.newRecordType("TypeB", 1);
        var fieldB1 = typeB1.newField("fieldB", StringType.unbounded(), Optionality.MANDATORY);

        revision1.finalizeDefinition();

        // Create revision 2
        var revision2 = new ProviderApiDefinition("test", noAnnotations(), 1, revision1);

        var typeC = revision2.newRecordType("TypeC", noInternalName(), 2, Abstract.YES, noSuperTypes(), noPredecessor());
        typeC.newField("fieldC", noInternalName(), StringType.unbounded(), Optionality.MANDATORY, Inherited.NO, Arrays.asList(fieldA1, fieldB1),
                noPredecessor());

        revision2.newRecordType("TypeA", noInternalName(), 0, Abstract.NO, Collections.singleton(typeC), typeA1);
        revision2.newRecordType("TypeB", noInternalName(), 1, Abstract.NO, Collections.singleton(typeC), typeB1);
        revision2.newRecordType("TypeD", noInternalName(), 3, Abstract.NO, Collections.singleton(typeC), noPredecessor());

        // Finalize the API definition
        revision2.finalizeDefinition();

        var expected = """
api test [] {
 abstract record TypeC(TypeC) {
  mandatory fieldC(fieldC):string
 }
 record TypeA(TypeA) extends TypeC <- TypeA {
  inherited mandatory fieldC(fieldC):string <- fieldA
 }
 record TypeB(TypeB) extends TypeC <- TypeB {
  inherited mandatory fieldC(fieldC):string <- fieldB
 }
 record TypeD(TypeD) extends TypeC {
  inherited mandatory fieldC(fieldC):string
 }
}
""";
        
        var actual = new ProviderApiDefinitionPrinter().printApiDefinition(revision2);
        assertEquals(expected, actual);
    }

    /**
     * Test case: Move an inherited field down the inheritance hierarchy and ensure that the model is accepted.
     */
    @Test
    void moveAttributeDown() {
        // Create the first revision, in which the field is inherited in all subtypes
        var revision1 = ProviderApiDefinition.create("test", 0);

        var superTypeV1 = revision1.newRecordType("SuperType", noInternalName(), 0, Abstract.YES, noSuperTypes(), noPredecessor());
        superTypeV1.newField("fieldA", StringType.unbounded(), Optionality.MANDATORY);

        var subTypeAV1 = revision1.newRecordType("SubTypeA", noInternalName(), 1, Abstract.NO, Collections.singleton(superTypeV1),
                noPredecessor());
        var subTypeBV1 = revision1.newRecordType("SubTypeB", noInternalName(), 2, Abstract.NO, Collections.singleton(superTypeV1),
                noPredecessor());

        revision1.finalizeDefinition();

        // Create the second revision, in which the inherited field is removed and
        // pushed down to one of the subtypes
        var revision2 = new ProviderApiDefinition("test", noAnnotations(), 1, revision1);

        var superTypeV2 = revision2.newRecordType("SuperType", noInternalName(), 0, Abstract.YES, noSuperTypes(), superTypeV1);

        var previousField = subTypeAV1.resolveField("fieldA").orElseThrow(NoSuchElementException::new);
        var subTypeAV2 = revision2.newRecordType("SubTypeA", noInternalName(), 1, Abstract.NO, Collections.singleton(superTypeV2), subTypeAV1);
        subTypeAV2.newField("fieldA", noInternalName(), StringType.unbounded(), Optionality.MANDATORY, previousField);

        revision2.newRecordType("SubTypeB", noInternalName(), 2, Abstract.NO, Collections.singleton(superTypeV2), subTypeBV1);

        revision2.finalizeDefinition();

        // Assert that the models are as expected
        var expected1 = """
api test [] {
 abstract record SuperType(SuperType) {
  mandatory fieldA(fieldA):string
 }
 record SubTypeA(SubTypeA) extends SuperType {
  inherited mandatory fieldA(fieldA):string
 }
 record SubTypeB(SubTypeB) extends SuperType {
  inherited mandatory fieldA(fieldA):string
 }
}
""";

        var expected2 = """
api test [] {
 abstract record SuperType(SuperType) <- SuperType {
 }
 record SubTypeA(SubTypeA) extends SuperType <- SubTypeA {
  mandatory fieldA(fieldA):string <- fieldA
 }
 record SubTypeB(SubTypeB) extends SuperType <- SubTypeB {
 }
}
""";
        
        var printer = new ProviderApiDefinitionPrinter();
        var actual1 = printer.printApiDefinition(revision1);
        var actual2 = printer.printApiDefinition(revision2);

        assertEquals(expected1, actual1);
        assertEquals(expected2, actual2);
    }

    /**
     * Test case: Adds a type to an inheritance hierarchy by setting the supertype of a type that previously had none.
     */
    @Test
    void addTypeToInheritanceHierarchy() {
        // Revision 1
        var revision1 = ProviderApiDefinition.create("test", 0);

        var typeAV1 = revision1.newRecordType("TypeA", 0);
        var fieldAV1 = typeAV1.newField("fieldA", StringType.unbounded(), Optionality.MANDATORY);

        var typeXV1 = revision1.newRecordType("TypeX", 10);

        revision1.finalizeDefinition();

        // Revision 2
        var revision2 = ProviderApiDefinition.create("test", 1);

        var typeAV2 = revision2.newRecordType("TypeA", noInternalName(), 0, typeAV1);
        typeAV2.newField("fieldA", noInternalName(), StringType.unbounded(), Optionality.MANDATORY, fieldAV1);

        // Set TypeA as the supertype of TypeX in the second revision
        revision2.newRecordType("TypeX", noInternalName(), 10, Abstract.NO, Set.of(typeAV2), typeXV1);

        revision2.finalizeDefinition();

        // Compare the created revisions
        var expected1 = """
api test [] {
 record TypeA(TypeA) {
  mandatory fieldA(fieldA):string
 }
 record TypeX(TypeX) {
 }
}
"""; 
                
        var expected2 = """
api test [] {
 record TypeA(TypeA) <- TypeA {
  mandatory fieldA(fieldA):string <- fieldA
 }
 record TypeX(TypeX) extends TypeA <- TypeX {
  inherited mandatory fieldA(fieldA):string
 }
}
"""; 
       
        var printer = new ProviderApiDefinitionPrinter();
        var actual1 = printer.printApiDefinition(revision1);
        var actual2 = printer.printApiDefinition(revision2);

        assertEquals(expected1, actual1);
        assertEquals(expected2, actual2);
    }

    /**
     * Test case: Removes a type to an inheritance hierarchy by removing the supertype.
     */
    @Test
    void removeTypeFromInheritanceHierarchy() {
        // Revision 1
        var revision1 = ProviderApiDefinition.create("test", 0);

        var typeAV1 = revision1.newRecordType("TypeA", 0);
        var fieldAV1 = typeAV1.newField("fieldA", StringType.unbounded(), Optionality.MANDATORY);

        var typeXV1 = revision1.newRecordType("TypeX", noInternalName(), 10, Abstract.NO, Set.of(typeAV1), noPredecessor());

        revision1.finalizeDefinition();

        // Revision 2
        var revision2 = ProviderApiDefinition.create("test", 1);

        var typeAV2 = revision2.newRecordType("TypeA", noInternalName(), 0, typeAV1);
        typeAV2.newField("fieldA", noInternalName(), StringType.unbounded(), Optionality.MANDATORY, fieldAV1);

        // Remove the supertype from TypeX
        revision2.newRecordType("TypeX", noInternalName(), 10, Abstract.NO, Set.of(), typeXV1);

        revision2.finalizeDefinition();

        // Compare the created revisions
        var expected1 = """
api test [] {
 record TypeA(TypeA) {
  mandatory fieldA(fieldA):string
 }
 record TypeX(TypeX) extends TypeA {
  inherited mandatory fieldA(fieldA):string
 }
}
""";
        
        var expected2 = """
api test [] {
 record TypeA(TypeA) <- TypeA {
  mandatory fieldA(fieldA):string <- fieldA
 }
 record TypeX(TypeX) <- TypeX {
 }
}
"""; 
        
        var printer = new ProviderApiDefinitionPrinter();
        var actual1 = printer.printApiDefinition(revision1);
        var actual2 = printer.printApiDefinition(revision2);

        assertEquals(expected1, actual1);
        assertEquals(expected2, actual2);
    }

    /**
     * Test case: Moves a given type from one type hierarchy to another by changing the supertype.
     */
    @Test
    void moveTypeToAnotherInheritanceHierarchy() {
        // Revision 1
        var revision1 = ProviderApiDefinition.create("test", 0);

        var typeAV1 = revision1.newRecordType("TypeA", 0);
        var fieldAV1 = typeAV1.newField("fieldA", StringType.unbounded(), Optionality.MANDATORY);

        var typeBV1 = revision1.newRecordType("TypeB", 0);
        var fieldBV1 = typeBV1.newField("fieldB", StringType.unbounded(), Optionality.MANDATORY);

        var typeXV1 = revision1.newRecordType("TypeX", noInternalName(), 10, Abstract.NO, Set.of(typeAV1), noPredecessor());

        revision1.finalizeDefinition();

        // Revision 2
        var revision2 = ProviderApiDefinition.create("test", 1);

        var typeAV2 = revision2.newRecordType("TypeA", noInternalName(), 0, typeAV1);
        typeAV2.newField("fieldA", noInternalName(), StringType.unbounded(), Optionality.MANDATORY, fieldAV1);

        var typeBV2 = revision2.newRecordType("TypeB", noInternalName(), 0, typeBV1);
        typeBV2.newField("fieldB", noInternalName(), StringType.unbounded(), Optionality.MANDATORY, fieldBV1);

        // Set TypeA as the supertype of TypeX in the second revision
        revision2.newRecordType("TypeX", noInternalName(), 10, Abstract.NO, Set.of(typeBV2), typeXV1);

        revision2.finalizeDefinition();

        // Compare the created revisions
        var expected1 = """
api test [] {
 record TypeA(TypeA) {
  mandatory fieldA(fieldA):string
 }
 record TypeB(TypeB) {
  mandatory fieldB(fieldB):string
 }
 record TypeX(TypeX) extends TypeA {
  inherited mandatory fieldA(fieldA):string
 }
}
""";
        var expected2 = """
api test [] {
 record TypeA(TypeA) <- TypeA {
  mandatory fieldA(fieldA):string <- fieldA
 }
 record TypeB(TypeB) <- TypeB {
  mandatory fieldB(fieldB):string <- fieldB
 }
 record TypeX(TypeX) extends TypeB <- TypeX {
  inherited mandatory fieldB(fieldB):string
 }
}
""";

        var printer = new ProviderApiDefinitionPrinter();
        var actual1 = printer.printApiDefinition(revision1);
        var actual2 = printer.printApiDefinition(revision2);

        assertEquals(expected1, actual1);
        assertEquals(expected2, actual2);
    }

}
