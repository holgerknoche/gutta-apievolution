package gutta.apievolution.core.apimodel.provider;

import gutta.apievolution.core.apimodel.Optionality;
import gutta.apievolution.core.apimodel.QualifiedName;
import gutta.apievolution.core.apimodel.StringType;
import org.junit.jupiter.api.Test;

import java.util.Collections;
import java.util.Optional;

class ProviderInheritanceTest {

    @Test
    void inheritanceTest() {
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

        ProviderField fieldA = new ProviderField("fieldA",
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

        ProviderField fieldB = new ProviderField("fieldB",
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

        ProviderField fieldC = new ProviderField("fieldC",
                Optional.empty(),
                typeC,
                StringType.unbounded(),
                Optionality.MANDATORY);

        // Finalize the API definition
        definition.finalizeDefinition();


    }



}
