package gutta.apievolution.dsl;

import gutta.apievolution.core.apimodel.NumericType;
import gutta.apievolution.core.apimodel.Optionality;
import gutta.apievolution.core.apimodel.QualifiedName;
import gutta.apievolution.core.apimodel.StringType;
import gutta.apievolution.core.apimodel.provider.*;
import gutta.apievolution.core.util.IntegerRange;
import org.junit.jupiter.api.Test;

import java.io.IOException;
import java.io.InputStream;
import java.util.Collections;
import java.util.List;
import java.util.Objects;
import java.util.Optional;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import static org.junit.jupiter.api.Assertions.assertEquals;

class ProviderApiRevisionModelBuilderTest {

    /**
     * Test case: A simple model to test the overall parsing functionality.
     */
    @Test
    void testSimpleAPIDefinition() {
        // Build the expected API definition with all expected elements
        ProviderApiDefinition expectedDefinition = new ProviderApiDefinition(QualifiedName.of("test.customer"),
                Collections.emptySet(),
                0,
                Optional.empty());


        // Address type
        ProviderRecordType addressType = new ProviderRecordType("Address",
                Optional.empty(),
                1,
                expectedDefinition,
                false,
                Optional.empty(),
                Optional.empty());

        new ProviderField("street",
                Optional.empty(),
                addressType,
                StringType.unbounded(),
                Optionality.MANDATORY,
                Optional.empty()
        );

        new ProviderField("number",
                Optional.empty(),
                addressType,
                StringType.unbounded(),
                Optionality.MANDATORY,
                Optional.empty()
        );

        new ProviderField("postalCode",
                Optional.empty(),
                addressType,
                NumericType.bounded(5,0),
                Optionality.MANDATORY,
                Optional.empty()
        );

        new ProviderField("city",
                Optional.empty(),
                addressType,
                StringType.unbounded(),
                Optionality.MANDATORY,
                Optional.empty()
        );

        // Gender enum
        ProviderEnumType genderEnum = new ProviderEnumType("Gender",
                Optional.empty(),
                1,
                expectedDefinition,
                Optional.empty());

        new ProviderEnumMember("MALE",
                Optional.empty(),
                genderEnum,
                Optional.empty());

        new ProviderEnumMember("FEMALE",
                Optional.empty(),
                genderEnum,
                Optional.empty());

        new ProviderEnumMember("THIRD",
                Optional.empty(),
                genderEnum,
                Optional.empty());

        // Customer type
        ProviderRecordType customerType = new ProviderRecordType("Customer",
                Optional.empty(),
                2,
                expectedDefinition,
                false,
                Optional.empty(),
                Optional.empty());

        new ProviderField("firstName",
                Optional.empty(),
                customerType,
                StringType.unbounded(),
                Optionality.MANDATORY,
                Optional.empty()
        );

        new ProviderField("lastName",
                Optional.empty(),
                customerType,
                StringType.unbounded(),
                Optionality.MANDATORY,
                Optional.empty()
        );

        new ProviderField("gender",
                Optional.empty(),
                customerType,
                genderEnum,
                Optionality.MANDATORY,
                Optional.empty()
        );

        new ProviderField("address",
                Optional.empty(),
                customerType,
                addressType,
                Optionality.MANDATORY,
                Optional.empty()
        );

        // Formatted address type
        ProviderRecordType formattedAddressType = new ProviderRecordType("FormattedAddress",
                Optional.empty(),
                3,
                expectedDefinition,
                false,
                Optional.empty(),
                Optional.empty());

        new ProviderField("address",
                Optional.empty(),
                formattedAddressType,
                StringType.unbounded(),
                Optionality.MANDATORY,
                Optional.empty()
        );

        // Customer service
        ProviderService customerService = new ProviderService("CustomerService",
                Optional.empty(),
                expectedDefinition,
                Optional.empty());

        new ProviderServiceOperation("save",
                Optional.empty(),
                customerService,
                customerType,
                customerType,
                Optional.empty());

        new ProviderServiceOperation("formatAddress",
                Optional.empty(),
                customerService,
                formattedAddressType,
                addressType,
                Optional.empty());

        // Load the API definition from a file
        ProviderApiDefinition loadedDefinition = this.loadDefinitions("apis/simple-model.api").get(0);

        // Compare the expected and loaded definition
        assertEquals(expectedDefinition, loadedDefinition);
    }

    private List<ProviderApiDefinition> loadDefinitions(String... fileNames) {
        try {
            ClassLoader classLoader = this.getClass().getClassLoader();

            List<InputStream> streams = Stream.of(fileNames)
                    .map(name -> classLoader.getResourceAsStream(name))
                    .filter(Objects::nonNull)
                    .collect(Collectors.toList());

            return ProviderApiLoader.loadHistoryFromStreams(IntegerRange.unbounded(), streams);
        } catch (IOException e) {
            throw new RuntimeException(e);
        }
    }

}
