package gutta.apievolution.core.resolution;

import gutta.apievolution.core.apimodel.EnumMember;
import gutta.apievolution.core.apimodel.EnumType;
import gutta.apievolution.core.apimodel.Field;
import gutta.apievolution.core.apimodel.RecordType;
import gutta.apievolution.core.apimodel.Type;
import gutta.apievolution.core.apimodel.TypeVisitor;
import gutta.apievolution.core.apimodel.UserDefinedType;
import gutta.apievolution.core.apimodel.consumer.ConsumerEnumMember;
import gutta.apievolution.core.apimodel.consumer.ConsumerField;
import gutta.apievolution.core.apimodel.provider.ProviderEnumMember;
import gutta.apievolution.core.apimodel.provider.ProviderField;
import gutta.apievolution.core.apimodel.provider.ProviderOperation;

import java.util.Comparator;
import java.util.List;
import java.util.stream.Collectors;

/**
 * Utility class for printing definition resolutions in test cases.
 */
class DefinitionResolutionPrinter implements TypeVisitor<Void> {

    private StringBuilder stringBuilder;

    private DefinitionResolution definitionResolution;

    @SuppressWarnings("rawtypes")
    String printDefinitionResolution(DefinitionResolution resolution) {
        StringBuilder builder = this.stringBuilder = new StringBuilder();
        this.definitionResolution = resolution;

        List<UserDefinedType> consumerTypes = resolution.consumerTypes()
                .filter(UserDefinedType.class::isInstance)
                .map(UserDefinedType.class::cast)
                .sorted(Comparator.comparing(type -> type.getInternalName())).collect(Collectors.toList());

        for (UserDefinedType consumerType : consumerTypes) {
            Type providerType = resolution.mapConsumerType(consumerType);

            builder.append(consumerType);
            builder.append(" -> ");
            builder.append(providerType);
            builder.append("\n");

            consumerType.accept(this);
        }

        resolution.consumerOperations().forEach(consumerOperation -> {
            ProviderOperation providerOperation = resolution.mapConsumerOperation(consumerOperation);
            
            builder.append(consumerOperation);
            builder.append(" -> ");
            builder.append(providerOperation);
            builder.append("\n");
        });
        
        return builder.toString();
    }

    @Override
    public Void handleRecordType(RecordType<?, ?, ?> recordType) {
        StringBuilder builder = this.stringBuilder;

        for (Field<?, ?> consumerField : recordType) {
            ProviderField providerField = this.definitionResolution.mapConsumerField((ConsumerField) consumerField);

            builder.append(" ");
            builder.append(consumerField);
            builder.append(" -> ");
            builder.append(providerField);
            builder.append("\n");
        }

        return null;
    }

    @Override
    public Void handleEnumType(EnumType<?, ?, ?> enumType) {
        StringBuilder builder = this.stringBuilder;

        for (EnumMember<?, ?> consumerMember : enumType) {
            ProviderEnumMember providerMember = this.definitionResolution
                    .mapConsumerEnumMember((ConsumerEnumMember) consumerMember);

            builder.append(" ");
            builder.append(consumerMember);
            builder.append(" -> ");
            builder.append(providerMember);
            builder.append("\n");
        }

        return null;
    }

}
