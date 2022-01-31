package gutta.apievolution.dsl;

import gutta.apievolution.core.apimodel.Optionality;
import gutta.apievolution.core.apimodel.Type;
import gutta.apievolution.core.apimodel.UserDefinedType;
import gutta.apievolution.core.apimodel.consumer.*;
import gutta.apievolution.dsl.parser.ApiRevisionParser;

import java.util.Optional;

/**
 * Specific revision model builder for consumer API definitions.
 */
class ConsumerApiRevisionModelBuilderPass2 extends ApiRevisionModelBuilderPass2<ConsumerApiDefinition,
        ConsumerRecordType, ConsumerField, ConsumerEnumType, ConsumerEnumMember, ConsumerService,
        ConsumerServiceOperation> {

    public void augmentConsumerRevision(final ApiRevisionParser.ApiDefinitionContext apiRevisionSpec,
                                        ConsumerApiDefinition apiDefinition) {
        this.augmentRevision(apiRevisionSpec, apiDefinition, Optional.empty());
    }

    @Override
    protected ConsumerField createField(final ApiRevisionParser.FieldContext context, final String name,
                                        final Optional<String> internalName, final Type type,
                                        final Optionality optionality, final ConsumerRecordType owner) {
        return new ConsumerField(name, internalName, owner, type, optionality, false);
    }

    @Override
    protected ConsumerEnumMember createEnumMember(final ApiRevisionParser.EnumMemberContext context, final String name,
                                                  final Optional<String> internalName, final ConsumerEnumType owner) {
        return new ConsumerEnumMember(name, internalName, owner);
    }

    @Override
    protected ConsumerService createService(final ApiRevisionParser.ServiceContext context, final String name,
                                            final Optional<String> internalName, final ConsumerApiDefinition owner) {
        return new ConsumerService(name, internalName, owner);
    }

    @Override
    protected ConsumerServiceOperation createServiceOperation(final ApiRevisionParser.ServiceOperationContext context,
                                                              final String name, final Optional<String> internalName,
                                                              final ConsumerService owner,
                                                              ConsumerRecordType returnType,
                                                              ConsumerRecordType parameterType) {
        return new ConsumerServiceOperation(name, internalName, owner, returnType, parameterType);
    }

    @Override
    protected ConsumerRecordType assertRecordType(final UserDefinedType<ConsumerApiDefinition> type) {
        return (type instanceof ConsumerRecordType) ? (ConsumerRecordType) type : null;
    }

    @Override
    protected ConsumerEnumType assertEnumType(UserDefinedType<ConsumerApiDefinition> type) {
        return (type instanceof ConsumerEnumType) ? (ConsumerEnumType) type : null;
    }
}
