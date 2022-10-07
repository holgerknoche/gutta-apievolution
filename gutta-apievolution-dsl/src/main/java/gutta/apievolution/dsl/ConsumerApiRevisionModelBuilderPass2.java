package gutta.apievolution.dsl;

import gutta.apievolution.core.apimodel.Inherited;
import gutta.apievolution.core.apimodel.Optionality;
import gutta.apievolution.core.apimodel.Type;
import gutta.apievolution.core.apimodel.UserDefinedType;
import gutta.apievolution.core.apimodel.consumer.*;
import gutta.apievolution.dsl.parser.ApiRevisionParser;

import java.util.Optional;

/**
 * Specific revision model builder for consumer API definitions.
 */
class ConsumerApiRevisionModelBuilderPass2
        extends ApiRevisionModelBuilderPass2<ConsumerApiDefinition, ConsumerRecordType, ConsumerField, ConsumerEnumType,
                ConsumerEnumMember, ConsumerOperation> {

    public void augmentConsumerRevision(final ApiRevisionParser.ApiDefinitionContext apiRevisionSpec,
            ConsumerApiDefinition apiDefinition) {
        this.augmentRevision(apiRevisionSpec, apiDefinition, Optional.empty());
    }

    @Override
    protected ConsumerField createField(final ApiRevisionParser.FieldContext context, final String name,
            final String internalName, final Type type, final Optionality optionality,
            final ConsumerRecordType owner) {
        return owner.newField(name, internalName, type, optionality, Inherited.NO);
    }

    @Override
    protected ConsumerEnumMember createEnumMember(final ApiRevisionParser.EnumMemberContext context, final String name,
            final String internalName, final ConsumerEnumType owner) {
        return owner.newEnumMember(name, internalName);
    }

    @Override
    protected ConsumerOperation createOperation(final ApiRevisionParser.OperationContext context, final String name,
            final String internalName, final ConsumerApiDefinition owner, ConsumerRecordType returnType,
            ConsumerRecordType parameterType) {
        return owner.newOperation(name, internalName, returnType, parameterType);
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
