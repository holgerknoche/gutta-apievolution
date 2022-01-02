package gutta.apievolution.dsl;

import gutta.apievolution.core.apimodel.*;
import gutta.apievolution.core.apimodel.consumer.*;
import gutta.apievolution.dsl.parser.ApiRevisionParser;

import java.util.Optional;
import java.util.Set;

/**
 * Specific revision model builder for consumer API definitions.
 */
class ConsumerApiRevisionModelBuilder extends ApiRevisionModelBuilder<ConsumerApiDefinition, ConsumerRecordType,
        ConsumerField, ConsumerEnumType, ConsumerEnumMember, ConsumerService, ConsumerServiceOperation> {

    private int referencedRevision;

    public ConsumerApiDefinition buildConsumerRevision(final int referencedRevision,
                                                       final ApiRevisionParser.ApiDefinitionContext apiRevisionSpec) {
        this.referencedRevision = referencedRevision;

        return this.buildRevision(apiRevisionSpec, Optional.empty());
    }

    @Override
    protected ConsumerApiDefinition createRevision(final ApiRevisionParser.ApiDefinitionContext context,
                                                   final QualifiedName name, final Set<Annotation> annotations,
                                                   final Optional<ConsumerApiDefinition> predecessor) {
        return new ConsumerApiDefinition(name, annotations, this.referencedRevision);
    }

    @Override
    protected ConsumerRecordType createRecordType(final ApiRevisionParser.RecordTypeContext context, final String name,
                                                  final Optional<String> internalName, final int typeId,
                                                  final ConsumerApiDefinition currentRevision,
                                                  final boolean abstractFlag,
                                                  final Optional<ConsumerRecordType> superType) {
        return new ConsumerRecordType(name, internalName, typeId, currentRevision, abstractFlag, superType);
    }

    @Override
    protected ConsumerField createField(final ApiRevisionParser.FieldContext context, final String name,
                                        final Optional<String> internalName, final Type type,
                                        final Optionality optionality, final ConsumerRecordType owner) {
        return new ConsumerField(name, internalName, owner, type, optionality);
    }

    @Override
    protected ConsumerEnumType createEnumType(final ApiRevisionParser.EnumTypeContext context, final String name,
                                              final Optional<String> internalName, final int typeId,
                                              final ConsumerApiDefinition owner) {
        return new ConsumerEnumType(name, internalName, typeId, owner);
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

}
