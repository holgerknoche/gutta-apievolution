package gutta.apievolution.dsl;

import gutta.apievolution.core.apimodel.Annotation;
import gutta.apievolution.core.apimodel.QualifiedName;
import gutta.apievolution.core.apimodel.consumer.*;
import gutta.apievolution.dsl.parser.ApiRevisionParser;

import java.util.Optional;
import java.util.Set;

class ConsumerApiRevisionModelBuilderPass1 extends ApiRevisionModelBuilderPass1<ConsumerApiDefinition,
        ConsumerRecordType, ConsumerField, ConsumerEnumType, ConsumerEnumMember, ConsumerService,
        ConsumerServiceOperation> {

    private int referencedRevision;

    public ConsumerApiDefinition buildConsumerRevision(final ApiRevisionParser.ApiDefinitionContext apiRevisionSpec,
                                               int referencedRevision) {
        this.referencedRevision = referencedRevision;
        return this.buildApiDefinition(apiRevisionSpec, Optional.empty());
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
                                                  final boolean abstractFlag) {
        return new ConsumerRecordType(name, internalName, typeId, currentRevision, abstractFlag);
    }

    @Override
    protected ConsumerEnumType createEnumType(final ApiRevisionParser.EnumTypeContext context, final String name,
                                              final Optional<String> internalName, final int typeId,
                                              final ConsumerApiDefinition owner) {
        return new ConsumerEnumType(name, internalName, typeId, owner);
    }

}
