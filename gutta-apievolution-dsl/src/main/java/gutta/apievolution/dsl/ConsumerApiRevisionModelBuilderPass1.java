package gutta.apievolution.dsl;

import gutta.apievolution.core.apimodel.Abstract;
import gutta.apievolution.core.apimodel.Annotation;
import gutta.apievolution.core.apimodel.QualifiedName;
import gutta.apievolution.core.apimodel.RecordKind;
import gutta.apievolution.core.apimodel.consumer.ConsumerApiDefinition;
import gutta.apievolution.core.apimodel.consumer.ConsumerEnumMember;
import gutta.apievolution.core.apimodel.consumer.ConsumerEnumType;
import gutta.apievolution.core.apimodel.consumer.ConsumerField;
import gutta.apievolution.core.apimodel.consumer.ConsumerOperation;
import gutta.apievolution.core.apimodel.consumer.ConsumerRecordType;
import gutta.apievolution.dsl.parser.ApiRevisionParser;

import java.util.Optional;
import java.util.Set;

import static gutta.apievolution.core.apimodel.Conventions.noSuperTypes;

class ConsumerApiRevisionModelBuilderPass1 extends
        ApiRevisionModelBuilderPass1<ConsumerApiDefinition, ConsumerRecordType, ConsumerField, ConsumerEnumType, ConsumerEnumMember, ConsumerOperation> {

    private String referencedApiName;

    private int referencedRevision;

    public ConsumerApiRevisionModelBuilderPass1(String sourceName) {
        super(sourceName);
    }

    public ConsumerApiDefinition buildConsumerRevision(final ApiRevisionParser.ApiDefinitionContext apiRevisionSpec, String referencedApiName,
            int referencedRevision) {
        this.referencedApiName = referencedApiName;
        this.referencedRevision = referencedRevision;
        return this.buildApiDefinition(apiRevisionSpec, Optional.empty());
    }

    @Override
    protected ConsumerApiDefinition createRevision(final ApiRevisionParser.ApiDefinitionContext context, final QualifiedName name,
            final Set<Annotation> annotations, final ConsumerApiDefinition predecessor) {
        return new ConsumerApiDefinition(name, annotations, this.referencedApiName, this.referencedRevision);
    }

    @Override
    protected ConsumerRecordType createRecordType(final ApiRevisionParser.RecordTypeContext context, final String name, final String internalName,
            final int typeId, final ConsumerApiDefinition revision, final Abstract abstractFlag, RecordKind recordKind) {
        return revision.newRecordOrExceptionType(name, internalName, typeId, abstractFlag, recordKind, noSuperTypes());
    }

    @Override
    protected ConsumerEnumType createEnumType(final ApiRevisionParser.EnumTypeContext context, final String name, final String internalName, final int typeId,
            final ConsumerApiDefinition revision) {
        return revision.newEnumType(name, internalName, typeId);
    }

}
