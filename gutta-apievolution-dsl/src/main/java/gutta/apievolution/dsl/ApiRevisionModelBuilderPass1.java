package gutta.apievolution.dsl;

import gutta.apievolution.core.apimodel.Abstract;
import gutta.apievolution.core.apimodel.Annotation;
import gutta.apievolution.core.apimodel.ApiDefinition;
import gutta.apievolution.core.apimodel.EnumMember;
import gutta.apievolution.core.apimodel.EnumType;
import gutta.apievolution.core.apimodel.Field;
import gutta.apievolution.core.apimodel.Operation;
import gutta.apievolution.core.apimodel.Optionality;
import gutta.apievolution.core.apimodel.QualifiedName;
import gutta.apievolution.core.apimodel.RecordKind;
import gutta.apievolution.core.apimodel.RecordType;
import gutta.apievolution.dsl.parser.ApiRevisionLexer;
import gutta.apievolution.dsl.parser.ApiRevisionParser;
import org.antlr.v4.runtime.Token;

import java.util.List;
import java.util.Optional;
import java.util.Set;
import java.util.function.Supplier;

import static gutta.apievolution.dsl.parser.ApiRevisionLexer.K_ABSTRACT;
import static gutta.apievolution.dsl.parser.ApiRevisionLexer.K_MANDATORY;
import static gutta.apievolution.dsl.parser.ApiRevisionLexer.K_OPTIN;
import static gutta.apievolution.dsl.parser.ApiRevisionLexer.K_OPTIONAL;

abstract class ApiRevisionModelBuilderPass1<A extends ApiDefinition<A, O>, R extends RecordType<A, R, F>,
        F extends Field<R, F>, E extends EnumType<A, E, M>, M extends EnumMember<E, M>, O extends Operation<A, O, R>>
        extends ApiRevisionModelBuilderPass<A, R, F, E, M, O> {

    private static final String MSG_ONLY_ONE_OPTIONALITY_MODIFIER = "Only one optionality modifier may be specified.";

    private int currentTypeId;

    private A currentRevision;

    protected Optional<A> previousRevision;
    
    protected ApiRevisionModelBuilderPass1(String sourceName) {
        super(sourceName);
    }

    protected A buildApiDefinition(final ApiRevisionParser.ApiDefinitionContext apiRevisionSpec,
            final Optional<A> optionalPredecessor) {
        this.previousRevision = optionalPredecessor;
        apiRevisionSpec.accept(this);

        return this.currentRevision;
    }

    private int getNextTypeId() {
        return this.currentTypeId++;
    }

    private static <T> Optional<T> setOnce(final Optional<T> currentValue, final T newValue, final Token referenceToken,
            final Supplier<String> errorMessageSupplier) {
        if (currentValue.isPresent()) {
            throw new APIParseException(referenceToken, errorMessageSupplier.get());
        } else {
            return Optional.of(newValue);
        }
    }

    private QualifiedName buildQualifiedName(final ApiRevisionParser.QualifiedNameContext context) {
        List<String> parts = this.splitQualifiedName(context);
        return new QualifiedName(parts);
    }

    @Override
    public Void visitApiDefinition(ApiRevisionParser.ApiDefinitionContext ctx) {
        QualifiedName name = this.buildQualifiedName(ctx.name);

        this.currentRevision = this.createRevision(ctx, name, this.handleAnnotations(ctx.annotations),
                this.previousRevision.orElse(null));

        // Process sub elements
        ctx.elements.forEach(element -> element.accept(this));

        return null;
    }

    protected abstract A createRevision(ApiRevisionParser.ApiDefinitionContext context, QualifiedName name,
            Set<Annotation> annotations, A predecessor);

    private RecordTypeModifiers determineModifiers(final ApiRevisionParser.RecordTypeContext context) {
        Optional<Abstract> abstractFlag = Optional.empty();
        Optional<Optionality> optionality = Optional.empty();

        // Process given modifiers
        for (ApiRevisionParser.RecordModifierContext modifierContext : context.modifiers) {
            Token modifierToken = modifierContext.start;

            switch (modifierToken.getType()) {
            case K_ABSTRACT:
                abstractFlag = setOnce(abstractFlag, Abstract.YES, modifierToken,
                        () -> "Abstract modifier may only be specified once.");
                break;

            case K_OPTIONAL:
                optionality = setOnce(optionality, Optionality.OPTIONAL, modifierToken,
                        () -> MSG_ONLY_ONE_OPTIONALITY_MODIFIER);
                break;

            case K_OPTIN:
                optionality = setOnce(optionality, Optionality.OPT_IN, modifierToken,
                        () -> MSG_ONLY_ONE_OPTIONALITY_MODIFIER);
                break;

            case K_MANDATORY:
                optionality = setOnce(optionality, Optionality.MANDATORY, modifierToken,
                        () -> MSG_ONLY_ONE_OPTIONALITY_MODIFIER);
                break;

            default:
                throw new IllegalArgumentException("Unsupported modifier type: " + modifierToken.getText());
            }
        }

        // Use defaults, if necessary
        return new RecordTypeModifiers(abstractFlag.orElse(Abstract.NO), optionality.orElse(Optionality.MANDATORY));
    }

    @Override
    public Void visitRecordType(ApiRevisionParser.RecordTypeContext ctx) {
        String name = this.identifierAsText(ctx.name);

        // Determine internal name
        String internalName = this.determineInternalName(ctx.as);

        // Handle modifiers, if any
        RecordTypeModifiers modifiers = this.determineModifiers(ctx);

        // Determine whether we have an exception or a record
        RecordKind recordKind = (ApiRevisionLexer.K_EXCEPTION == ctx.refToken.getType()) ? RecordKind.EXCEPTION :
            RecordKind.RECORD;

        R recordType = this.createRecordType(ctx, name, internalName, this.getNextTypeId(), this.currentRevision,
                modifiers.isAbstract(), recordKind);

        this.registerNewRecordType(recordType);

        return null;
    }

    protected abstract R createRecordType(final ApiRevisionParser.RecordTypeContext context, final String name,
            final String internalName, int typeId, final A currentRevision, final Abstract abstractFlag,
            RecordKind recordKind);

    @Override
    public Void visitEnumType(ApiRevisionParser.EnumTypeContext ctx) {
        String name = this.identifierAsText(ctx.name);

        // Determine internal name
        String internalName = this.determineInternalName(ctx.as);

        E enumType = this.createEnumType(ctx, name, internalName, this.getNextTypeId(), this.currentRevision);
        this.registerNewEnumType(enumType);

        return null;
    }

    protected abstract E createEnumType(ApiRevisionParser.EnumTypeContext context, String name,
            String internalName, int typeId, A owner);

    private static class RecordTypeModifiers {

        private final Abstract abstractFlag;

        private final Optionality optionality;

        public RecordTypeModifiers(final Abstract abstractFlag, final Optionality optionality) {
            this.abstractFlag = abstractFlag;
            this.optionality = optionality;
        }

        public Abstract isAbstract() {
            return this.abstractFlag;
        }

        public Optionality getOptionality() {
            return this.optionality;
        }

    }

}
