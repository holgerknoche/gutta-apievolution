package gutta.apievolution.dsl;

import gutta.apievolution.core.apimodel.ApiDefinition;
import gutta.apievolution.core.apimodel.AtomicType;
import gutta.apievolution.core.apimodel.EnumMember;
import gutta.apievolution.core.apimodel.EnumType;
import gutta.apievolution.core.apimodel.Field;
import gutta.apievolution.core.apimodel.ListType;
import gutta.apievolution.core.apimodel.NumericType;
import gutta.apievolution.core.apimodel.Operation;
import gutta.apievolution.core.apimodel.Optionality;
import gutta.apievolution.core.apimodel.RecordType;
import gutta.apievolution.core.apimodel.StringType;
import gutta.apievolution.core.apimodel.Type;
import gutta.apievolution.core.apimodel.UserDefinedType;
import gutta.apievolution.dsl.parser.ApiRevisionBaseVisitor;
import gutta.apievolution.dsl.parser.ApiRevisionParser;
import org.antlr.v4.runtime.Token;

import java.util.Optional;
import java.util.function.Supplier;

import static gutta.apievolution.dsl.parser.ApiRevisionLexer.K_MANDATORY;
import static gutta.apievolution.dsl.parser.ApiRevisionLexer.K_OPTIN;
import static gutta.apievolution.dsl.parser.ApiRevisionLexer.K_OPTIONAL;

/**
 * Abstract superclass for both consumer and provider API revision model
 * builders.
 *
 * @param <A> The concrete API definition type
 * @param <R> The concrete record type
 * @param <F> The concrete field type
 * @param <E> The concrete enumeration type
 * @param <M> The concrete enumeration member type
 * @param <O> The concrete service operation type
 */
abstract class ApiRevisionModelBuilderPass2<A extends ApiDefinition<A, O>, R extends RecordType<A, R, F>,
        F extends Field<R, F>, E extends EnumType<A, E, M>, M extends EnumMember<E, M>, O extends Operation<A, O, R>>
        extends ApiRevisionModelBuilderPass<A, R, F, E, M, O> {

    protected A currentRevision;

    protected Optional<A> previousRevision;

    protected R currentRecordType;

    protected E currentEnumType;

    public ApiRevisionModelBuilderPass2(String sourceName) {
        super(sourceName);
    }
    
    protected void augmentRevision(final ApiRevisionParser.ApiDefinitionContext apiRevisionSpec, A apiDefinition,
            final Optional<A> optionalPredecessor) {
        this.currentRevision = apiDefinition;
        this.currentRecordType = null;
        this.previousRevision = optionalPredecessor;

        apiRevisionSpec.accept(this);
    }

    @Override
    public final Void visitApiDefinition(final ApiRevisionParser.ApiDefinitionContext ctx) {
        // Process sub elements
        ctx.elements.forEach(element -> element.accept(this));

        return null;
    }

    @Override
    public final Void visitRecordType(final ApiRevisionParser.RecordTypeContext ctx) {
        String typeName = this.identifierAsText(ctx.name);
        R recordType = this.resolveRecord(typeName, ctx.name.start);

        // Resolve super type, if applicable
        Optional<String> optionalSuperTypeName = this.optionalIdentifierAsText(ctx.superType);
        Optional<R> optionalSuperType = this.resolveRecord(optionalSuperTypeName, () -> ctx.superType.start);
        optionalSuperType.ifPresent(recordType::addSuperType);

        this.currentRecordType = recordType;
        ctx.fields.forEach(field -> field.accept(this));
        this.currentRecordType = null;

        return null;
    }

    private R resolveRecord(String name, Token nameToken) {
        Optional<UserDefinedType<A>> optionalType = this.currentRevision.resolveUserDefinedType(name);

        if (!optionalType.isPresent()) {
            throw new APIResolutionException(nameToken, "No record type named '" + name + "'.");
        }

        R type = this.assertRecordType(optionalType.get());

        if (type == null) {
            throw new APIResolutionException(nameToken, "User-defined type '" + name + // NOSONAR
                    "' is not a record type.");
        }

        return type;
    }

    private Optional<R> resolveRecord(final Optional<String> name, final Supplier<Token> nameToken) {
        if (!name.isPresent()) {
            return Optional.empty();
        }

        Optional<UserDefinedType<A>> optionalType = this.currentRevision.resolveUserDefinedType(name.get());

        if (!optionalType.isPresent()) {
            throw new APIResolutionException(nameToken.get(), "No record type named '" + name.get() + "'.");
        }

        R type = this.assertRecordType(optionalType.get());

        if (type != null) {
            return Optional.of(type);
        } else {
            throw new APIResolutionException(nameToken.get(),
                    "User-defined type '" + name.get() + "' is not a record type.");
        }
    }

    @Override
    public final Void visitField(final ApiRevisionParser.FieldContext ctx) {
        String name = this.identifierAsText(ctx.name);
        Type type = this.resolveType(ctx.type);

        Optionality optionality = this.determineFieldModifiers(ctx.modifier);

        // Determine internal name
        String internalName = this.determineInternalName(ctx.as);

        F field = this.createField(ctx, name, internalName, type, optionality, this.currentRecordType);
        this.registerNewField(field);

        return null;
    }

    protected abstract F createField(final ApiRevisionParser.FieldContext context, final String name,
            final String internalName, Type type, Optionality optionality, final R owner);

    private Optionality determineFieldModifiers(final ApiRevisionParser.FieldModifierContext context) {
        if (context == null) {
            return Optionality.MANDATORY;
        }

        Token modifierToken = context.start;

        switch (modifierToken.getType()) {
        case K_OPTIONAL:
            return Optionality.OPTIONAL;

        case K_OPTIN:
            return Optionality.OPT_IN;

        case K_MANDATORY:
        default:
            return Optionality.MANDATORY;
        }
    }

    @Override
    public final Void visitEnumType(final ApiRevisionParser.EnumTypeContext ctx) {
        String typeName = this.identifierAsText(ctx.name);
        E enumType = this.resolveEnumType(typeName, ctx.name.start);

        this.registerNewEnumType(enumType);

        this.currentEnumType = enumType;
        ctx.members.forEach(this::visitEnumMember);
        this.currentEnumType = null;

        return null;
    }

    private E resolveEnumType(String name, Token nameToken) {
        Optional<UserDefinedType<A>> optionalType = this.currentRevision.resolveUserDefinedType(name);

        if (!optionalType.isPresent()) {
            throw new APIResolutionException(nameToken, "No enum type named '" + name + "'.");
        }

        E type = this.assertEnumType(optionalType.get());

        if (type == null) {
            throw new APIResolutionException(nameToken, "User-defined type '" + name + "' is not a enum type.");
        }

        return type;
    }

    @Override
    public final Void visitEnumMember(final ApiRevisionParser.EnumMemberContext ctx) {
        String name = this.identifierAsText(ctx.name);

        // Determine internal name
        String internalName = this.determineInternalName(ctx.as);

        M enumMember = this.createEnumMember(ctx, name, internalName, this.currentEnumType);
        this.registerNewEnumMember(enumMember);

        return null;
    }

    protected abstract M createEnumMember(ApiRevisionParser.EnumMemberContext context, String name,
            String internalName, E owner);

    @Override
    @SuppressWarnings("unchecked")
    public Void visitOperation(final ApiRevisionParser.OperationContext ctx) {
        String name = this.identifierAsText(ctx.name);

        // Determine internal name
        String internalName = this.determineInternalName(ctx.as);

        // Determine return and parameter types
        R returnType = (R) this.resolveType(ctx.resultType);
        R parameterType = (R) this.resolveType(ctx.parameterType);

        O operation = this.createOperation(ctx, name, internalName, this.currentRevision, returnType, parameterType);
        this.registerNewOperation(operation);

        // Add exceptions
        for (ApiRevisionParser.UserDefinedTypeReferenceContext exceptionCtx : ctx.exceptions) {
            operation.addThrownException((R) this.resolveType(exceptionCtx));
        }

        return null;
    }

    protected abstract O createOperation(ApiRevisionParser.OperationContext context, String name,
            String internalName, A owner, R returnType, R parameterType);

    private Type resolveType(final ApiRevisionParser.TypeReferenceContext context) {
        return new TypeResolver().visit(context);
    }

    private Type resolveType(ApiRevisionParser.UserDefinedTypeReferenceContext context) {
        return new TypeResolver().visitUserDefinedTypeReference(context);
    }

    protected abstract R assertRecordType(UserDefinedType<A> type);

    protected abstract E assertEnumType(UserDefinedType<A> type);

    private class TypeResolver
            extends ApiRevisionBaseVisitor<Type> {

        @Override
        public Type visitAtomicType(final ApiRevisionParser.AtomicTypeContext ctx) {
            String typeName = ctx.start.getText();

            switch (typeName) {
            case "int32":
                return AtomicType.INT_32;

            case "int64":
                return AtomicType.INT_64;

            default:
                throw new APIParseException(ctx.start, "Unknown atomic type " + typeName + ".");
            }
        }

        @Override
        public Type visitBoundedType(final ApiRevisionParser.BoundedTypeContext ctx) {
            String typeName = ctx.type.getText();

            switch (typeName) {
            case "string":
                return this.handleStringType(ctx);

            case "numeric":
                return this.handleNumericType(ctx);

            default:
                throw new APIParseException(ctx.type, "Unknown type " + typeName + ".");
            }
        }

        private Type handleStringType(final ApiRevisionParser.BoundedTypeContext context) {
            if (context.bound != null) {
                int length = Integer.parseInt(context.bound.getText());
                return StringType.bounded(length);
            } else {
                return StringType.unbounded();
            }
        }

        private Type handleNumericType(final ApiRevisionParser.BoundedTypeContext context) {
            int integerPlaces = Integer.parseInt(context.integerPlaces.getText());

            if (context.fractionalPlaces != null) {
                int fractionalPlaces = Integer.parseInt(context.fractionalPlaces.getText());
                return NumericType.bounded(integerPlaces, fractionalPlaces);
            } else {
                return NumericType.bounded(integerPlaces, 0);
            }
        }

        @Override
        public Type visitUserDefinedTypeReference(final ApiRevisionParser.UserDefinedTypeReferenceContext ctx) {
            String typeName = ApiRevisionModelBuilderPass2.this.identifierAsText(ctx.typeName);
            Optional<UserDefinedType<A>> optionalType = ApiRevisionModelBuilderPass2.this.currentRevision
                    .resolveUserDefinedType(typeName);

            if (optionalType.isPresent()) {
                return optionalType.get();
            } else {
                throw new APIParseException(ctx.typeName.start, "Unknown user-defined type " + typeName + ".");
            }
        }

        @Override
        public Type visitTypeReference(final ApiRevisionParser.TypeReferenceContext ctx) {
            if (ctx.unbounded != null || ctx.cardinality != null) {
                return this.handleListType(ctx);
            } else {
                return super.visitTypeReference(ctx);
            }
        }

        private Type handleListType(final ApiRevisionParser.TypeReferenceContext ctx) {
            Type elementType = this.fork().visit(ctx.typeReference());

            if (ctx.unbounded != null) {
                return ListType.unbounded(elementType);
            } else {
                int bound = Integer.parseInt(ctx.cardinality.getText());
                return ListType.bounded(elementType, bound);
            }
        }

        private TypeResolver fork() {
            return new TypeResolver();
        }

    }

}
