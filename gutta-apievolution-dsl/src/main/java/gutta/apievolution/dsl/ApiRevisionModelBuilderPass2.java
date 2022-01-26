package gutta.apievolution.dsl;

import gutta.apievolution.core.apimodel.*;
import gutta.apievolution.dsl.parser.ApiRevisionBaseVisitor;
import gutta.apievolution.dsl.parser.ApiRevisionParser;
import org.antlr.v4.runtime.Token;

import java.util.*;
import java.util.function.Supplier;

import static gutta.apievolution.dsl.parser.ApiRevisionLexer.*;

/**
 * Abstract superclass for both consumer and provider API revision model builders.
 * @param <A> The concrete API definition type
 * @param <R> The concrete record type
 * @param <F> The concrete field type
 * @param <E> The concrete enumeration type
 * @param <M> The concrete enumeration member type
 * @param <S> The concrete service type
 * @param <O> The concrete service operation type
 */
abstract class ApiRevisionModelBuilderPass2<A extends ApiDefinition<A>, R extends RecordType<A, R, F>,
        F extends Field<R, F>, E extends EnumType<A, E, M>, M extends EnumMember<E, M>,
        S extends Service<A, S, O, R>, O extends ServiceOperation<S, O, R>>
        extends ApiRevisionModelBuilderPass<A, R, F, E, M, S, O> {

    protected A currentRevision;

    protected Optional<A> previousRevision;

    protected Map<String, ApiRevisionParser.RecordTypeContext> nameToRecord;

    protected R currentRecordType;

    protected E currentEnumType;

    protected S currentService;

    protected Set<String> processedRecordTypes;

    protected void augmentRevision(final ApiRevisionParser.ApiDefinitionContext apiRevisionSpec,
                                   A apiDefinition,
                                   final Optional<A> optionalPredecessor,
                                   Map<String, ApiRevisionParser.RecordTypeContext> nameToRecord) {
        this.currentRevision = apiDefinition;
        this.currentRecordType = null;
        this.processedRecordTypes = new HashSet<>();
        this.previousRevision = optionalPredecessor;
        this.nameToRecord = nameToRecord;

        apiRevisionSpec.accept(this);
    }

    @Override
    public final Void visitApiDefinition(final ApiRevisionParser.ApiDefinitionContext ctx) {
        // Process sub elements
        ctx.elements.forEach(element -> element.accept(this));

        return null;
    }

    private void processSuperType(String superTypeName) {
        ApiRevisionParser.RecordTypeContext recordTypeContext = this.nameToRecord.get(superTypeName);

        // Process the super type recursively within a scope that saves the state stored within
        // the instance variables
        try (RecursiveInvocationScope scope = new RecursiveInvocationScope()) {
            recordTypeContext.accept(this);
        }
    }

    @Override
    public final Void visitRecordType(final ApiRevisionParser.RecordTypeContext ctx) {
        String typeName = this.identifierAsText(ctx.name);
        R recordType = this.resolveRecord(typeName, ctx.name.start);

        // TODO Check equals stability for record type
        this.processedRecordTypes.add(typeName);

        // Resolve super type, if applicable
        Optional<String> optionalSuperTypeName = this.optionalIdentifierAsText(ctx.superType);
        Optional<R> optionalSuperType = this.resolveRecord(optionalSuperTypeName, () -> ctx.superType.start);

        if (optionalSuperType.isPresent()) {
            R superType = optionalSuperType.get();
            recordType.setSuperType(superType);

            // Ensure that the supertype is already initialized
            String superTypeName = optionalSuperTypeName.get();
            if (!this.processedRecordTypes.contains(superTypeName)) {
                this.processSuperType(superTypeName);
            }

            // TODO Add fields from supertypes to the current record so that they
            // appear in the desired order
        }

        this.currentRecordType = recordType;
        ctx.fields.forEach(field -> field.accept(this));
        this.currentRecordType = null;

        return null;
    }

    private void addSuperTypeFieldsTo(R targetType, R superType) {
        // Ensure that the types from the most abstract type are added first
        Optional<R> superSuperType = superType.getSuperType();
        superSuperType.ifPresent(supSupType -> this.addSuperTypeFieldsTo(targetType, supSupType));

        for (F field : superType.getDeclaredFields()) {

        }
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
            throw new APIResolutionException(nameToken.get(), "User-defined type '" + name.get() +
                    "' is not a record type.");
        }
    }

    @Override
    public final Void visitField(final ApiRevisionParser.FieldContext ctx) {
        String name = this.identifierAsText(ctx.name);
        Type type = this.resolveType(ctx.type);

        Optionality optionality = this.determineFieldModifiers(ctx.modifier);

        // Determine internal name
        Optional<String> internalName = this.determineInternalName(ctx.as);

        F field = this.createField(ctx, name, internalName, type, optionality, this.currentRecordType);
        this.registerNewField(field);

        return null;
    }

    protected abstract F createField(final ApiRevisionParser.FieldContext context, final String name,
                                     final Optional<String> internalName, Type type, Optionality optionality,
                                     final R owner);

    protected abstract F createInheritedField(F originalField, R type);

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
            throw new APIResolutionException(nameToken, "User-defined type '" + name +
                    "' is not a enum type.");
        }

        return type;
    }

    @Override
    public final Void visitEnumMember(final ApiRevisionParser.EnumMemberContext ctx) {
        String name = this.identifierAsText(ctx.name);

        // Determine internal name
        Optional<String> internalName = this.determineInternalName(ctx.as);

        M enumMember = this.createEnumMember(ctx, name, internalName, this.currentEnumType);
        this.registerNewEnumMember(enumMember);

        return null;
    }

    protected abstract M createEnumMember(ApiRevisionParser.EnumMemberContext context, String name,
                                          Optional<String> internalName, E owner);

    @Override
    public final Void visitService(final ApiRevisionParser.ServiceContext ctx) {
        String name = identifierAsText(ctx.name);

        // Determine internal name
        Optional<String> internalName = determineInternalName(ctx.as);

        S service = this.createService(ctx, name, internalName, this.currentRevision);
        this.registerNewService(service);

        this.currentService = service;
        ctx.operations.forEach(this::visitServiceOperation);
        this.currentService = null;

        return null;
    }

    protected abstract S createService(ApiRevisionParser.ServiceContext context, String name,
                                       Optional<String> internalName, A owner);

    @Override
    @SuppressWarnings("unchecked")
    public Void visitServiceOperation(final ApiRevisionParser.ServiceOperationContext ctx) {
        String name = this.identifierAsText(ctx.name);

        // Determine internal name
        Optional<String> internalName = this.determineInternalName(ctx.as);

        // Determine return and parameter types
        R returnType = (R) this.resolveType(ctx.resultType);
        R parameterType = (R) this.resolveType(ctx.parameterType);

        O operation = this.createServiceOperation(ctx, name, internalName, this.currentService, returnType,
                parameterType);
        this.registerNewServiceOperation(operation);

        return null;
    }

    protected abstract O createServiceOperation(ApiRevisionParser.ServiceOperationContext context, String name,
                                                Optional<String> internalName, S owner, R returnType, R parameterType);

    private Type resolveType(final ApiRevisionParser.TypeReferenceContext context) {
        return new TypeResolver().visit(context);
    }

    private Type resolveType(ApiRevisionParser.UserDefinedTypeReferenceContext context) {
        return new TypeResolver().visitUserDefinedTypeReference(context);
    }

    protected abstract R assertRecordType(UserDefinedType<A> type);

    protected abstract E assertEnumType(UserDefinedType<A> type);

    private class TypeResolver extends ApiRevisionBaseVisitor<Type> {

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
            Optional<UserDefinedType<A>> optionalType =
                    ApiRevisionModelBuilderPass2.this.currentRevision.resolveUserDefinedType(typeName);

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

    private class RecursiveInvocationScope implements AutoCloseable {

        private final R recordType;

        private final E enumType;

        private final S service;

        public RecursiveInvocationScope() {
            this.recordType = ApiRevisionModelBuilderPass2.this.currentRecordType;
            this.enumType = ApiRevisionModelBuilderPass2.this.currentEnumType;
            this.service = ApiRevisionModelBuilderPass2.this.currentService;
        }

        @Override
        public void close() {
            ApiRevisionModelBuilderPass2.this.currentRecordType = this.recordType;
            ApiRevisionModelBuilderPass2.this.currentEnumType = this.enumType;
            ApiRevisionModelBuilderPass2.this.currentService = this.service;
        }
    }

}
