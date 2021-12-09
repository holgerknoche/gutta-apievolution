package gutta.apievolution.dsl;

import gutta.apievolution.core.apimodel.*;
import gutta.apievolution.dsl.parser.ApiRevisionBaseVisitor;
import gutta.apievolution.dsl.parser.ApiRevisionParser;
import org.antlr.v4.runtime.Token;

import java.util.ArrayList;
import java.util.List;
import java.util.Optional;
import java.util.function.Supplier;
import java.util.stream.Collectors;

import static gutta.apievolution.dsl.parser.ApiRevisionLexer.*;

public abstract class ApiRevisionModelBuilder<A extends ApiDefinition<A>, R extends RecordType<A, R, F>, F extends Field<R, F>, E extends EnumType<A, E, M>, M extends EnumMember<E, M>, S extends Service<A, S, O, R>, O extends ServiceOperation<S, O, R>> extends ApiRevisionBaseVisitor<Void> {
	
	private int currentTypeId;
	
	protected A currentRevision;
	
	protected Optional<A> previousRevision;
	
	protected R currentRecordType;
	
	protected E currentEnumType;
	
	protected S currentService;
	
	protected A buildRevision(final ApiRevisionParser.ApiDefinitionContext apiRevisionSpec, final Optional<A> optionalPredecessor) {
		this.currentTypeId = 0;
		this.currentRevision = null;
		this.currentRecordType = null;
		this.previousRevision = optionalPredecessor;

		apiRevisionSpec.accept(this);
		
		return this.currentRevision;
	}

	private int getNextTypeId() {
		return this.currentTypeId++;
	}
	
	@Override
	public final Void visitApiDefinition(final ApiRevisionParser.ApiDefinitionContext ctx) {
		QualifiedName name = this.buildQualifiedName(ctx.name);
		
		this.currentRevision = this.createRevision(ctx, name, this.handleAnnotations(ctx.annotations), this.previousRevision);				
		
		// Process sub elements
		ctx.elements.forEach(element -> element.accept(this));
		
		return null;
	}
	
	protected abstract A createRevision(ApiRevisionParser.ApiDefinitionContext context, QualifiedName name, List<Annotation> annotations, Optional<A> predecessor);
	
	@Override
	public final Void visitRecordType(final ApiRevisionParser.RecordTypeContext ctx) {
		String name = this.identifierAsText(ctx.name);
		
		// Determine internal name
		Optional<String> internalName = this.determineInternalName(ctx.as);
		
		// Handle modifiers, if any
		RecordTypeModifiers modifiers = this.determineModifiers(ctx);
		
		// Resolve super type, if applicable
		Optional<String> superTypeName = this.optionalIdentifierAsText(ctx.superType);		
		Optional<R> superType = this.resolveRecord(superTypeName, () -> ctx.superType.start);
						
		R recordType = this.createRecordType(ctx, name, internalName, this.getNextTypeId(), this.currentRevision, modifiers.isAbstract(), superType, modifiers.getOptionality());				
		this.registerNewRecordType(recordType);
		
		this.currentRecordType = recordType;		
		ctx.fields.forEach(field -> field.accept(this));		
		this.currentRecordType = null;
		
		return null;
	}
	
	protected abstract R createRecordType(final ApiRevisionParser.RecordTypeContext context, final String name, final Optional<String> internalName, int typeId, final A currentRevision, final boolean abstractFlag, Optional<R> superType, final Optionality optionality);
	
	protected void registerNewRecordType(final R recordType) {
		// Do nothing by default
	}
	
	private static final String MSG_ONLY_ONE_OPTIONALITY_MODIFIER = "Only one optionality modifier may be specified.";

	private RecordTypeModifiers determineModifiers(final ApiRevisionParser.RecordTypeContext context) {
		Optional<Boolean> abstractFlag = Optional.empty();
		Optional<Optionality> optionality = Optional.empty();

		// Process given modifiers
		for (ApiRevisionParser.RecordModifierContext modifierContext : context.modifiers) {
			Token modifierToken = modifierContext.start;

			switch (modifierToken.getType()) {
				case K_ABSTRACT:
					abstractFlag = setOnce(abstractFlag, Boolean.TRUE, modifierToken, () -> "Abstract modifier may only be specified once.");
					break;

				case K_OPTIONAL:
					optionality = setOnce(optionality, Optionality.OPTIONAL, modifierToken, () -> MSG_ONLY_ONE_OPTIONALITY_MODIFIER);
					break;

				case K_OPTIN:
					optionality = setOnce(optionality, Optionality.OPT_IN, modifierToken, () -> MSG_ONLY_ONE_OPTIONALITY_MODIFIER);
					break;

				case K_MANDATORY:
					optionality = setOnce(optionality, Optionality.MANDATORY, modifierToken, () -> MSG_ONLY_ONE_OPTIONALITY_MODIFIER);
					break;
			}
		}

		// Use defaults, if necessary
		return new RecordTypeModifiers(abstractFlag.orElse(Boolean.FALSE), optionality.orElse(Optionality.MANDATORY));
	}
	
	private static <T> Optional<T> setOnce(final Optional<T> currentValue, final T newValue, final Token referenceToken, final Supplier<String> errorMessageSupplier) {
		if (currentValue.isPresent()) {
			throw new APIParseException(referenceToken, errorMessageSupplier.get());
		} else {
			return Optional.of(newValue);
		}
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
			throw new APIResolutionException(nameToken.get(), "User-defined type '" + name.get() + "' is not a record type.");
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
	
	protected abstract F createField(final ApiRevisionParser.FieldContext context, final String name, final Optional<String> internalName, Type type, Optionality optionality, final R owner);
	
	protected void registerNewField(final F field) {
		// Do nothing by default
	}
	
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
		String name = this.identifierAsText(ctx.name);

		// Determine internal name
		Optional<String> internalName = this.determineInternalName(ctx.as);

		E enumType = this.createEnumType(ctx, name, internalName, this.getNextTypeId(), this.currentRevision);
		this.registerNewEnumType(enumType);

		this.currentEnumType = enumType;		
		ctx.members.forEach(member -> this.visitEnumMember(member));		
		this.currentEnumType = null;
		
		return null;
	}
	
	protected abstract E createEnumType(ApiRevisionParser.EnumTypeContext context, String name, Optional<String> internalName, int typeId, A owner);
	
	protected void registerNewEnumType(final E enumType) {
		// Do nothing by default
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
	
	protected abstract M createEnumMember(ApiRevisionParser.EnumMemberContext context, String name, Optional<String> internalName, E owner);
	
	protected void registerNewEnumMember(final M enumMember) {
		// Do nothing by default
	}
	
	@Override
	public final Void visitService(final ApiRevisionParser.ServiceContext ctx) {
		String name = this.identifierAsText(ctx.name);

		// Determine internal name
		Optional<String> internalName = this.determineInternalName(ctx.as);
		
		S service = this.createService(ctx, name, internalName, this.currentRevision);
		this.registerNewService(service);
		
		this.currentService = service;
		ctx.operations.forEach(operation -> this.visitServiceOperation(operation));		
		this.currentService = null;
		
		return null;
	}

	protected abstract S createService(ApiRevisionParser.ServiceContext context, String name, Optional<String> internalName, A owner);
	
	protected void registerNewService(final S service) {
		// Do nothing by default
	}
	
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
	
	protected void registerNewServiceOperation(final O serviceOperation) {
		// Do nothing by default
	}
	
	protected List<Annotation> handleAnnotations(final List<ApiRevisionParser.AnnotationContext> annotationContexts) {
		List<Annotation> annotations = new ArrayList<>(annotationContexts.size());
		
		for (ApiRevisionParser.AnnotationContext annotationContext : annotationContexts) {
			String typeText = annotationContext.typeToken.getText();
			String valueText = annotationContext.value.getText();
			
			// Remove leading '@'
			String typeName = typeText.substring(1);
			Annotation annotation = new Annotation(typeName, valueText);
			annotations.add(annotation);
		}
		
		return annotations;
	}	
	
	protected static String unquote(final String input) {
		int endIndex = (input.length() - 2);
		
		return input.substring(1, endIndex);
	}
	
	protected List<String> splitQualifiedName(final ApiRevisionParser.QualifiedNameContext context) {
		return context.parts.stream()
				.map(this::identifierAsText)
				.collect(Collectors.toList());
	}
	
	private QualifiedName buildQualifiedName(final ApiRevisionParser.QualifiedNameContext context) {
		List<String> parts = this.splitQualifiedName(context);
		return new QualifiedName(parts);
	}
	
	protected String identifierAsText(final ApiRevisionParser.IdentifierContext context) {
		if (context.id != null) {
			return context.id.getText();
		} else {
			return unquote(context.literal.getText());
		}
	}
	
	protected Optional<String> optionalIdentifierAsText(final ApiRevisionParser.IdentifierContext context) {
		if (context == null) {
			return Optional.empty();
		} else {
			return Optional.of(this.identifierAsText(context));
		}
	}
	
	private Optional<String> determineInternalName(final ApiRevisionParser.AsClauseContext context) {
		if (context == null) {
			return Optional.empty();
		} else {
			return Optional.of(this.identifierAsText(context.aliasName));
		}
	}
		
	private Type resolveType(final ApiRevisionParser.TypeReferenceContext context) {
		return new TypeResolver().visit(context);
	}

	private Type resolveType(ApiRevisionParser.UserDefinedTypeReferenceContext context) {
		return new TypeResolver().visitUserDefinedTypeReference(context);
	}
	
	protected abstract R assertRecordType(UserDefinedType<A> type);
	
	private static class RecordTypeModifiers {

		private final boolean abstractFlag;

		private final Optionality optionality;

		public RecordTypeModifiers(final boolean abstractFlag, final Optionality optionality) {
			this.abstractFlag = abstractFlag;
			this.optionality = optionality;
		}

		public boolean isAbstract() {
			return this.abstractFlag;
		}

		public Optionality getOptionality() {
			return this.optionality;
		}

	}
	
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
			String typeName = ApiRevisionModelBuilder.this.identifierAsText(ctx.typeName);
			Optional<UserDefinedType<A>> optionalType = ApiRevisionModelBuilder.this.currentRevision.resolveUserDefinedType(typeName);
			
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
