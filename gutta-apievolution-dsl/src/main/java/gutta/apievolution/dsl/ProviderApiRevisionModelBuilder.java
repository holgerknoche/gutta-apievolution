package gutta.apievolution.dsl;

import gutta.apievolution.core.apimodel.*;
import gutta.apievolution.core.apimodel.provider.*;
import gutta.apievolution.dsl.parser.ApiRevisionParser;
import org.antlr.v4.runtime.Token;

import java.util.List;
import java.util.Optional;

public class ProviderApiRevisionModelBuilder extends ApiRevisionModelBuilder<ProviderApiDefinition, ProviderRecordType, ProviderField, ProviderEnumType, ProviderEnumMember, ProviderService, ProviderServiceOperation> {
	
	private int revision;							
	
	public ProviderApiDefinition buildProviderRevision(final int revision, final ApiRevisionParser.ApiDefinitionContext apiRevisionSpec, final Optional<ProviderApiDefinition> optionalPredecessor) {
		this.revision = revision;
		
		return this.buildRevision(apiRevisionSpec, optionalPredecessor);
	}
	
	@Override
	protected ProviderApiDefinition createRevision(final ApiRevisionParser.ApiDefinitionContext context, final QualifiedName name, final List<Annotation> annotations, final Optional<ProviderApiDefinition> predecessor) {
		return new ProviderApiDefinition(name, annotations, this.revision, predecessor);
	}
		
	@Override
	protected ProviderRecordType createRecordType(final ApiRevisionParser.RecordTypeContext context, final String name, final Optional<String> internalName, final int typeId, final ProviderApiDefinition currentRevision, final boolean abstractFlag, final Optional<ProviderRecordType> superType, final Optionality optionality) {
		// Resolve predecessor, if applicable
		PredecessorType predecessorType = this.determinePredecessorType(context.replaces);
		Optional<ProviderRecordType> predecessor;
		switch (predecessorType) {
		case EXPLICIT:
			// Resolve an explicit predecessor
			String predecessorName = this.identifierAsText(context.replaces.itemName);
			predecessor = this.resolvePredecessorRecord(predecessorName, true, context.replaces.refToken, context.replaces.itemName.start);
			break;

		case IMPLICIT:
			// Perform implicit predecessor resolution
			predecessor = this.resolvePredecessorRecord(name, false, context.refToken, context.name.start);
			break;

		case NONE:
		default:
			predecessor = Optional.empty();
			break;
		}				

		return new ProviderRecordType(name, internalName, typeId, currentRevision, abstractFlag, superType, optionality, predecessor);
	}		
	
	@Override
	protected ProviderRecordType assertRecordType(final UserDefinedType<ProviderApiDefinition> type) {
		return (type instanceof ProviderRecordType) ? (ProviderRecordType) type : null;
	}
	
	@Override
	protected ProviderField createField(final ApiRevisionParser.FieldContext context, final String name,
										final Optional<String> internalName, final Type type,
										final Optionality optionality, final ProviderRecordType owner) {
		// Resolve predecessor field, if present
		PredecessorType predecessorType = this.determinePredecessorType(context.replaces);
		Optional<ProviderField> predecessor;
		switch (predecessorType) {
			case EXPLICIT:
				// Resolve an explicit predecessor
				FieldPredecessorSpec predecessorSpec = this.determinePredecessorSpec(context.replaces);
				predecessor = this.resolvePredecessorField(predecessorSpec, true, context.replaces.refToken, context.replaces.qualifiedName.start);
				break;

			case IMPLICIT:
				// Perform implicit predecessor resolution
				predecessor = this.resolvePredecessorField(new FieldPredecessorSpec(name), false, context.name.start, context.name.start);
				break;

			case NONE:
			default:
				predecessor = Optional.empty();
				break;
		}
				
		return new ProviderField(name, internalName, owner, type, optionality, predecessor);
	}
	
	private FieldPredecessorSpec determinePredecessorSpec(final ApiRevisionParser.FieldReplacesClauseContext context) {
		List<ApiRevisionParser.QualifiedNameContext> itemNames = context.items;

		if (itemNames.size() > 1) {
			// TODO Support multiple predecessors for pushing up fields
			throw new APIParseException(context.refToken, "More than one predecessor is currently unsupported.");
		}

		List<String> itemNameParts = this.splitQualifiedName(itemNames.get(0));
		int itemPartCount = itemNameParts.size();

		if (itemPartCount == 1) {
			return new FieldPredecessorSpec(itemNameParts.get(0));
		} else if (itemPartCount == 2) {
			return new FieldPredecessorSpec(Optional.of(itemNameParts.get(0)), itemNameParts.get(1));
		} else {
			throw new APIParseException(context.refToken, "Predecessor names may only consist of two parts.");
		}
	}
	
	private Optional<ProviderRecordType> resolvePredecessorRecord(final String name, final boolean explicitReference, final Token refToken, final Token nameToken) {
		return this.resolvePredecessorUDT(name, ProviderRecordType.class, explicitReference, refToken, nameToken, "a record type");
	}

	private Optional<ProviderField> resolvePredecessorField(final FieldPredecessorSpec predecessorSpec, final boolean explicitReference, final Token refToken, final Token nameToken) {
		ProviderRecordType predecessorRecordType;

		if (predecessorSpec.typeName.isPresent()) {
			// TODO Allow specifying types in predecessors
			throw new APIParseException(refToken, "Type specs in field predecessors are currently not supported.");
		}

		if (explicitReference) {
			predecessorRecordType = this.currentRecordType.getPredecessor().orElseThrow(() -> new APIResolutionException(refToken, "Evolution clause specified, but no predecessor structure is available."));
		} else {
			if (this.currentRecordType.getPredecessor().isPresent()) {
				predecessorRecordType = this.currentRecordType.getPredecessor().get();
			} else {
				return Optional.empty();
			}
		}

		String fieldName = predecessorSpec.getFieldName();
		Optional<ProviderField> predecessorField = predecessorRecordType.resolveField(fieldName);

		if (explicitReference && !predecessorField.isPresent()) {
			throw new APIResolutionException(nameToken, "No predecessor field named '" + fieldName + "'.");
		}
		
		return predecessorField;
	}
	
	@Override
	protected ProviderEnumType createEnumType(final ApiRevisionParser.EnumTypeContext context, final String name, final Optional<String> internalName, final int typeId, final ProviderApiDefinition owner) {
		// Resolve predecessor, if applicable
		PredecessorType predecessorType = this.determinePredecessorType(context.replaces);
		Optional<ProviderEnumType> predecessor;
		switch (predecessorType) {
		case EXPLICIT:
			// Resolve an explicit predecessor
			String predecessorName = this.identifierAsText(context.replaces.itemName);
			predecessor = this.resolvePredecessorEnum(predecessorName, true, context.replaces.refToken, context.replaces.itemName.start);
			break;

		case IMPLICIT:
			// Perform implicit predecessor resolution
			predecessor = this.resolvePredecessorEnum(name, false, context.refToken, context.name.start);
			break;

		case NONE:
		default:
			predecessor = Optional.empty();
			break;
		}

		return new ProviderEnumType(name, internalName, typeId, owner, predecessor);
	}
			
	private Optional<ProviderEnumType> resolvePredecessorEnum(final String name, final boolean explicitReference, final Token refToken, final Token nameToken) {
		return this.resolvePredecessorUDT(name, ProviderEnumType.class, explicitReference, refToken, nameToken, "an enum type");
	}
	
	@SuppressWarnings("unchecked")
	private <U extends UserDefinedType<?>> Optional<U> resolvePredecessorUDT(final String name, final Class<U> udtClass, final boolean explicitReference, final Token refToken, final Token nameToken, final String typeErrorMessage) {
		ProviderApiDefinition predecessorRevision;
		
		if (explicitReference) {
			// If an explicit reference is made, a predecessor revision is required
			predecessorRevision = this.previousRevision.orElseThrow(() -> new APIResolutionException(refToken, "Replaces clause specified, but no predecessor model is available."));	
		} else {
			// If only an implicit reference is made, there may be no predecessor revision
			if (this.previousRevision.isPresent()) {
				predecessorRevision = this.previousRevision.get();
			} else {
				return Optional.empty();
			}
		}
		
		Optional<UserDefinedType<ProviderApiDefinition>> optionalPredecessor = predecessorRevision.resolveUserDefinedType(name);

		if (!optionalPredecessor.isPresent()) {
			if (explicitReference) {
				throw new APIResolutionException(nameToken, "No predecessor element named '" + name + "'.");	
			} else {
				return Optional.empty();
			}
		}

		UserDefinedType<?> predecessorType = optionalPredecessor.get();
		if (udtClass.isAssignableFrom(predecessorType.getClass())) {
			return Optional.of((U) predecessorType);
		} else {
			throw new APIResolutionException(nameToken, "Predecessor element '" + name + "' exists, but is not " + typeErrorMessage + ".");	
		}
	}
	
	@Override
	protected ProviderEnumMember createEnumMember(final ApiRevisionParser.EnumMemberContext context, final String name, final Optional<String> internalName, final ProviderEnumType owner) {
		// Resolve predecessor, if applicable
		PredecessorType predecessorType = this.determinePredecessorType(context.replaces);
		Optional<ProviderEnumMember> predecessor;
		switch (predecessorType) {
		case EXPLICIT:
			// Resolve an explicit predecessor
			String predecessorName = this.identifierAsText(context.replaces.itemName);
			predecessor = this.resolvePredecessorEnumMember(predecessorName, true, context.replaces.refToken, context.replaces.itemName.start);
			break;

		case IMPLICIT:
			// Perform implicit predecessor resolution
			predecessor = this.resolvePredecessorEnumMember(name, false, context.name.start, context.name.start);
			break;

		case NONE:
		default:
			predecessor = Optional.empty();
			break;
		}

		return new ProviderEnumMember(name, internalName, this.currentEnumType, predecessor);
	}
		
	private Optional<ProviderEnumMember> resolvePredecessorEnumMember(final String name, final boolean explicitReference, final Token refToken, final Token nameToken) {
		ProviderEnumType predecessorEnumType;

		if (explicitReference) {
			predecessorEnumType = this.currentEnumType.getPredecessor().orElseThrow(() -> new APIResolutionException(refToken, "Replaces clause specified, but no predecessor is available."));
		} else {
			if (this.currentEnumType.getPredecessor().isPresent()) {
				predecessorEnumType = this.currentEnumType.getPredecessor().get();
			} else {
				return Optional.empty();
			}
		}
		
		Optional<ProviderEnumMember> predecessorMember = predecessorEnumType.resolveMember(name);

		if (explicitReference && !predecessorMember.isPresent()) {
			throw new APIResolutionException(nameToken, "No predecessor member named '" + name + "'.");
		}
		
		return predecessorMember;
	}
	
	@Override
	protected ProviderService createService(final ApiRevisionParser.ServiceContext context, final String name, final Optional<String> internalName, final ProviderApiDefinition owner) {
		// Resolve predecessor, if applicable
		PredecessorType predecessorType = this.determinePredecessorType(context.replaces);
		Optional<ProviderService> predecessor;
		switch (predecessorType) {
		case EXPLICIT:
			// Resolve an explicit predecessor
			String predecessorName = this.identifierAsText(context.replaces.itemName);
			predecessor = this.resolvePredecessorService(predecessorName, true, context.replaces.refToken, context.replaces.itemName.start);
			break;

		case IMPLICIT:
			// Perform implicit predecessor resolution
			predecessor = this.resolvePredecessorService(name, false, context.refToken, context.name.start);
			break;

		case NONE:
		default:
			predecessor = Optional.empty();
			break;
		}		

		return new ProviderService(name, internalName, owner, predecessor);
	}
		
	private Optional<ProviderService> resolvePredecessorService(final String name, final boolean explicitReference, final Token refToken, final Token nameToken) {
		ProviderApiDefinition predecessorRevision;
		
		if (explicitReference) {
			// If an explicit reference is made, a predecessor revision is required
			predecessorRevision = this.previousRevision.orElseThrow(() -> new APIResolutionException(refToken, "Replaces clause specified, but no predecessor model is available."));	
		} else {
			// If only an implicit reference is made, there may be no predecessor revision
			if (this.previousRevision.isPresent()) {
				predecessorRevision = this.previousRevision.get();
			} else {
				return Optional.empty();
			}
		}
		
		Optional<Service<ProviderApiDefinition, ?, ?, ?>> optionalPredecessor =
				predecessorRevision.resolveService(name);

		if (!optionalPredecessor.isPresent()) {
			throw new APIResolutionException(nameToken, "No predecessor element named '" + name + "'.");	
		}

		Service<ProviderApiDefinition, ?, ?, ?> predecessorType = optionalPredecessor.get();
		if (predecessorType instanceof ProviderService) {
			return Optional.of((ProviderService) predecessorType);
		} else {
			throw new APIResolutionException(nameToken, "Predecessor element '" + name + "' exists, but is not a service.");	
		}
	}
	
	@Override
	protected ProviderServiceOperation createServiceOperation(final ApiRevisionParser.ServiceOperationContext context,
															  final String name, final Optional<String> internalName,
															  final ProviderService owner,
															  final ProviderRecordType returnType,
															  final ProviderRecordType parameterType) {
		// Resolve predecessor, if applicable
		PredecessorType predecessorType = this.determinePredecessorType(context.replaces);
		Optional<ProviderServiceOperation> predecessor;
		switch (predecessorType) {
		case EXPLICIT:
			// Resolve an explicit predecessor
			String predecessorName = this.identifierAsText(context.replaces.itemName);
			predecessor = this.resolvePredecessorServiceOperation(predecessorName, true, context.replaces.refToken, context.replaces.itemName.start);
			break;

		case IMPLICIT:
			// Perform implicit predecessor resolution
			predecessor = this.resolvePredecessorServiceOperation(name, false, context.name.start, context.name.start);
			break;

		case NONE:
		default:
			predecessor = Optional.empty();
			break;
		}

		return new ProviderServiceOperation(name, internalName, owner, returnType, parameterType, predecessor);
	}	
	
	private Optional<ProviderServiceOperation> resolvePredecessorServiceOperation(final String name, final boolean explicitReference, final Token refToken, final Token nameToken) {
		ProviderService predecessorService;

		if (explicitReference) {
			predecessorService = this.currentService.getPredecessor().orElseThrow(() -> new APIResolutionException(refToken, "Replaces clause specified, but no predecessor service is available."));
		} else {
			if (this.currentService.getPredecessor().isPresent()) {
				predecessorService = this.currentService.getPredecessor().get();
			} else {
				return Optional.empty();
			}
		}

		Optional<ProviderServiceOperation> predecessorOperation = predecessorService.resolveServiceOperation(name);

		if (explicitReference && !predecessorOperation.isPresent()) {
			throw new APIResolutionException(nameToken, "No predecessor operation named '" + name + "'.");
		}
		
		return predecessorOperation;
	}
		
	private PredecessorType determinePredecessorType(final ApiRevisionParser.ReplacesClauseContext context) {
		if (context == null) {
			// If no replaces clause is given, we have an implicit predecessor
			return PredecessorType.IMPLICIT;
		} else if (context.nothing != null) {
			// If "nothing" is specified, there is explicitly no predecessor
			return PredecessorType.NONE;
		} else {
			// Otherwise, an explicit predecessor is given
			return PredecessorType.EXPLICIT;
		}
	}

	private PredecessorType determinePredecessorType(final ApiRevisionParser.FieldReplacesClauseContext context) {
		if (context == null) {
			// If no replaces clause is given, we have an implicit predecessor
			return PredecessorType.IMPLICIT;
		} else if (context.nothing != null) {
			// If "nothing" is specified, there is explicitly no predecessor
			return PredecessorType.NONE;
		} else {
			// Otherwise, an explicit predecessor is given
			return PredecessorType.EXPLICIT;
		}
	}	
	
	private enum PredecessorType {
		NONE,
		IMPLICIT,
		EXPLICIT
	}	

	private static class FieldPredecessorSpec {

		private final Optional<String> typeName;

		private final String fieldName;

		public FieldPredecessorSpec(final String fieldName) {
			this(Optional.empty(), fieldName);
		}

		public FieldPredecessorSpec(final Optional<String> typeName, final String fieldName) {
			this.typeName = typeName;
			this.fieldName = fieldName;
		}

		public Optional<String> getTypeName() {
			return this.typeName;
		}

		public String getFieldName() {
			return this.fieldName;
		}

	}

}
