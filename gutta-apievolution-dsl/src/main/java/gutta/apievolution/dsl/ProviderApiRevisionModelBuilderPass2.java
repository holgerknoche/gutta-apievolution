package gutta.apievolution.dsl;

import gutta.apievolution.core.apimodel.Optionality;
import gutta.apievolution.core.apimodel.Service;
import gutta.apievolution.core.apimodel.Type;
import gutta.apievolution.core.apimodel.UserDefinedType;
import gutta.apievolution.core.apimodel.provider.*;
import gutta.apievolution.dsl.parser.ApiRevisionParser;
import org.antlr.v4.runtime.Token;

import java.util.List;
import java.util.Optional;

/**
 * Specific revision model builder for consumer API definitions.
 */
class ProviderApiRevisionModelBuilderPass2 extends ApiRevisionModelBuilderPass2<ProviderApiDefinition,
        ProviderRecordType, ProviderField, ProviderEnumType, ProviderEnumMember, ProviderService,
        ProviderServiceOperation> implements ProviderApiRevisionModelBuilderPass {

    public void augmentProviderRevision(final ApiRevisionParser.ApiDefinitionContext apiRevisionSpec,
                                   ProviderApiDefinition apiDefinition,
                                   final Optional<ProviderApiDefinition> optionalPredecessor) {

        this.augmentRevision(apiRevisionSpec, apiDefinition, optionalPredecessor);
    }

    @Override
    protected ProviderRecordType assertRecordType(final UserDefinedType<ProviderApiDefinition> type) {
        return (type instanceof ProviderRecordType) ? (ProviderRecordType) type : null;
    }

    @Override
    protected ProviderEnumType assertEnumType(final UserDefinedType<ProviderApiDefinition> type) {
        return (type instanceof ProviderEnumType) ? (ProviderEnumType) type : null;
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
                predecessor = this.resolvePredecessorField(predecessorSpec, true,
                        context.replaces.refToken, context.replaces.qualifiedName.start);
                break;

            case IMPLICIT:
                // Perform implicit predecessor resolution
                predecessor = this.resolvePredecessorField(new FieldPredecessorSpec(name), false,
                        context.name.start, context.name.start);
                break;

            case NONE:
            default:
                predecessor = Optional.empty();
                break;
        }

        // Check for a type change, as it "breaks" the predecessor relationship
        boolean typeChange = false;
        if (predecessor.isPresent()) {
            ProviderField predecessorField = predecessor.get();
            Type predecessorFieldType = predecessorField.getType();

            if (type instanceof RevisionedElement) {
                // If the current type is revisioned, we must compare the its predecessor to the predecessor
                // field's type
                Optional<?> optionalOwnTypePredecessor = ((RevisionedElement<?>) type).getPredecessor();

                if (optionalOwnTypePredecessor.isPresent()) {
                    Type ownTypePredecessor = (Type) optionalOwnTypePredecessor.get();
                    typeChange = !(ownTypePredecessor.equals(predecessorFieldType));
                } else {
                    // If no predecessor is present, we have a type change
                    typeChange = true;
                }
            } else {
                // Otherwise, the types can be imm
                typeChange = !(type.equals(predecessorFieldType));
            }
        }

        if (typeChange) {
            // If a type change is detected, it is actually a new field (hence, no predecessor)
            return new ProviderField(name, internalName, owner, type, optionality, Optional.empty());
        } else {
            return new ProviderField(name, internalName, owner, type, optionality, predecessor);
        }
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

    private Optional<ProviderField> resolvePredecessorField(final FieldPredecessorSpec predecessorSpec,
                                                            final boolean explicitReference, final Token refToken,
                                                            final Token nameToken) {
        ProviderRecordType predecessorRecordType;

        if (predecessorSpec.typeName.isPresent()) {
            // TODO Allow specifying types in predecessors
            throw new APIParseException(refToken, "Type specs in field predecessors are currently not supported.");
        }

        if (explicitReference) {
            predecessorRecordType = this.currentRecordType.getPredecessor().orElseThrow(
                    () -> new APIResolutionException(refToken,
                            "Evolution clause specified, but no predecessor structure is available.")
            );
        } else {
            Optional<ProviderRecordType> optionalPredecessor = this.currentRecordType.getPredecessor();
            if (optionalPredecessor.isPresent()) {
                predecessorRecordType = optionalPredecessor.get();
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
    protected ProviderEnumMember createEnumMember(final ApiRevisionParser.EnumMemberContext context, final String name,
                                                  final Optional<String> internalName, final ProviderEnumType owner) {
        // Resolve predecessor, if applicable
        PredecessorType predecessorType = this.determinePredecessorType(context.replaces);
        Optional<ProviderEnumMember> predecessor;
        switch (predecessorType) {
            case EXPLICIT:
                // Resolve an explicit predecessor
                String predecessorName = this.identifierAsText(context.replaces.itemName);
                predecessor = this.resolvePredecessorEnumMember(predecessorName, true,
                        context.replaces.refToken, context.replaces.itemName.start);
                break;

            case IMPLICIT:
                // Perform implicit predecessor resolution
                predecessor = this.resolvePredecessorEnumMember(name, false, context.name.start,
                        context.name.start);
                break;

            case NONE:
            default:
                predecessor = Optional.empty();
                break;
        }

        return new ProviderEnumMember(name, internalName, this.currentEnumType, predecessor);
    }

    private Optional<ProviderEnumMember> resolvePredecessorEnumMember(final String name,
                                                                      final boolean explicitReference,
                                                                      final Token refToken, final Token nameToken) {
        ProviderEnumType predecessorEnumType;

        if (explicitReference) {
            predecessorEnumType = this.currentEnumType.getPredecessor().orElseThrow(
                    () -> new APIResolutionException(refToken,
                            "Replaces clause specified, but no predecessor is available.")
            );
        } else {
            Optional<ProviderEnumType> optionalPredecessor = this.currentEnumType.getPredecessor();
            if (optionalPredecessor.isPresent()) {
                predecessorEnumType = optionalPredecessor.get();
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
    protected ProviderService createService(final ApiRevisionParser.ServiceContext context, final String name,
                                            final Optional<String> internalName, final ProviderApiDefinition owner) {
        // Resolve predecessor, if applicable
        PredecessorType predecessorType = this.determinePredecessorType(context.replaces);
        Optional<ProviderService> predecessor;
        switch (predecessorType) {
            case EXPLICIT:
                // Resolve an explicit predecessor
                String predecessorName = this.identifierAsText(context.replaces.itemName);
                predecessor = this.resolvePredecessorService(predecessorName, true,
                        context.replaces.refToken, context.replaces.itemName.start);
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

    private Optional<ProviderService> resolvePredecessorService(final String name, final boolean explicitReference,
                                                                final Token refToken, final Token nameToken) {
        ProviderApiDefinition predecessorRevision;

        if (explicitReference) {
            // If an explicit reference is made, a predecessor revision is required
            predecessorRevision = this.previousRevision.orElseThrow(() -> new APIResolutionException(refToken,
                    "Replaces clause specified, but no predecessor model is available."));
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
            throw new APIResolutionException(nameToken, "Predecessor element '" + name +
                    "' exists, but is not a service.");
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
                predecessor = this.resolvePredecessorServiceOperation(predecessorName, true,
                        context.replaces.refToken, context.replaces.itemName.start);
                break;

            case IMPLICIT:
                // Perform implicit predecessor resolution
                predecessor = this.resolvePredecessorServiceOperation(name, false, context.name.start,
                        context.name.start);
                break;

            case NONE:
            default:
                predecessor = Optional.empty();
                break;
        }

        return new ProviderServiceOperation(name, internalName, owner, returnType, parameterType, predecessor);
    }

    private Optional<ProviderServiceOperation> resolvePredecessorServiceOperation(final String name,
                                                                                  final boolean explicitReference,
                                                                                  final Token refToken,
                                                                                  final Token nameToken) {
        ProviderService predecessorService;

        if (explicitReference) {
            predecessorService = this.currentService.getPredecessor().orElseThrow(
                    () -> new APIResolutionException(refToken,
                            "Replaces clause specified, but no predecessor service is available.")
            );
        } else {
            Optional<ProviderService> optionalPredecessor = this.currentService.getPredecessor();
            if (optionalPredecessor.isPresent()) {
                predecessorService = optionalPredecessor.get();
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
