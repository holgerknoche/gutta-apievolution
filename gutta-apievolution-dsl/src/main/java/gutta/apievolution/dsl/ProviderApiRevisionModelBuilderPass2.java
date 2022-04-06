package gutta.apievolution.dsl;

import gutta.apievolution.core.apimodel.*;
import gutta.apievolution.core.apimodel.provider.*;
import gutta.apievolution.dsl.parser.ApiRevisionParser;
import org.antlr.v4.runtime.Token;

import java.util.*;
import java.util.stream.Collectors;

/**
 * Specific revision model builder for consumer API definitions.
 */
class ProviderApiRevisionModelBuilderPass2
        extends
        ApiRevisionModelBuilderPass2<ProviderApiDefinition, ProviderRecordType, ProviderField, ProviderEnumType,
                ProviderEnumMember, ProviderOperation>
        implements ProviderApiRevisionModelBuilderPass {

    private boolean ignoreReplacements;

    public void augmentProviderRevision(final ApiRevisionParser.ApiDefinitionContext apiRevisionSpec,
            ProviderApiDefinition apiDefinition, boolean ignoreReplacements,
            final Optional<ProviderApiDefinition> optionalPredecessor) {

        this.ignoreReplacements = ignoreReplacements;
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
            final Optional<String> internalName, final Type type, final Optionality optionality,
            final ProviderRecordType owner) {
        // Resolve predecessor field, if present
        PredecessorType predecessorType = this.determinePredecessorType(context.replaces);

        Optional<ProviderField> predecessor;
        List<ProviderField> declaredPredecessors;
        switch (predecessorType) {
        case EXPLICIT:
            // Resolve explicit predecessors
            List<FieldPredecessorSpec> predecessorSpecs = this.determinePredecessorSpecs(context.replaces);
            declaredPredecessors = this.resolvePredecessorFields(predecessorSpecs, context.replaces);

            // Use the (first) predecessor owned by the current type as the
            // actual predecessor. For inherited fields, there may be no matching
            // predecessor
            predecessor = declaredPredecessors.stream().filter(field -> {
                Optional<ProviderRecordType> optionalPredecessor = owner.getPredecessor();

                if (optionalPredecessor.isPresent()) {
                    return field.getOwner().equals(owner.getPredecessor().get());
                } else {
                    return false;
                }
            }).findFirst();
            break;

        case IMPLICIT:
            // Perform implicit predecessor resolution
            predecessor = this.resolvePredecessorField(new FieldPredecessorSpec(name), false, context.name.start,
                    context.name.start);
            declaredPredecessors = Collections.emptyList();
            break;

        case NONE:
        default:
            predecessor = Optional.empty();
            declaredPredecessors = Collections.emptyList();
            break;
        }

        // Check for a type change, as it "breaks" the predecessor relationship
        boolean typeChange = false;
        if (predecessor.isPresent()) {
            ProviderField predecessorField = predecessor.get();
            Type predecessorFieldType = predecessorField.getType();

            typeChange = ProviderTypeTools.isTypeChange(predecessorFieldType, type);
        }

        if (typeChange) {
            // If a type change is detected, it is actually a new field (hence, no
            // predecessor)
            return new ProviderField(name, internalName, owner, type, optionality, false, declaredPredecessors,
                    Optional.empty());
        } else {
            return new ProviderField(name, internalName, owner, type, optionality, false, declaredPredecessors,
                    predecessor);
        }
    }

    private List<ProviderField> resolvePredecessorFields(List<FieldPredecessorSpec> specs,
            ApiRevisionParser.FieldReplacesClauseContext context) {
        // If replacements are ignored, just return "no predecessor"
        if (this.ignoreReplacements) {
            return Collections.emptyList();
        }

        int specCount = specs.size();
        Token refToken = context.refToken;

        List<ProviderField> predecessors = new ArrayList<>(specCount);
        for (int index = 0; index < specCount; index++) {
            FieldPredecessorSpec spec = specs.get(index);
            Optional<ProviderField> predecessor = this.resolvePredecessorField(spec, true, refToken,
                    context.qualifiedName(index).start);
            predecessor.ifPresent(predecessors::add);
        }

        return predecessors;
    }

    private List<FieldPredecessorSpec> determinePredecessorSpecs(
            final ApiRevisionParser.FieldReplacesClauseContext context) {
        return context.items.stream().map(this::createPredecessorSpec).collect(Collectors.toList());
    }

    private FieldPredecessorSpec createPredecessorSpec(ApiRevisionParser.QualifiedNameContext qualifiedNameContext) {
        List<String> itemNameParts = this.splitQualifiedName(qualifiedNameContext);
        int itemPartCount = itemNameParts.size();

        if (itemPartCount == 1) {
            return new FieldPredecessorSpec(itemNameParts.get(0));
        } else if (itemPartCount == 2) {
            return new FieldPredecessorSpec(Optional.of(itemNameParts.get(0)), itemNameParts.get(1));
        } else {
            Token refToken = qualifiedNameContext.start;
            throw new APIParseException(refToken, "Predecessor names may only consist of two parts.");
        }
    }

    private Optional<ProviderField> resolvePredecessorField(final FieldPredecessorSpec predecessorSpec,
            final boolean explicitReference, final Token refToken, final Token nameToken) {
        // If replacements are ignored, just return "no predecessor"
        if (this.ignoreReplacements) {
            return Optional.empty();
        }

        ProviderRecordType predecessorRecordType;

        if (predecessorSpec.typeName.isPresent()) {
            String predecessorTypeName = predecessorSpec.typeName.get();
            ProviderApiDefinition previousRevision = this.currentRevision.getPredecessor()
                    .orElseThrow(() -> new APIResolutionException(refToken, "No predecessor revision available."));

            UserDefinedType<ProviderApiDefinition> predecessorType = previousRevision
                    .resolveUserDefinedType(predecessorTypeName).orElseThrow(() -> new APIResolutionException(refToken,
                            "Predecessor type" + predecessorTypeName + "does not exist."));

            predecessorRecordType = this.assertRecordType(predecessorType);
        } else if (explicitReference) {
            predecessorRecordType = this.currentRecordType.getPredecessor()
                    .orElseThrow(() -> new APIResolutionException(refToken,
                            "Evolution clause specified, but no predecessor structure is available."));
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
            predecessor = this.resolvePredecessorEnumMember(predecessorName, true, context.replaces.refToken,
                    context.replaces.itemName.start);
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

    private Optional<ProviderEnumMember> resolvePredecessorEnumMember(final String name,
            final boolean explicitReference, final Token refToken, final Token nameToken) {
        // If replacements are ignored, just return "no predecessor"
        if (this.ignoreReplacements) {
            return Optional.empty();
        }

        ProviderEnumType predecessorEnumType;

        if (explicitReference) {
            predecessorEnumType = this.currentEnumType.getPredecessor()
                    .orElseThrow(() -> new APIResolutionException(refToken,
                            "Replaces clause specified, but no predecessor is available."));
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
    protected ProviderOperation createOperation(final ApiRevisionParser.OperationContext context, final String name,
            final Optional<String> internalName, final ProviderApiDefinition owner, final ProviderRecordType returnType,
            final ProviderRecordType parameterType) {
        // Resolve predecessor, if applicable
        PredecessorType predecessorType = this.determinePredecessorType(context.replaces);
        Optional<ProviderOperation> predecessor;
        switch (predecessorType) {
        case EXPLICIT:
            // Resolve an explicit predecessor
            String predecessorName = this.identifierAsText(context.replaces.itemName);
            predecessor = this.resolvePredecessorOperation(predecessorName, true, context.replaces.refToken,
                    context.replaces.itemName.start);
            break;

        case IMPLICIT:
            // Perform implicit predecessor resolution
            predecessor = this.resolvePredecessorOperation(name, false, context.name.start, context.name.start);
            break;

        case NONE:
        default:
            predecessor = Optional.empty();
            break;
        }

        return new ProviderOperation(this.handleAnnotations(context.annotations), name, internalName, owner, returnType,
                parameterType, predecessor);
    }

    private Optional<ProviderOperation> resolvePredecessorOperation(final String name, final boolean explicitReference,
            final Token refToken, final Token nameToken) {
        // If replacements are ignored, just return "no predecessor"
        if (this.ignoreReplacements) {
            return Optional.empty();
        }

        Optional<ProviderOperation> predecessorOperation = (this.previousRevision.isPresent()) ?
                this.previousRevision.get().resolveOperation(name) :
                Optional.empty();

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
