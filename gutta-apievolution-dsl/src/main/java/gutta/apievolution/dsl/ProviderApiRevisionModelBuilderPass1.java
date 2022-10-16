package gutta.apievolution.dsl;

import gutta.apievolution.core.apimodel.Abstract;
import gutta.apievolution.core.apimodel.Annotation;
import gutta.apievolution.core.apimodel.QualifiedName;
import gutta.apievolution.core.apimodel.RecordKind;
import gutta.apievolution.core.apimodel.UserDefinedType;
import gutta.apievolution.core.apimodel.provider.ProviderApiDefinition;
import gutta.apievolution.core.apimodel.provider.ProviderEnumMember;
import gutta.apievolution.core.apimodel.provider.ProviderEnumType;
import gutta.apievolution.core.apimodel.provider.ProviderField;
import gutta.apievolution.core.apimodel.provider.ProviderOperation;
import gutta.apievolution.core.apimodel.provider.ProviderRecordType;
import gutta.apievolution.dsl.parser.ApiRevisionParser;
import org.antlr.v4.runtime.Token;

import java.util.Optional;
import java.util.Set;

import static gutta.apievolution.core.apimodel.Conventions.noPredecessor;
import static gutta.apievolution.core.apimodel.Conventions.noSuperTypes;

class ProviderApiRevisionModelBuilderPass1
        extends
        ApiRevisionModelBuilderPass1<ProviderApiDefinition, ProviderRecordType, ProviderField, ProviderEnumType,
                ProviderEnumMember, ProviderOperation>
        implements ProviderApiRevisionModelBuilderPass {

    private int revision;

    private boolean ignoreReplacements;

    /**
     * Builds a provider API definition from the given spec object.
     *
     * @param revision            The revision number to assign to the API
     *                            definition
     * @param apiRevisionSpec     The spec object to build the revision from
     * @param ignoreReplacements  Flag whether to ignore replacement clauses
     * @param optionalPredecessor An optional predecessor to resolve the new
     *                            definition against
     * @return The built and resolved definition
     */
    public ProviderApiDefinition buildProviderRevision(final int revision,
            final ApiRevisionParser.ApiDefinitionContext apiRevisionSpec, boolean ignoreReplacements,
            final Optional<ProviderApiDefinition> optionalPredecessor) {
        this.revision = revision;
        this.ignoreReplacements = ignoreReplacements;

        return this.buildApiDefinition(apiRevisionSpec, optionalPredecessor);
    }

    @Override
    protected ProviderApiDefinition createRevision(final ApiRevisionParser.ApiDefinitionContext context,
            final QualifiedName name, final Set<Annotation> annotations,
            final ProviderApiDefinition predecessor) {
        return new ProviderApiDefinition(name, annotations, this.revision, predecessor);
    }

    @Override
    protected ProviderRecordType createRecordType(final ApiRevisionParser.RecordTypeContext context, final String name,
            final String internalName, final int typeId, final ProviderApiDefinition currentRevision,
            final Abstract abstractFlag, RecordKind recordKind) {
        // Resolve predecessor, if applicable
        PredecessorType predecessorType = this.determinePredecessorType(context.replaces);
        Optional<ProviderRecordType> predecessor;
        switch (predecessorType) {
        case EXPLICIT:
            // Resolve an explicit predecessor
            String predecessorName = this.identifierAsText(context.replaces.itemName);
            predecessor = this.resolvePredecessorRecord(predecessorName, true, context.replaces.refToken,
                    context.replaces.itemName.start);
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

        predecessor.ifPresent(record -> {
            if (record.getRecordKind() != recordKind) {
                throw new APIResolutionException(context.refToken,
                        "The predecessor of an exception may not be a record (or vice versa).");
            }
        });
        
        return currentRevision.newRecordOrExceptionType(name, internalName, typeId, abstractFlag, recordKind,
                noSuperTypes(), predecessor.orElse(noPredecessor()));
    }

    private Optional<ProviderRecordType> resolvePredecessorRecord(final String name, final boolean explicitReference,
            final Token refToken, final Token nameToken) {
        return this.resolvePredecessorUDT(name, ProviderRecordType.class, explicitReference, refToken, nameToken,
                "a record type");
    }

    @Override
    protected ProviderEnumType createEnumType(final ApiRevisionParser.EnumTypeContext context, final String name,
            final String internalName, final int typeId, final ProviderApiDefinition owner) {
        // Resolve predecessor, if applicable
        PredecessorType predecessorType = this.determinePredecessorType(context.replaces);
        Optional<ProviderEnumType> predecessor;
        switch (predecessorType) {
        case EXPLICIT:
            // Resolve an explicit predecessor
            String predecessorName = this.identifierAsText(context.replaces.itemName);
            predecessor = this.resolvePredecessorEnum(predecessorName, true, context.replaces.refToken,
                    context.replaces.itemName.start);
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

        return owner.newEnumType(name, internalName, typeId, predecessor.orElse(noPredecessor()));
    }

    private Optional<ProviderEnumType> resolvePredecessorEnum(final String name, final boolean explicitReference,
            final Token refToken, final Token nameToken) {
        return this.resolvePredecessorUDT(name, ProviderEnumType.class, explicitReference, refToken, nameToken,
                "an enum type");
    }

    @SuppressWarnings("unchecked")
    private <U extends UserDefinedType<?>> Optional<U> resolvePredecessorUDT(final String name, final Class<U> udtClass,
            final boolean explicitReference, final Token refToken, final Token nameToken,
            final String typeErrorMessage) {
        // If replacements are ignored, just return "no predecessor"
        if (this.ignoreReplacements) {
            return Optional.empty();
        }

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

        Optional<UserDefinedType<ProviderApiDefinition>> optionalPredecessor = predecessorRevision
                .resolveUserDefinedType(name);

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
            throw new APIResolutionException(nameToken,
                    "Predecessor element '" + name + "' exists, but is not " + typeErrorMessage + ".");
        }
    }

}
