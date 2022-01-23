package gutta.apievolution.dsl;

import gutta.apievolution.core.apimodel.Annotation;
import gutta.apievolution.core.apimodel.QualifiedName;
import gutta.apievolution.core.apimodel.UserDefinedType;
import gutta.apievolution.core.apimodel.provider.*;
import gutta.apievolution.dsl.parser.ApiRevisionParser;
import org.antlr.v4.runtime.Token;

import java.util.Optional;
import java.util.Set;

class ProviderApiRevisionModelBuilderPass1 extends ApiRevisionModelBuilderPass1<ProviderApiDefinition,
        ProviderRecordType, ProviderField, ProviderEnumType, ProviderEnumMember, ProviderService,
        ProviderServiceOperation> implements ProviderApiRevisionModelBuilderPass {

    private int revision;

    /**
     * Builds a provider API definition from the given spec object.
     * @param revision The revision number to assign to the API definition
     * @param apiRevisionSpec The spec object to build the revision from
     * @param optionalPredecessor An optional predecessor to resolve the new definition against
     * @return The built and resolved definition
     */
    public ProviderApiDefinition buildProviderRevision(final int revision,
                                                       final ApiRevisionParser.ApiDefinitionContext apiRevisionSpec,
                                                       final Optional<ProviderApiDefinition> optionalPredecessor) {
        this.revision = revision;

        return this.buildApiDefinition(apiRevisionSpec, optionalPredecessor);
    }

    @Override
    protected ProviderApiDefinition createRevision(final ApiRevisionParser.ApiDefinitionContext context,
                                                   final QualifiedName name, final Set<Annotation> annotations,
                                                   final Optional<ProviderApiDefinition> predecessor) {
        return new ProviderApiDefinition(name, annotations, this.revision, predecessor);
    }

    @Override
    protected ProviderRecordType createRecordType(final ApiRevisionParser.RecordTypeContext context, final String name,
                                                  final Optional<String> internalName, final int typeId,
                                                  final ProviderApiDefinition currentRevision,
                                                  final boolean abstractFlag) {
        // Resolve predecessor, if applicable
        PredecessorType predecessorType = this.determinePredecessorType(context.replaces);
        Optional<ProviderRecordType> predecessor;
        switch (predecessorType) {
            case EXPLICIT:
                // Resolve an explicit predecessor
                String predecessorName = this.identifierAsText(context.replaces.itemName);
                predecessor = this.resolvePredecessorRecord(predecessorName, true,
                        context.replaces.refToken, context.replaces.itemName.start);
                break;

            case IMPLICIT:
                // Perform implicit predecessor resolution
                predecessor = this.resolvePredecessorRecord(name, false, context.refToken,
                        context.name.start);
                break;

            case NONE:
            default:
                predecessor = Optional.empty();
                break;
        }

        return new ProviderRecordType(name, internalName, typeId, currentRevision, abstractFlag, predecessor);
    }

    private Optional<ProviderRecordType> resolvePredecessorRecord(final String name, final boolean explicitReference,
                                                                  final Token refToken, final Token nameToken) {
        return this.resolvePredecessorUDT(name, ProviderRecordType.class, explicitReference, refToken, nameToken,
                "a record type");
    }

    @Override
    protected ProviderEnumType createEnumType(final ApiRevisionParser.EnumTypeContext context, final String name,
                                              final Optional<String> internalName, final int typeId,
                                              final ProviderApiDefinition owner) {
        // Resolve predecessor, if applicable
        PredecessorType predecessorType = this.determinePredecessorType(context.replaces);
        Optional<ProviderEnumType> predecessor;
        switch (predecessorType) {
            case EXPLICIT:
                // Resolve an explicit predecessor
                String predecessorName = this.identifierAsText(context.replaces.itemName);
                predecessor = this.resolvePredecessorEnum(predecessorName, true,
                        context.replaces.refToken, context.replaces.itemName.start);
                break;

            case IMPLICIT:
                // Perform implicit predecessor resolution
                predecessor = this.resolvePredecessorEnum(name, false, context.refToken,
                        context.name.start);
                break;

            case NONE:
            default:
                predecessor = Optional.empty();
                break;
        }

        return new ProviderEnumType(name, internalName, typeId, owner, predecessor);
    }

    private Optional<ProviderEnumType> resolvePredecessorEnum(final String name, final boolean explicitReference,
                                                              final Token refToken, final Token nameToken) {
        return this.resolvePredecessorUDT(name, ProviderEnumType.class, explicitReference, refToken, nameToken,
                "an enum type");
    }

    @SuppressWarnings("unchecked")
    private <U extends UserDefinedType<?>> Optional<U> resolvePredecessorUDT(final String name,
                                                                             final Class<U> udtClass,
                                                                             final boolean explicitReference,
                                                                             final Token refToken,
                                                                             final Token nameToken,
                                                                             final String typeErrorMessage) {
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

        Optional<UserDefinedType<ProviderApiDefinition>> optionalPredecessor =
                predecessorRevision.resolveUserDefinedType(name);

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
            throw new APIResolutionException(nameToken, "Predecessor element '" + name + "' exists, but is not " +
                    typeErrorMessage + ".");
        }
    }

}
