package gutta.apievolution.core.apimodel.provider;

import gutta.apievolution.core.apimodel.*;

import java.util.*;
import java.util.function.Consumer;
import java.util.function.UnaryOperator;

/**
 * The model merger builds a merged definition from a history of definitions. This merged definition is the basis for
 * internal provider representations, as it is essentially the union of the definition history.
 */
public class ModelMerger {

    /**
     * Creates a merged API definition from a given revision history.
     * @param revisionHistory A non-empty revision history to merge
     * @return A merged API definition that represents the union of the definition history
     */
    public ProviderApiDefinition createMergedDefinition(RevisionHistory revisionHistory) {
        ProviderApiDefinition mergedDefinition = this.createEmptyMergedDefinition(revisionHistory);
        this.mergeElementsIntoRevision(revisionHistory, mergedDefinition, RevisionMergePass2::new);

        mergedDefinition.finalizeDefinition();

        return mergedDefinition;
    }

    /**
     * Creates a merged API definition from a given revision history and provides a mapping of the elements of the
     * given revision (which needs to be part of the revision history) to the elements of the merged revision.
     * @param revisionHistory A non-empty revision history to merge
     * @param referenceRevision A specific revision for whose elements a mapping is created
     * @return A map from the reference revision to the merged definition
     */
    public ToMergedModelMap createMergedDefinition(RevisionHistory revisionHistory,
                                                   ProviderApiDefinition referenceRevision) {
        ProviderApiDefinition mergedDefinition = this.createEmptyMergedDefinition(revisionHistory);
        MergePass2Creator pass2Creator = (supportedRevisions, typeLookup) ->
                new RevisionMergePass2.MappingRevisionMergePass2(supportedRevisions, typeLookup,
                        referenceRevision);
        RevisionMergePass2.MappingRevisionMergePass2 pass2 = (RevisionMergePass2.MappingRevisionMergePass2)
                this.mergeElementsIntoRevision(revisionHistory, mergedDefinition, pass2Creator);

        return pass2.getToMergedModelMap();
    }

    private ProviderApiDefinition createEmptyMergedDefinition(RevisionHistory revisionHistory) {
        if (revisionHistory == null || revisionHistory.isEmpty()) {
            throw new ModelMergeException("No or empty revision history given.");
        }

        QualifiedName mergedDefinitionName = this.determineHistoryName(revisionHistory);
        Set<Annotation> mergedAnnotations = this.mergeAnnotations(revisionHistory);

        return new ProviderApiDefinition(mergedDefinitionName,
                mergedAnnotations,
                0,
                Optional.empty());
    }

    private QualifiedName determineHistoryName(RevisionHistory revisionHistory) {
        ListIterator<ProviderApiDefinition> revisions = revisionHistory.reverseIterator();
        QualifiedName nameCandidate = revisions.previous().getName();

        while (revisions.hasPrevious()) {
            QualifiedName currentName = revisions.previous().getName();
            if (!nameCandidate.equals(currentName)) {
                throw new ModelMergeException("Inconsistent names in revision history.");
            }
        }

        return nameCandidate;
    }

    private Set<Annotation> mergeAnnotations(RevisionHistory revisionHistory) {
        Set<String> existingAnnotationTypes = new HashSet<>();
        Set<Annotation> mergedAnnotations = new HashSet<>();

        ListIterator<ProviderApiDefinition> revisions = revisionHistory.reverseIterator();
        while (revisions.hasPrevious()) {
            ProviderApiDefinition currentRevision = revisions.previous();

            for (Annotation annotation : currentRevision.getAnnotations()) {
                String annotationName = annotation.getName();

                if (!existingAnnotationTypes.contains(annotationName)) {
                    mergedAnnotations.add(annotation);
                    existingAnnotationTypes.add(annotationName);
                }
            }
        }

        return mergedAnnotations;
    }

    private RevisionMergePass2 mergeElementsIntoRevision(RevisionHistory revisionHistory,
                                           ProviderApiDefinition mergedDefinition,
                                           MergePass2Creator pass2Creator) {
        ListIterator<ProviderApiDefinition> revisions = revisionHistory.reverseIterator();
        Set<ProviderApiDefinition> supportedRevisions = revisionHistory.revisionSet();

        // First, merge the user-defined types and create a type lookup for the second pass
        RevisionMergePass1 pass1 = new RevisionMergePass1();
        ProviderTypeLookup typeLookup = pass1.createTypeLookup(revisionHistory, supportedRevisions, mergedDefinition);

        // Then, convert the remaining elements using the previously created type lookup
        RevisionMergePass2 pass2 = pass2Creator.create(supportedRevisions, typeLookup);

        while (revisions.hasPrevious()) {
            ProviderApiDefinition currentRevision = revisions.previous();
            pass2.mergeRevision(currentRevision);
        }

        return pass2;
    }

    /**
     * This class embodies the first pass of the revision merge process, in which only the user-defined types
     * are collected and mapped to their respective counterparts. Members of the types are only processed in
     * the second pass, as especially fields may require access to all mapped types.
     */
    private static class RevisionMergePass1 implements  ProviderApiDefinitionElementVisitor<Void> {

        private final Map<ProviderUserDefinedType, ProviderUserDefinedType> udtLookup = new HashMap<>();

        private final Set<String> knownTypeNames = new HashSet<>();

        private ProviderApiDefinition mergedDefinition;

        public ProviderTypeLookup createTypeLookup(RevisionHistory revisionHistory,
                                           Set<ProviderApiDefinition> supportedRevisions,
                                           ProviderApiDefinition mergedDefinition) {
            // Ensure that the revision history is consistent
            revisionHistory.checkConsistency();

            this.mergedDefinition = mergedDefinition;

            ListIterator<ProviderApiDefinition> revisions = revisionHistory.reverseIterator();
            while (revisions.hasPrevious()) {
                ProviderApiDefinition currentRevision = revisions.previous();
                currentRevision.forEach(element -> element.accept(this));
            }

            return new ProviderTypeLookup(this.udtLookup);
        }

        @SuppressWarnings("unchecked")
        private <T extends UserDefinedType<ProviderApiDefinition> & RevisionedElement<T> & ProviderUserDefinedType>
            Void handleUserDefinedType(T inType, UnaryOperator<T> mapperFunction) {

            // Check if a successor of this type is already part of the merged model
            Optional<T> optionalMappedSuccessor = inType.findFirstSuccessorMatching(
                    this.udtLookup::containsKey
            );

            T mappedType;
            if (optionalMappedSuccessor.isPresent()) {
                mappedType = (T) this.udtLookup.get(optionalMappedSuccessor.get());
            } else {
                // Check for duplicate internal names
                this.assertUniqueInternalName(inType);
                mappedType = mapperFunction.apply(inType);
            }

            this.udtLookup.put(inType, mappedType);
            return null;
        }

        @Override
        public Void handleProviderRecordType(ProviderRecordType recordType) {
            return this.handleUserDefinedType(recordType, this::convertRecordType);
        }

        private Optional<String> determineInternalName(UserDefinedType<ProviderApiDefinition> inType) {
            return (inType.getPublicName().equals(inType.getInternalName())) ? Optional.empty() :
                    Optional.of(inType.getInternalName());
        }

        private void assertUniqueInternalName(UserDefinedType<ProviderApiDefinition> type) {
            if (this.knownTypeNames.contains(type.getInternalName())) {
                throw new ModelMergeException("Duplicate internal name '" + type.getInternalName() + "'.");
            }

            this.knownTypeNames.add(type.getInternalName());
        }

        private ProviderRecordType convertRecordType(ProviderRecordType inType) {
            return new ProviderRecordType(inType.getPublicName(),
                    this.determineInternalName(inType),
                    inType.getTypeId(),
                    this.mergedDefinition,
                    inType.isAbstract(),
                    Optional.empty());
        }

        @Override
        public Void handleProviderEnumType(ProviderEnumType enumType) {
            return this.handleUserDefinedType(enumType, this::convertEnumType);
        }

        private ProviderEnumType convertEnumType(ProviderEnumType inType) {
            return new ProviderEnumType(inType.getPublicName(),
                    this.determineInternalName(inType),
                    inType.getTypeId(),
                    this.mergedDefinition,
                    Optional.empty());
        }

    }

    @FunctionalInterface
    private interface MergePass2Creator {

        RevisionMergePass2 create(Set<ProviderApiDefinition> supportedRevisions, ProviderTypeLookup typeLookup);

    }

    /**
     * This class embodies the second pass of the revision merge process. In this pass, fields and enum members as well
     * as services and service operations are merged. These elements require access to the merged types, which are
     * passed on from the first pass.
     */
    private static class RevisionMergePass2 implements ProviderApiDefinitionElementVisitor<Void> {

        private final Set<ProviderApiDefinition> supportedRevisions;

        protected final ProviderTypeLookup typeLookup;

        private final Set<MemberName> knownMemberNames = new HashSet<>();

        private final Map<ProviderField, ProviderField> mappedFields = new HashMap<>();

        private final Map<ProviderEnumMember, ProviderEnumMember> mappedMembers = new HashMap<>();

        private ProviderRecordType currentRecordType;

        private ProviderEnumType currentEnumType;

        public RevisionMergePass2(Set<ProviderApiDefinition> supportedRevisions,
                                  ProviderTypeLookup typeLookup) {
            this.supportedRevisions = supportedRevisions;
            this.typeLookup = typeLookup;
        }

        public void mergeRevision(ProviderApiDefinition definition) {
            definition.forEach(element -> element.accept(this));
        }

        protected <T extends Type> T lookupType(T inType) {
            return this.typeLookup.lookupType(inType);
        }

        @Override
        public Void handleProviderRecordType(ProviderRecordType recordType) {
            this.currentRecordType = this.typeLookup.lookupType(recordType);

            // Set supertype, if applicable
            recordType.getSuperType().ifPresent(originalSuperType -> {
                ProviderRecordType superType = this.typeLookup.lookupType(originalSuperType);

                // If the current record already has a supertype, make sure that it is the
                // same as the one we would add
                if (this.currentRecordType.getSuperType().isPresent()) {
                    if (!this.currentRecordType.getSuperType().get().equals(superType)) {
                        throw new ModelMergeException("Inconsistent supertype for " + this.currentRecordType + ".");
                    }
                } else {
                    this.currentRecordType.setSuperType(superType);
                }
            });

            for (ProviderField field : recordType.getDeclaredFields()) {
                field.accept(this);
            }

            this.currentRecordType = null;
            return null;
        }

        @Override
        public Void handleProviderEnumType(ProviderEnumType enumType) {
            this.currentEnumType = this.typeLookup.lookupType(enumType);

            for (ProviderEnumMember member : enumType.getDeclaredMembers()) {
                member.accept(this);
            }

            this.currentEnumType = null;
            return null;
        }

        private void assertUniqueMemberName(MemberName memberName) {
            if (this.knownMemberNames.contains(memberName)) {
                throw new ModelMergeException("Duplicate internal name '" + memberName + "'.");
            }

            this.knownMemberNames.add(memberName);
        }

        @Override
        public Void handleProviderField(ProviderField field) {
            Optional<ProviderField> optionalMappedSuccessor = field.findFirstSuccessorMatching(
                    this.mappedFields::containsKey
            );

            boolean typeChange = false;
            if (optionalMappedSuccessor.isPresent()) {
                Type currentFieldType = field.getType();
                Type successorType = optionalMappedSuccessor.get().getType();

                typeChange = ProviderField.isTypeChange(currentFieldType, successorType);
            }

            ProviderField mappedField;
            if (!optionalMappedSuccessor.isPresent() || typeChange) {
                // If no predecessor exists or a type change has occurred, create a new field
                Optional<String> internalName = (field.getPublicName().equals(field.getInternalName())) ?
                        Optional.empty() : Optional.of(field.getInternalName());
                Optionality optionality = this.determineOptionalityForField(field);
                Type type = this.lookupType(field.getType());

                // Ensure that the internal name is unique
                MemberName memberName = new MemberName(field.getOwner().getInternalName(), field.getInternalName());
                this.assertUniqueMemberName(memberName);

                mappedField = new ProviderField(field.getPublicName(),
                        internalName,
                        this.currentRecordType,
                        type,
                        optionality,
                        field.isInherited(),
                        Collections.emptyList(),
                        Optional.empty());

                this.knownMemberNames.add(memberName);
                this.mappedFields.put(field, mappedField);
            } else {
                mappedField = this.mappedFields.get(optionalMappedSuccessor.get());
            }

            this.registerFieldMapping(field, mappedField);

            return null;
        }

        protected void registerFieldMapping(ProviderField originalField, ProviderField mappedField) {
            // Do nothing as of now
        }

        private Optionality determineOptionalityForField(ProviderField field) {
            // First, determine the minimal specified optionality. Note that the field is always from the latest
            // revision in which it exists, as we iterate backwards through the revision history. Therefore, we
            // only have to take predecessors into account
            MaxOptionalityCollector maxOptionalityCollector = new MaxOptionalityCollector();
            field.predecessorStream(true)
                    .forEach(maxOptionalityCollector);
            Optionality specifiedOptionality = maxOptionalityCollector.getMaxOptionality();

            // Then, check whether the field exists in all supported revisions
            Set<ProviderApiDefinition> defsInWhichFieldExists = new HashSet<>();
            field.predecessorStream(true)
                    .forEach(fld -> defsInWhichFieldExists.add(fld.getOwner().getOwner()));
            boolean existsInAllRevisions = defsInWhichFieldExists.containsAll(this.supportedRevisions);

            Optionality minimalOptionality;
            if (existsInAllRevisions) {
                // If the field exists in all revisions, it can be mandatory
                minimalOptionality = Optionality.MANDATORY;
            } else {
                // If the field does not exist in all revisions, it must be optional in some way. If it is used as
                // output, it must be fully optional, otherwise, it may be opt-in.
                Usage fieldUsage = field.getOwner().getUsage();
                minimalOptionality = (fieldUsage.includes(Usage.OUTPUT)) ? Optionality.OPTIONAL : Optionality.OPT_IN;
            }

            return Optionality.max(specifiedOptionality, minimalOptionality);
        }

        @Override
        public Void handleProviderEnumMember(ProviderEnumMember enumMember) {
            Optional<ProviderEnumMember> optionalMappedPredecessor = enumMember.findFirstSuccessorMatching(
                    this.mappedMembers::containsKey
            );

            ProviderEnumMember mappedMember;
            if (!optionalMappedPredecessor.isPresent()) {
                Optional<String> internalName = (enumMember.getPublicName().equals(enumMember.getInternalName())) ?
                        Optional.empty() : Optional.of(enumMember.getInternalName());

                // Ensure that the internal name is unique
                MemberName memberName = new MemberName(enumMember.getOwner().getInternalName(),
                        enumMember.getInternalName());
                this.assertUniqueMemberName(memberName);

                mappedMember = new ProviderEnumMember(enumMember.getPublicName(),
                        internalName,
                        this.currentEnumType,
                        Optional.empty());

                this.mappedMembers.put(enumMember, mappedMember);
            } else {
                mappedMember = this.mappedMembers.get(optionalMappedPredecessor.get());
            }

            this.registerEnumMemberMapping(enumMember, mappedMember);

            return null;
        }

        protected void registerEnumMemberMapping(ProviderEnumMember originalMember, ProviderEnumMember mappedMember) {
            // Do nothing as of now
        }

        private static class MappingRevisionMergePass2 extends RevisionMergePass2 {

            private final ProviderApiDefinition referenceRevision;

            private final Map<ProviderField, ProviderField> fieldMap = new HashMap<>();

            private final Map<ProviderEnumMember, ProviderEnumMember> enumMemberMap = new HashMap<>();

            public MappingRevisionMergePass2(Set<ProviderApiDefinition> supportedRevisions,
                                             ProviderTypeLookup typeLookup,
                                             ProviderApiDefinition referenceRevision) {
                super(supportedRevisions, typeLookup);

                this.referenceRevision = referenceRevision;
            }

            @Override
            protected void registerFieldMapping(ProviderField originalField, ProviderField mappedField) {
                if (originalField.getOwner().getOwner().equals(this.referenceRevision)) {
                    this.fieldMap.put(originalField, mappedField);
                }
            }

            @Override
            protected void registerEnumMemberMapping(ProviderEnumMember originalMember,
                                                     ProviderEnumMember mappedMember) {
                if (originalMember.getOwner().getOwner().equals(this.referenceRevision)) {
                    this.enumMemberMap.put(originalMember, mappedMember);
                }
            }

            public ToMergedModelMap getToMergedModelMap() {
                ProviderTypeLookup restrictedTypeLookup = this.typeLookup.restrictTo(this.referenceRevision);
                return new ToMergedModelMap(restrictedTypeLookup, this.fieldMap, this.enumMemberMap);
            }

        }

        /**
         * Representation of a type-qualified member name for uniqueness tests.
         */
        private static class MemberName {

            public final String typeName;

            public final String memberName; // NOSONAR This name is appropriate

            public MemberName(String typeName, String memberName) {
                this.typeName = typeName;
                this.memberName = memberName;
            }

            @Override
            public int hashCode() {
                return this.typeName.hashCode() + this.memberName.hashCode();
            }

            @Override
            public boolean equals(Object that) {
                if (this == that) {
                    return true;
                } else if (that instanceof MemberName) {
                    return this.stateEquals((MemberName) that);
                } else {
                    return false;
                }
            }

            private boolean stateEquals(MemberName that) {
                return this.typeName.equals(that.typeName) &&
                        this.memberName.equals(that.memberName);
            }

            @Override
            public String toString() {
                return this.typeName + "." + this.memberName;
            }
        }

        /**
         * Collector operation to determine the most permissive optionality from a revision history of provider
         * fields.
         */
        private static class MaxOptionalityCollector implements Consumer<ProviderField> {

            private Optionality maxOptionality = Optionality.MANDATORY;

            @Override
            public void accept(ProviderField providerField) {
                Optionality fieldOptionality = providerField.getOptionality();

                this.maxOptionality = Optionality.max(this.maxOptionality, fieldOptionality);
            }

            public Optionality getMaxOptionality() {
                return this.maxOptionality;
            }

        }

    }

}
