package gutta.apievolution.core.apimodel.provider;

import gutta.apievolution.core.apimodel.*;

import java.util.*;
import java.util.function.Consumer;
import java.util.function.UnaryOperator;

import static gutta.apievolution.core.apimodel.Conventions.*;
import static gutta.apievolution.core.apimodel.provider.ProviderTypeTools.isTypeChange;

/**
 * The model merger builds a merged definition from a history of definitions.
 * This merged definition is the basis for internal provider representations, as
 * it is essentially the union of the definition history.
 */
public class ModelMerger {

    /**
     * Creates a merged API definition from a given revision history.
     *
     * @param revisionHistory A non-empty revision history to merge
     * @return A merged API definition that represents the union of the definition
     *         history
     */
    public ProviderApiDefinition createMergedDefinition(RevisionHistory revisionHistory) {
        ProviderApiDefinition mergedDefinition = this.createEmptyMergedDefinition(revisionHistory);
        this.mergeElementsIntoRevision(revisionHistory, mergedDefinition, RevisionMergePass2::new);

        mergedDefinition.finalizeDefinition();

        return mergedDefinition;
    }

    /**
     * Creates a merged API definition from a given revision history and provides a
     * mapping of the elements of the given revision (which needs to be part of the
     * revision history) to the elements of the merged revision.
     *
     * @param revisionHistory   A non-empty revision history to merge
     * @param referenceRevision A specific revision for whose elements a mapping is
     *                          created
     * @return A map from the reference revision to the merged definition
     */
    public ToMergedModelMap createMergedDefinition(RevisionHistory revisionHistory,
            ProviderApiDefinition referenceRevision) {
        ProviderApiDefinition mergedDefinition = this.createEmptyMergedDefinition(revisionHistory);
        MergePass2Creator pass2Creator = (supportedRevisions, typeLookup, mergedDef) -> new MappingRevisionMergePass2(
                supportedRevisions, typeLookup, mergedDef, referenceRevision);
        MappingRevisionMergePass2 pass2 = (MappingRevisionMergePass2) this.mergeElementsIntoRevision(revisionHistory,
                mergedDefinition, pass2Creator);

        return pass2.getToMergedModelMap();
    }

    private ProviderApiDefinition createEmptyMergedDefinition(RevisionHistory revisionHistory) {
        if (revisionHistory == null || revisionHistory.isEmpty()) {
            throw new ModelMergeException("No or empty revision history given.");
        }

        String mergedDefinitionName = this.determineHistoryName(revisionHistory);
        Set<Annotation> mergedAnnotations = this.mergeAnnotations(revisionHistory);

        return new ProviderApiDefinition(mergedDefinitionName, mergedAnnotations, 0, null);
    }

    private String determineHistoryName(RevisionHistory revisionHistory) {
        ListIterator<ProviderApiDefinition> revisions = revisionHistory.reverseIterator();
        QualifiedName nameCandidate = revisions.previous().getName();

        while (revisions.hasPrevious()) {
            QualifiedName currentName = revisions.previous().getName();
            if (!nameCandidate.equals(currentName)) {
                throw new ModelMergeException("Inconsistent names in revision history.");
            }
        }

        return nameCandidate.toString();
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
            ProviderApiDefinition mergedDefinition, MergePass2Creator pass2Creator) {
        ListIterator<ProviderApiDefinition> revisions = revisionHistory.reverseIterator();
        Set<ProviderApiDefinition> supportedRevisions = revisionHistory.revisionSet();

        // First, merge the user-defined types and create a type lookup for the second
        // pass
        RevisionMergePass1 pass1 = new RevisionMergePass1();
        TypeMap<ProviderUserDefinedType, ProviderUserDefinedType> typeLookup = 
                pass1.createTypeLookup(revisionHistory, supportedRevisions, mergedDefinition);

        // Then, convert the remaining elements using the previously created type lookup
        RevisionMergePass2 pass2 = pass2Creator.create(supportedRevisions, typeLookup, mergedDefinition);

        while (revisions.hasPrevious()) {
            ProviderApiDefinition currentRevision = revisions.previous();
            pass2.mergeRevision(currentRevision);
        }

        return pass2;
    }

    /**
     * This class embodies the first pass of the revision merge process, in which
     * only the user-defined types are collected and mapped to their respective
     * counterparts. Members of the types are only processed in the second pass, as
     * especially fields may require access to all mapped types.
     */
    private static class RevisionMergePass1 implements ProviderApiDefinitionElementVisitor<Void> {

        // We use an identity hash map so that we can distinguish otherwise equal types in different revisions
        private final Map<ProviderUserDefinedType, ProviderUserDefinedType> udtLookup = new IdentityHashMap<>();

        private final Set<String> knownTypeNames = new HashSet<>();

        private ProviderApiDefinition mergedDefinition;

        public TypeMap<ProviderUserDefinedType, ProviderUserDefinedType> 
                createTypeLookup(RevisionHistory revisionHistory,
                Set<ProviderApiDefinition> supportedRevisions, ProviderApiDefinition mergedDefinition) {
            // Ensure that the revision history is consistent
            revisionHistory.checkConsistency();

            this.mergedDefinition = mergedDefinition;

            ListIterator<ProviderApiDefinition> revisions = revisionHistory.reverseIterator();
            while (revisions.hasPrevious()) {
                ProviderApiDefinition currentRevision = revisions.previous();
                currentRevision.forEach(element -> element.accept(this));
            }

            return new TypeMap<ProviderUserDefinedType, ProviderUserDefinedType>(this.udtLookup);
        }

        @SuppressWarnings("unchecked")
        private <T extends UserDefinedType<ProviderApiDefinition> & RevisionedElement<T> & ProviderUserDefinedType> 
            Void handleUserDefinedType(T inType, UnaryOperator<T> mapperFunction) {

            // Check if a successor of this type is already part of the merged model
            Optional<T> optionalMappedSuccessor = inType.findFirstSuccessorMatching(this.udtLookup::containsKey);

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

        private void assertUniqueInternalName(UserDefinedType<ProviderApiDefinition> type) {
            if (this.knownTypeNames.contains(type.getInternalName())) {
                throw new ModelMergeException("Duplicate internal name '" + type.getInternalName() + "'.");
            }

            this.knownTypeNames.add(type.getInternalName());
        }

        private ProviderRecordType convertRecordType(ProviderRecordType inType) {
            Abstract abstractness = (inType.isAbstract()) ? Abstract.YES : Abstract.NO;
            
            if (inType.isException()) {
                return this.mergedDefinition.newExceptionType(inType.getPublicName(), inType.getInternalName(),
                        inType.getTypeId(), abstractness, noSuperTypes(), noPredecessor());
            } else {
                return this.mergedDefinition.newRecordType(inType.getPublicName(), inType.getInternalName(),
                        inType.getTypeId(), abstractness, noSuperTypes(), noPredecessor());
            }
        }

        @Override
        public Void handleProviderEnumType(ProviderEnumType enumType) {
            return this.handleUserDefinedType(enumType, this::convertEnumType);
        }

        private ProviderEnumType convertEnumType(ProviderEnumType inType) {
            return this.mergedDefinition.newEnumType(inType.getPublicName(), inType.getInternalName(), inType.getTypeId(), null);
        }

    }

    @FunctionalInterface
    private interface MergePass2Creator {

        RevisionMergePass2 create(Set<ProviderApiDefinition> supportedRevisions, 
                TypeMap<ProviderUserDefinedType, ProviderUserDefinedType> typeMap,
                ProviderApiDefinition mergedDefinition);

    }

    /**
     * This class embodies the second pass of the revision merge process. In this
     * pass, fields and enum members as well as services and service operations are
     * merged. These elements require access to the merged types, which are passed
     * on from the first pass.
     */
    private static class RevisionMergePass2 implements ProviderApiDefinitionElementVisitor<Void> {

        private final Set<ProviderApiDefinition> supportedRevisions;

        protected final TypeMap<ProviderUserDefinedType, ProviderUserDefinedType> typeMap;

        private final ProviderApiDefinition mergedDefinition;

        private final Set<MemberName> knownMemberNames = new HashSet<>();

        private final Map<ProviderField, ProviderField> mappedFields = new HashMap<>();

        private final Map<ProviderEnumMember, ProviderEnumMember> mappedMembers = new HashMap<>();

        private final Map<ProviderOperation, ProviderOperation> mappedOperations = new HashMap<>();

        private ProviderRecordType currentRecordType;

        private ProviderEnumType currentEnumType;

        public RevisionMergePass2(Set<ProviderApiDefinition> supportedRevisions,
                TypeMap<ProviderUserDefinedType, ProviderUserDefinedType> typeMap,
                ProviderApiDefinition mergedDefinition) {
            this.supportedRevisions = supportedRevisions;
            this.typeMap = typeMap;
            this.mergedDefinition = mergedDefinition;
        }

        public void mergeRevision(ProviderApiDefinition definition) {
            definition.forEach(element -> element.accept(this));
        }

        protected <T extends Type> T lookupType(T inType) {
            return this.typeMap.mapType(inType);
        }

        @Override
        public Void handleProviderRecordType(ProviderRecordType recordType) {
            this.currentRecordType = this.typeMap.mapType(recordType);

            // Set supertypes, if applicable
            recordType.getSuperTypes().forEach(originalSuperType -> {
                ProviderRecordType superType = this.typeMap.mapType(originalSuperType);

                this.currentRecordType.addSuperType(superType);
            });

            recordType.getDeclaredFields().forEach(field -> field.accept(this));

            this.currentRecordType = null;
            return null;
        }

        @Override
        public Void handleProviderEnumType(ProviderEnumType enumType) {
            this.currentEnumType = this.typeMap.mapType(enumType);

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
            Optional<ProviderField> optionalMappedSuccessor = field
                    .findFirstSuccessorMatching(this.mappedFields::containsKey);

            boolean typeChange = false;
            if (optionalMappedSuccessor.isPresent()) {
                Type currentFieldType = field.getType();
                Type successorType = optionalMappedSuccessor.get().getType();

                typeChange = isTypeChange(currentFieldType, successorType);
            }

            ProviderField mappedField;
            if (!optionalMappedSuccessor.isPresent() || typeChange) {
                // If no predecessor exists or a type change has occurred, create a new field
                mappedField = this.reuseOrCreateField(field);
            } else {
                mappedField = this.mappedFields.get(optionalMappedSuccessor.get());
            }

            this.registerFieldMapping(field, mappedField);

            return null;
        }

        private ProviderField reuseOrCreateField(ProviderField originalField) {
            Optionality optionality = this.determineOptionalityForField(originalField);
            Type type = this.lookupType(originalField.getType());

            // See if there is already a matching field in the merged type that can be
            // reused
            ProviderField potentialMatch = this.currentRecordType
                    .resolveFieldByInternalName(originalField.getInternalName()).orElse(null);
            if (potentialMatch != null && this.fieldMatches(originalField, potentialMatch, optionality, type)) {
                // If there is a matching type, reuse it
                return potentialMatch;
            }

            // Ensure that the internal name is unique
            MemberName memberName = new MemberName(originalField.getOwner().getInternalName(),
                    originalField.getInternalName());
            this.assertUniqueMemberName(memberName);

            
            Inherited inherited = (originalField.isInherited()) ? Inherited.YES : Inherited.NO;            
            ProviderField newField = this.currentRecordType.newField(originalField.getPublicName(),
                    originalField.getInternalName(), type, optionality, inherited, noDeclaredPredecessors(),
                    noPredecessor());
            
            this.knownMemberNames.add(memberName);
            this.mappedFields.put(originalField, newField);

            return newField;
        }

        private boolean fieldMatches(Field<?, ?> originalField, Field<?, ?> matchCandidate, Optionality optionality,
                Type type) {
            // See if the candidate actually matches with respect to the relevant properties
            return matchCandidate.getPublicName().equals(originalField.getPublicName()) &&
                    matchCandidate.getInternalName().equals(originalField.getInternalName()) &&
                    matchCandidate.getType().equals(type) && matchCandidate.getOptionality().equals(optionality) &&
                    matchCandidate.isInherited() == originalField.isInherited();
        }

        protected void registerFieldMapping(ProviderField originalField, ProviderField mappedField) {
            // Do nothing as of now
        }

        private Optionality determineOptionalityForField(ProviderField field) {
            // First, determine the minimal specified optionality. Note that the field is
            // always from the latest revision in which it exists, as we iterate backwards through the revision
            // history. Therefore, we only have to take predecessors into account
            MaxOptionalityCollector maxOptionalityCollector = new MaxOptionalityCollector();
            field.predecessorStream(true).forEach(maxOptionalityCollector);
            Optionality specifiedOptionality = maxOptionalityCollector.getMaxOptionality();

            // Then, check whether the field exists in all supported revisions
            Set<ProviderApiDefinition> defsInWhichFieldExists = new HashSet<>();
            field.predecessorStream(true).forEach(fld -> defsInWhichFieldExists.add(fld.getOwner().getOwner()));
            boolean existsInAllRevisions = defsInWhichFieldExists.containsAll(this.supportedRevisions);

            Optionality minimalOptionality;
            if (existsInAllRevisions) {
                // If the field exists in all revisions, it can be mandatory
                minimalOptionality = Optionality.MANDATORY;
            } else {
                // If the field does not exist in all revisions, it must be optional in some
                // way. If it is used as output, it must be fully optional, otherwise, it may be opt-in.
                Usage fieldUsage = field.getOwner().getUsage();
                minimalOptionality = (fieldUsage.includes(Usage.OUTPUT)) ? Optionality.OPTIONAL : Optionality.OPT_IN;
            }

            return Optionality.max(specifiedOptionality, minimalOptionality);
        }

        @Override
        public Void handleProviderEnumMember(ProviderEnumMember enumMember) {
            Optional<ProviderEnumMember> optionalMappedPredecessor = enumMember
                    .findFirstSuccessorMatching(this.mappedMembers::containsKey);

            ProviderEnumMember mappedMember;
            if (!optionalMappedPredecessor.isPresent()) {
                String internalName = enumMember.getInternalName();

                // Ensure that the internal name is unique
                MemberName memberName = new MemberName(enumMember.getOwner().getInternalName(),
                        enumMember.getInternalName());
                this.assertUniqueMemberName(memberName);

                mappedMember = this.currentEnumType.newEnumMember(enumMember.getPublicName(), internalName, noPredecessor());

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

        @Override
        public Void handleProviderOperation(ProviderOperation operation) {
            Optional<ProviderOperation> optionalMappedPredecessor = operation
                    .findFirstSuccessorMatching(this.mappedOperations::containsKey);

            boolean typeChange = false;
            if (optionalMappedPredecessor.isPresent()) {
                Type returnType = operation.getReturnType();
                Type successorReturnType = optionalMappedPredecessor.get().getReturnType();

                Type parameterType = operation.getParameterType();
                Type successorParameterType = optionalMappedPredecessor.get().getReturnType();

                typeChange = isTypeChange(returnType, successorReturnType) ||
                        isTypeChange(parameterType, successorParameterType);
            }

            ProviderOperation mappedOperation;
            if (!optionalMappedPredecessor.isPresent() || typeChange) {
                ProviderRecordType mappedReturnType = this.lookupType(operation.getReturnType());
                ProviderRecordType mappedParameterType = this.lookupType(operation.getParameterType());

                mappedOperation = new ProviderOperation(operation.getAnnotations(), operation.getPublicName(),
                        operation.getInternalName(), this.mergedDefinition, mappedReturnType,
                        mappedParameterType, null);

                // Add exceptions to the operation
                operation.getThrownExceptions()
                        .forEach(exception -> mappedOperation.addThrownException(this.lookupType(exception)));

                this.mappedOperations.put(operation, mappedOperation);
            } else {
                mappedOperation = this.mappedOperations.get(optionalMappedPredecessor.get());

                // Merge annotations; the newest annotation of a type is kept
                for (Annotation annotation : operation.getAnnotations()) {
                    Optional<Annotation> existingAnnotation = mappedOperation.getAnnotation(annotation.getName());
                    if (!existingAnnotation.isPresent()) {
                        mappedOperation.addAnnotation(annotation);
                    }
                }

                // Merge exceptions
                for (ProviderRecordType exceptionType : operation.getThrownExceptions()) {
                    ProviderRecordType mappedExceptionType = this.lookupType(exceptionType);
                    mappedOperation.addThrownException(mappedExceptionType);
                }
            }

            this.registerOperationMapping(operation, mappedOperation);

            return null;
        }

        protected void registerOperationMapping(ProviderOperation originalOperation,
                ProviderOperation mappedOperation) {
            // Do nothing as of now
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
                return this.typeName.equals(that.typeName) && this.memberName.equals(that.memberName);
            }

            @Override
            public String toString() {
                return this.typeName + "." + this.memberName;
            }
        }

        /**
         * Collector operation to determine the most permissive optionality from a
         * revision history of provider fields.
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

    private static class MappingRevisionMergePass2
            extends RevisionMergePass2 {

        private final ProviderApiDefinition referenceRevision;

        private final Map<ProviderField, ProviderField> fieldMap = new HashMap<>();

        private final Map<ProviderEnumMember, ProviderEnumMember> enumMemberMap = new HashMap<>();

        private final Map<ProviderOperation, ProviderOperation> operationMap = new HashMap<>();

        public MappingRevisionMergePass2(Set<ProviderApiDefinition> supportedRevisions, 
                TypeMap<ProviderUserDefinedType, ProviderUserDefinedType> typeMap,
                ProviderApiDefinition mergedDefinition, ProviderApiDefinition referenceRevision) {
            super(supportedRevisions, typeMap, mergedDefinition);

            this.referenceRevision = referenceRevision;
        }

        @Override
        protected void registerFieldMapping(ProviderField originalField, ProviderField mappedField) {
            if (originalField.getOwner().getOwner().equals(this.referenceRevision)) {
                this.fieldMap.put(originalField, mappedField);
            }
        }

        @Override
        protected void registerEnumMemberMapping(ProviderEnumMember originalMember, ProviderEnumMember mappedMember) {
            if (originalMember.getOwner().getOwner().equals(this.referenceRevision)) {
                this.enumMemberMap.put(originalMember, mappedMember);
            }
        }

        @Override
        protected void registerOperationMapping(ProviderOperation originalOperation,
                ProviderOperation mappedOperation) {
            if (originalOperation.getOwner().equals(this.referenceRevision)) {
                this.operationMap.put(originalOperation, mappedOperation);
            }
        }

        public ToMergedModelMap getToMergedModelMap() {
            TypeMap<ProviderUserDefinedType, ProviderUserDefinedType> restrictedTypeLookup = 
                    this.typeMap.restrictTo(this.referenceRevision);
            return new ToMergedModelMap(restrictedTypeLookup, this.fieldMap, this.enumMemberMap, this.operationMap);
        }

    }

}
