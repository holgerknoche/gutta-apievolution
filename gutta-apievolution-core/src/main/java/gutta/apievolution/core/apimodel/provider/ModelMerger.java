package gutta.apievolution.core.apimodel.provider;

import gutta.apievolution.core.apimodel.*;

import java.util.*;
import java.util.function.Consumer;
import java.util.function.Function;
import java.util.function.Predicate;

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
        if (revisionHistory == null || revisionHistory.isEmpty()) {
            throw new ModelMergeException("No or empty revision history given.");
        }

        QualifiedName mergedDefinitionName = this.determineHistoryName(revisionHistory);
        Set<Annotation> mergedAnnotations = this.mergeAnnotations(revisionHistory);

        ProviderApiDefinition mergedDefinition = new ProviderApiDefinition(mergedDefinitionName,
                mergedAnnotations,
                0,
                Optional.empty());

        this.mergeElementsIntoRevision(revisionHistory, mergedDefinition);

        return mergedDefinition;
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

    private void mergeElementsIntoRevision(RevisionHistory revisionHistory,
                                           ProviderApiDefinition mergedDefinition) {
        ListIterator<ProviderApiDefinition> revisions = revisionHistory.reverseIterator();
        Set<ProviderApiDefinition> supportedRevisions = revisionHistory.revisionSet();

        RevisionMergePass1 pass1 = new RevisionMergePass1();
        TypeLookup typeLookup = pass1.createTypeLookup(revisionHistory, supportedRevisions, mergedDefinition);

        // TODO
        RevisionMergePass2 merger = new RevisionMergePass2(mergedDefinition, supportedRevisions, typeLookup);

        while (revisions.hasPrevious()) {
            ProviderApiDefinition currentRevision = revisions.previous();
            merger.mergeRevision(currentRevision);
        }
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

        public TypeLookup createTypeLookup(RevisionHistory revisionHistory,
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

            return new TypeLookup(this.udtLookup);
        }

        @SuppressWarnings("unchecked")
        private <T extends UserDefinedType<ProviderApiDefinition> & RevisionedElement<T> & ProviderUserDefinedType>
            Void handleUserDefinedType(T inType, Function<T, T> mapperFunction) {

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
                    inType.getSuperType(),
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

    private static class TypeLookup {

        private final Map<ProviderUserDefinedType, ProviderUserDefinedType> udtLookup;

        public TypeLookup(Map<ProviderUserDefinedType, ProviderUserDefinedType> udtLookup) {
            this.udtLookup = udtLookup;
        }

        @SuppressWarnings("unchecked")
        public <T extends Type> T lookupType(T inType) {
            if (inType instanceof ProviderUserDefinedType) {
                return (T) this.udtLookup.get(inType);
            } else if (inType instanceof ListType) {
                return (T) this.convertListType((ListType) inType);
            } else {
                return inType;
            }
        }

        private ListType convertListType(ListType inType) {
            if (inType.isBounded()) {
                return ListType.bounded(inType.getElementType(), inType.getBound());
            } else {
                return ListType.unbounded(inType.getElementType());
            }
        }

    }

    /**
     * This class embodies the second pass of the revision merge process. In this pass, fields and enum members as well
     * as services and service operations are merged. These elements require access to the merged types, which are
     * passed on from the first pass.
     */
    private static class RevisionMergePass2 implements ProviderApiDefinitionElementVisitor<Void> {

        private final ProviderApiDefinition mergedDefinition;

        private final Set<ProviderApiDefinition> supportedRevisions;

        private final TypeLookup typeLookup;

        private final Set<MemberName> knownMemberNames = new HashSet<>();

        private final Set<ProviderField> mappedFields = new HashSet<>();

        private final Set<ProviderEnumMember> mappedMembers = new HashSet<>();

        private ProviderRecordType currentRecordType;

        private ProviderEnumType currentEnumType;

        public RevisionMergePass2(ProviderApiDefinition mergedDefinition, Set<ProviderApiDefinition> supportedRevisions,
                                  TypeLookup typeLookup) {
            this.mergedDefinition = mergedDefinition;
            this.supportedRevisions = supportedRevisions;
            this.typeLookup = typeLookup;
        }

        public void mergeRevision(ProviderApiDefinition definition) {
            definition.forEach(element -> element.accept(this));
        }

        private <T extends Type> T lookupType(T inType) {
            return this.typeLookup.lookupType(inType);
        }

        @Override
        public Void handleProviderRecordType(ProviderRecordType recordType) {
            this.currentRecordType = this.typeLookup.lookupType(recordType);

            for (ProviderField field : recordType.getDeclaredFields()) {
                field.accept(this);
            }

            this.currentRecordType = null;
            return null;
        }

        @Override
        public Void handleProviderEnumType(ProviderEnumType enumType) {
            this.currentEnumType = this.typeLookup.lookupType(enumType);

            for (ProviderEnumMember member : enumType.getMembers()) {
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
            Optional<ProviderField> optionalMappedPredecessor = field.findFirstSuccessorMatching(
                    this.mappedFields::contains
            );

            boolean typeChange = (optionalMappedPredecessor.isPresent() &&
                    !optionalMappedPredecessor.get().getType().equals(field.getType()));

            if (!optionalMappedPredecessor.isPresent() || typeChange) {
                // If no predecessor exists or a type change has occurred, create a new field
                Optional<String> internalName = (field.getPublicName().equals(field.getInternalName())) ?
                        Optional.empty() : Optional.of(field.getInternalName());
                Optionality optionality = this.determineOptionalityForField(field);
                Type type = this.lookupType(field.getType());

                // Ensure that the internal name is unique
                MemberName memberName = new MemberName(field.getOwner().getInternalName(), field.getInternalName());
                this.assertUniqueMemberName(memberName);

                ProviderField mappedField = new ProviderField(field.getPublicName(),
                        internalName,
                        this.currentRecordType,
                        type,
                        optionality,
                        Optional.empty());

                this.mappedFields.add(mappedField);
                this.knownMemberNames.add(memberName);
            }

            return null;
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
                    this.mappedMembers::contains
            );

            if (!optionalMappedPredecessor.isPresent()) {
                Optional<String> internalName = (enumMember.getPublicName().equals(enumMember.getInternalName())) ?
                        Optional.empty() : Optional.of(enumMember.getInternalName());

                // Ensure that the internal name is unique
                MemberName memberName = new MemberName(enumMember.getOwner().getInternalName(),
                        enumMember.getInternalName());
                this.assertUniqueMemberName(memberName);

                ProviderEnumMember mappedMember = new ProviderEnumMember(enumMember.getPublicName(),
                        internalName,
                        this.currentEnumType,
                        Optional.empty());

                this.mappedMembers.add(mappedMember);
            }

            return null;
        }

        /**
         * Representation of a type-qualified member name for uniqueness tests.
         */
        private static class MemberName {

            public final String typeName;

            public final String memberName;

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
         * Predicate to detect type changes in a revision history. This is essentially a workaround as the
         * takeWhile operation is only introduced in Java 9, and we want Java 8 support.
         */
        private static class TypeChangeDetectionPredicate implements Predicate<ProviderField> {

            private final Type referenceType;

            private boolean changeDetected = false;

            public TypeChangeDetectionPredicate(ProviderField referenceField) {
                this.referenceType = referenceField.getType();
            }

            @Override
            public boolean test(ProviderField providerField) {
                if (this.changeDetected) {
                    return false;
                } else if (!this.referenceType.equals(providerField.getType())) {
                    this.changeDetected = true;
                    return false;
                } else {
                    return true;
                }
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
