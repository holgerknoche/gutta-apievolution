package gutta.apievolution.core.apimodel.provider;

import gutta.apievolution.core.apimodel.ListType;
import gutta.apievolution.core.apimodel.QualifiedName;
import gutta.apievolution.core.apimodel.Type;

import java.util.Arrays;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.ListIterator;
import java.util.Map;
import java.util.Optional;
import java.util.Set;
import java.util.function.Function;
import java.util.stream.Collectors;

/**
 * A revision history embodies an ordered sequence of dependent provider API
 * definitions.
 */
public class RevisionHistory {

    private final List<ProviderApiDefinition> revisions;

    private final Map<Integer, ProviderApiDefinition> revisionLookup;

    /**
     * Creates a new revision history from the given revisions.
     *
     * @param revisions The revisions that make up the history
     */
    public RevisionHistory(ProviderApiDefinition... revisions) {
        this(Arrays.asList(revisions));
    }

    /**
     * Creates a new revision history from the given revisions.
     *
     * @param revisions The revisions that make up the history
     */
    public RevisionHistory(List<ProviderApiDefinition> revisions) {
        this.revisions = revisions;

        this.revisionLookup = revisions.stream()
                .collect(Collectors.toMap(ProviderApiDefinition::getRevision, Function.identity()));
    }

    /**
     * Returns whether this history contains no elements.
     *
     * @return see above
     */
    public boolean isEmpty() {
        return this.revisions.isEmpty();
    }

    /**
     * Retrieves a revision within this history by its revision number.
     *
     * @param revisionNo The desired revision's revision number
     * @return The desired revision, if it exists
     */
    public Optional<ProviderApiDefinition> getRevision(int revisionNo) {
        return Optional.ofNullable(this.revisionLookup.get(revisionNo));
    }

    /**
     * Returns a {@link ListIterator} that is positioned after the last element of
     * the history.
     *
     * @return see above
     */
    public ListIterator<ProviderApiDefinition> reverseIterator() {
        return this.revisions.listIterator(this.revisions.size());
    }

    /**
     * Returns the API definitions contained in this history as a set.
     *
     * @return see above
     */
    public Set<ProviderApiDefinition> revisionSet() {
        return new HashSet<>(this.revisions);
    }

    /**
     * Checks this revision history for consistency.
     */
    public void checkConsistency() {
        if (this.isEmpty()) {
            return;
        }

        // Instantiate the relevant checks
        List<RevisionCheck> checks = Arrays.asList(new RevisionsAreFinalizedCheck(), new SameAPINameCheck(), new DescendingRevisionsCheck(),
                new NoTypeChangesForFieldsCheck());

        // Iterate over the revisions in reverse order and invoke the checks on each
        // revision
        ListIterator<ProviderApiDefinition> revisionsToCheck = this.reverseIterator();
        while (revisionsToCheck.hasPrevious()) {
            ProviderApiDefinition revisionToCheck = revisionsToCheck.previous();
            checks.forEach(check -> check.checkRevision(revisionToCheck));
        }

        // Notify each check that all revisions have been processed
        checks.forEach(RevisionCheck::atEnd);
    }

    /**
     * Internal interface for checks on revisions.
     */
    private interface RevisionCheck {

        void checkRevision(ProviderApiDefinition revision);

        default void atEnd() {
        }
    }

    /**
     * Check implementation to ensure that all revisions in the history have the
     * same name.
     */
    private static class SameAPINameCheck implements RevisionCheck {

        private QualifiedName expectedName = null;

        @Override
        public void checkRevision(ProviderApiDefinition revision) {
            if (this.expectedName == null) {
                this.expectedName = revision.getName();
            } else {
                if (!this.expectedName.equals(revision.getName())) {
                    throw new InconsistentHistoryException("Different API names within the same revision history.");
                }
            }
        }

    }

    /**
     * Check implementation to ensure that revisions in the history have descending
     * revision numbers.
     */
    private static class DescendingRevisionsCheck implements RevisionCheck {

        private int lastRevision = Integer.MAX_VALUE;

        @Override
        public void checkRevision(ProviderApiDefinition revision) {
            if (revision.getRevision() >= lastRevision) {
                throw new InconsistentHistoryException("Revisions must have ascending revision numbers.");
            } else {
                this.lastRevision = revision.getRevision();
            }
        }
    }

    /**
     * Check implementation to ensure that no type changes occur for fields in
     * record types.
     */
    private static class NoTypeChangesForFieldsCheck implements RevisionCheck {

        private final Map<ProviderField, Type> lastTypes = new HashMap<>();

        @Override
        public void checkRevision(ProviderApiDefinition revision) {
            revision.forEach(element -> {
                if (element instanceof ProviderRecordType) {
                    this.checkRecordType((ProviderRecordType) element);
                }
            });
        }

        private void checkRecordType(ProviderRecordType recordType) {
            recordType.forEach(this::checkField);
        }

        private void checkField(ProviderField field) {
            Optional<ProviderField> optionalSuccessor = field.findFirstSuccessorMatching(this.lastTypes::containsKey);

            if (!optionalSuccessor.isPresent()) {
                // No type associated yet to any successor, therefore, no check necessary
                this.lastTypes.put(field, field.getType());
                return;
            }

            // Determine the equivalent type in the current revision
            Type successorType = optionalSuccessor.get().getType();
            ProviderApiDefinition currentRevision = field.getOwner().getOwner();
            Type expectedType = this.determineEquivalentInRevision(successorType, currentRevision);

            if (expectedType == null) {
                throw new InconsistentHistoryException(
                        "No equivalent type for " + successorType + " in revision " + currentRevision + ".");
            } else if (!expectedType.equals(field.getType())) {
                throw new InconsistentHistoryException("Illegal type change for field " + field + ".");
            }
        }

        private Type determineEquivalentInRevision(Type type, ProviderApiDefinition revision) {
            if (type instanceof ProviderRecordType) {
                // If the type is a record type, we need to find the predecessor of the
                // respective type in
                // the current revision
                ProviderRecordType recordType = (ProviderRecordType) type;
                Optional<ProviderRecordType> equivalentType = recordType
                        .findFirstPredecessorMatching(pred -> revision.equals(pred.getOwner()));

                return equivalentType.orElse(null);
            } else if (type instanceof ProviderEnumType) {
                // Same for enum types
                ProviderEnumType recordType = (ProviderEnumType) type;
                Optional<ProviderEnumType> equivalentType = recordType
                        .findFirstPredecessorMatching(pred -> revision.equals(pred.getOwner()));

                return equivalentType.orElse(null);
            } else if (type instanceof ListType) {
                // If the type is a list type, the element type must be converted recursively
                ListType listType = (ListType) type;
                Type elementType = this.determineEquivalentInRevision(listType.getElementType(), revision);

                return (listType.isBounded()) ? ListType.bounded(elementType, listType.getBound())
                        : ListType.unbounded(elementType);
            } else {
                // If the type is a basic type, it is the same in all revisions
                return type;
            }
        }

    }
    
    /**
     * Check implementation that all contained revisions are finalized.
     */
    private static class RevisionsAreFinalizedCheck implements RevisionCheck {
        
        @Override
        public void checkRevision(ProviderApiDefinition revision) {
            if (!revision.isFinalized()) {
                throw new InconsistentHistoryException("Revision '" + revision.getRevision() + "' is not finalized.");
            }            
        }
        
    }

}
