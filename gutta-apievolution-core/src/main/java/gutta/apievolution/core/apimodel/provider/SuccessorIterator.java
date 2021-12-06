package gutta.apievolution.core.apimodel.provider;

import java.util.Optional;

/**
 * Specific variant of the {@link RevisionHistoryIterator} to iterate over the successors of a
 * {@link RevisionedElement}.
 * @param <T> The concrete type of the revisioned element
 */
class SuccessorIterator<T extends RevisionedElement<T>> extends RevisionHistoryIterator<T> {

    public SuccessorIterator(RevisionedElement<T> element, boolean inclusive) {
        // If this iterator is inclusive, start with a synthetic predecessor to avoid
        // special handling of the first element
        super((inclusive) ? new SyntheticPredecessor<>(element) : element);
    }

    @Override
    protected Optional<T> navigateToNext(RevisionedElement<T> element) {
        return element.getSuccessor();
    }

    private static class SyntheticPredecessor<E extends RevisionedElement<E>> implements RevisionedElement<E> {

        private final Optional<RevisionedElement<E>> successor;

        public SyntheticPredecessor(RevisionedElement<E> element) {
            this.successor = Optional.of(element);
        }

        @Override
        public Optional<E> getPredecessor() {
            return Optional.empty();
        }

        @Override
        @SuppressWarnings("unchecked")
        public Optional<E> getSuccessor() {
            return (Optional<E>) this.successor;
        }

    }

}
