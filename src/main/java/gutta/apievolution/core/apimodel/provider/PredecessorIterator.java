package gutta.apievolution.core.apimodel.provider;

import java.util.Optional;

/**
 * Specific variant of the {@link RevisionHistoryIterator} to iterate over the predecessors of a
 * {@link RevisionedElement}.
 * @param <T> The concrete type of the revisioned element
 */
class PredecessorIterator<T extends RevisionedElement<T>> extends RevisionHistoryIterator<T> {

    public PredecessorIterator(RevisionedElement<T> element, boolean inclusive) {
        // If this iterator is inclusive, start with a synthetic predecessor to avoid
        // special handling of the first element
        super((inclusive) ? new SyntheticSuccessor<>(element) : element);
    }

    @Override
    protected Optional<T> navigateToNext(RevisionedElement<T> element) {
        return element.getPredecessor();
    }

    private static class SyntheticSuccessor<E extends RevisionedElement<E>> implements RevisionedElement<E> {

        private final Optional<RevisionedElement<E>> predecessor;

        public SyntheticSuccessor(RevisionedElement<E> element) {
            this.predecessor = Optional.of(element);
        }

        @Override
        @SuppressWarnings("unchecked")
        public Optional<E> getPredecessor() {
            return (Optional<E>) this.predecessor;
        }

        @Override

        public Optional<E> getSuccessor() {
            return Optional.empty();
        }

    }

}
