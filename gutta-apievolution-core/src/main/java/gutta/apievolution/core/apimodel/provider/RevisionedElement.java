package gutta.apievolution.core.apimodel.provider;

import java.util.Iterator;
import java.util.Optional;
import java.util.Spliterator;
import java.util.Spliterators;
import java.util.function.Predicate;
import java.util.stream.Stream;
import java.util.stream.StreamSupport;

/**
 * Revisioned elements have a history of revisions, represented by predecessors and successors.
 * @param <T> The concrete type of revisioned element
 */
public interface RevisionedElement<T extends RevisionedElement<T>> {

    /**
     * Returns this element's predecessor, if it exists.
     * @return see above
     */
    Optional<T> getPredecessor();

    /**
     * Returns this element's successor, if it exists.
     * @return see above
     */
    Optional<T> getSuccessor();

    /**
     * Finds the first successor of this revisioned element matching the given predicate.
     * @param matchPredicate The predicate to match
     * @return The first successor matching the predicate
     */
    default Optional<T> findFirstSuccessorMatching(Predicate<T> matchPredicate) {
        return this.successorStream(false)
                .filter(matchPredicate)
                .findFirst();
    }

    /**
     * Finds the first predecessor of this revisioned element matching the given predicate.
     * @param matchPredicate The predicate to match
     * @return The first predecessor matching the predicate
     */
    default Optional<T> findFirstPredecessorMatching(Predicate<T> matchPredicate) {
        return this.predecessorStream(false)
                .filter(matchPredicate)
                .findFirst();
    }

    /**
     * Returns a stream of all predecessors of this element, possibly including the element itself.
     * @param inclusive Denotes whether the element itself should be included in the stream
     * @return The stream of predecessors
     */
    default Stream<T> predecessorStream(boolean inclusive) {
        Iterator<T> iterator = new PredecessorIterator<>(this, inclusive);
        Spliterator<T> spliterator = Spliterators.spliteratorUnknownSize(iterator, 0);
        return StreamSupport.stream(spliterator, false);
    }

    /**
     * Returns a stream of all successors of this element, possibly including the element itself.
     * @param inclusive Denotes whether the element itself should be included in the stream
     * @return The stream of successors
     */
    default Stream<T> successorStream(boolean inclusive) {
        Iterator<T> iterator = new SuccessorIterator<>(this, inclusive);
        Spliterator<T> spliterator = Spliterators.spliteratorUnknownSize(iterator, 0);
        return StreamSupport.stream(spliterator, false);
    }

}
