package gutta.apievolution.core.util;

import java.util.Iterator;
import java.util.NoSuchElementException;

/**
 * Iterator implementation that concatenates several existing iterators into a consecutive sequence.
 * @param <T> The type of the elements of the iterators
 */
public class ConcatenatedIterator<T> implements Iterator<T> {

    private final Iterator<T>[] iterators;

    private int iteratorIndex = 0;

    private Iterator<T> currentIterator;

    /**
     * Creates a new concatenated iterators of the given iterables.
     * @param iterables The iterables to build a concatenated iterator of
     */
    @SafeVarargs
    public ConcatenatedIterator(Iterable<T>... iterables) {
        this(createIterators(iterables));
    }

    @SuppressWarnings("unchecked")
    private static <X> Iterator<X>[] createIterators(Iterable<X>[] iterables) {
        Iterator<X>[] iterators = new Iterator[iterables.length];
        for (int index = 0; index < iterators.length; index++) {
            iterators[index] = iterables[index].iterator();
        }
        return iterators;
    }

    /**
     * Creates a new concatenated iterator from the given iterators.
     * @param iterators The iterators to concatenate
     */
    @SafeVarargs
    public ConcatenatedIterator(Iterator<T>... iterators) {
        this.iterators = iterators;
        this.currentIterator = (iterators.length == 0) ? new EmptyIterator<>() : iterators[0];
    }

    @Override
    public boolean hasNext() {
        while (true) {
            if (this.currentIterator.hasNext()) {
                return true;
            }

            this.iteratorIndex++;
            if (this.iteratorIndex >= this.iterators.length) {
                return false;
            }

            this.currentIterator = this.iterators[this.iteratorIndex];
        }
    }

    @Override
    public T next() {
        return this.currentIterator.next();
    }

    private static class EmptyIterator<T> implements Iterator<T> {

        @Override
        public boolean hasNext() {
            return false;
        }

        @Override
        public T next() {
            throw new NoSuchElementException();
        }
    }

}
