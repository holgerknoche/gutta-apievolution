package gutta.apievolution.core.util;

import java.util.Iterator;

/**
 * Iterable integer range.
 */
public abstract class IntegerRange implements Iterable<Integer> { // NOSONAR This must be a class due to inner
    // classes

    /**
     * Creates a new unbounded integer range.
     *
     * @return see above
     */
    public static IntegerRange unbounded() {
        return new UnboundedIntegerRange();
    }

    private static class UnboundedIntegerRange extends IntegerRange {

        @Override
        public Iterator<Integer> iterator() {
            return new UnboundedIntegerRangeIterator();
        }

    }

    private static class UnboundedIntegerRangeIterator implements Iterator<Integer> {

        private int value;

        @Override
        public boolean hasNext() {
            return true;
        }

        @Override
        public Integer next() { // NOSONAR The range is unbounded, so there is no end of the collection
            return this.value++;
        }

    }

}
