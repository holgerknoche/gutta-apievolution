package gutta.apievolution.core.util;

import java.util.Iterator;

/**
 * Iterable integer range.
 */
public abstract class IntegerRange implements Iterable<Integer> {

    /**
     * Creates a new unbounded integer range.
     * @return see above
     */
    public static IntegerRange unbounded() {
        return new UnboundedIntegerRange();
    }

    private static class UnboundedIntegerRange extends IntegerRange {

        @Override
        public Iterator<Integer> iterator() {
            return new IntegerRangeIterator();
        }

    }

    private static class IntegerRangeIterator implements Iterator<Integer> {

        private int value;

        @Override
        public boolean hasNext() {
            return true;
        }

        @Override
        public Integer next() {
            return this.value++;
        }

    }

}
