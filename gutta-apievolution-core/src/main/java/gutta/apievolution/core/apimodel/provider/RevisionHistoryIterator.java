package gutta.apievolution.core.apimodel.provider;

import java.util.Iterator;
import java.util.NoSuchElementException;
import java.util.Optional;

/**
 * Iterator for iterating over the revision history of a
 * {@link RevisionedElement}.
 *
 * @param <T> The concrete type of revisioned element
 */
abstract class RevisionHistoryIterator<T extends RevisionedElement<T>> implements Iterator<T> {

    private RevisionedElement<T> currentElement;

    protected RevisionHistoryIterator(RevisionedElement<T> startElement) {
        this.currentElement = startElement;
    }

    protected abstract Optional<T> navigateToNext(RevisionedElement<T> element);

    @Override
    public boolean hasNext() {
        return this.navigateToNext(this.currentElement).isPresent();
    }

    @Override
    @SuppressWarnings("unchecked")
    public T next() {
        Optional<T> optionalNext = this.navigateToNext(this.currentElement);

        if (optionalNext.isPresent()) {
            T next = optionalNext.get();
            this.currentElement = next;
            return next;
        } else {
            throw new NoSuchElementException();
        }
    }

}
