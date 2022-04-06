package gutta.apievolution.core.apimodel;

import java.util.Collections;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;
import java.util.Set;
import java.util.function.Function;
import java.util.stream.Collectors;

/**
 * Abstract superclass for all elements (such as user-defined types) that can be
 * specified within an API definition.
 */
abstract class AbstractApiDefinitionElement {

    private final Set<Annotation> annotations;

    private final Map<String, Annotation> annotationLookup;

    private final String publicName;

    private final String internalName;

    /**
     * Creates a new API definition element from the given data.
     *
     * @param publicName   The element's public name
     * @param internalName The element's internal name, if any. Otherwise, the
     *                     public name is assumed
     */
    protected AbstractApiDefinitionElement(final String publicName, final Optional<String> internalName) {
        this(Collections.emptySet(), publicName, internalName);
    }

    /**
     * Creates a new API definition element from the given data.
     *
     * @param annotations  The annotations on the element
     * @param publicName   The element's public name
     * @param internalName The element's internal name, if any. Otherwise, the
     *                     public name is assumed
     */
    protected AbstractApiDefinitionElement(Set<Annotation> annotations, String publicName,
            Optional<String> internalName) {
        this.annotations = annotations;
        this.annotationLookup = (annotations.isEmpty()) ? Collections.emptyMap()
                : annotations.stream().collect(Collectors.toMap(Annotation::getName, Function.identity()));
        this.publicName = publicName;
        this.internalName = internalName.orElse(publicName);
    }

    /**
     * Returns this element's public name.
     *
     * @return see above
     */
    public String getPublicName() {
        return this.publicName;
    }

    /**
     * Returns this element's internal name.
     *
     * @return see above
     */
    public String getInternalName() {
        return this.internalName;
    }

    /**
     * Returns the internal name as an optional, which is only present if the
     * internal name differs from the public name.
     *
     * @return see above
     */
    public Optional<String> getOptionalInternalName() {
        return (this.publicName.equals(this.internalName)) ? Optional.empty() : Optional.of(this.internalName);
    }

    /**
     * Returns the annotations on this element.
     *
     * @return see above
     */
    public Set<Annotation> getAnnotations() {
        return this.annotations;
    }

    /**
     * Returns the annotation of the given type, if it exists.
     *
     * @param type The type of the desired annotation
     * @return The optional annotation
     */
    public Optional<Annotation> getAnnotation(String type) {
        return Optional.ofNullable(this.annotationLookup.get(type));
    }

    /**
     * Adds an annotation to this element, provided that it is mutable.
     *
     * @param annotation The annotation to add
     */
    protected void addAnnotation(Annotation annotation) {
        this.assertMutability();

        if (!this.getAnnotation(annotation.getName()).isPresent()) {
            this.annotations.add(annotation);
            this.annotationLookup.put(annotation.getName(), annotation);
        }
    }

    protected abstract void assertMutability();

    @Override
    public int hashCode() { // NOSONAR Equals is overridden in the concrete subclasses
        return Objects.hash(this.publicName, this.internalName);
    }

    boolean stateEquals(AbstractApiDefinitionElement that) {
        return this.publicName.equals(that.publicName) && this.internalName.equals(that.internalName);
    }

    @Override
    public String toString() {
        if (this.getInternalName().equals(this.getPublicName())) {
            return this.getPublicName();
        } else {
            return this.getPublicName() + "(" + this.getInternalName() + ")";
        }
    }

}
