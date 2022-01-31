package gutta.apievolution.core.apimodel;

import java.util.Objects;
import java.util.Optional;

/**
 * A field represents a data field within a record type.
 * @param <R> The concrete type of the record type
 * @param <F> The concrete type of the field (e.g., provider or consumer)
 */
public abstract class Field<R extends RecordType<?, R, F>, F extends Field<R, F>> extends AbstractApiDefinitionElement {

    private final R owner;

    private final Type type;

    private final Optionality optionality;

    private final boolean inherited;

    /**
     * Creates a new field from the given data.
     * @param publicName The public name of the field
     * @param internalName The internal name of the field, if applicable. Otherwise, the public name is assumed
     * @param owner The record type that owns this field
     * @param type The field's type
     * @param optionality The field's optionality
     * @param inherited Denotes whether the field is inherited from a supertype
     */
    @SuppressWarnings("unchecked")
    protected Field(final String publicName, final Optional<String> internalName, final R owner, final Type type,
                    Optionality optionality, boolean inherited) {
        super(publicName, internalName);

        this.owner = owner;
        this.type = type;
        this.optionality = optionality;
        this.inherited = inherited;

        if (inherited) {
            owner.addInheritedField((F) this);
        } else {
            owner.addDeclaredField((F) this);
        }
    }

    /**
     * Returns the field's type.
     * @return see above
     */
    public Type getType() {
        return this.type;
    }

    /**
     * Returns the record type that owns the field.
     * @return see above
     */
    public R getOwner() {
        return this.owner;
    }

    /**
     * Returns the field's optionality.
     * @return see above
     */
    public Optionality getOptionality() {
        return this.optionality;
    }

    /**
     * Returns whether this field is inherited from a supertype.
     * @return see above
     */
    public boolean isInherited() {
        return this.inherited;
    }

    @Override
    public int hashCode() { // NOSONAR Equals is overridden in the concrete subclasses
        return super.hashCode() + Objects.hash(this.optionality, this.type);
    }

    /**
     * Compares this field's state against the state of the given member.
     * @param that The field to compare against
     * @return Whether the states are equal
     */
    protected boolean stateEquals(Field<R, F> that) {
        return super.stateEquals(that) &&
                this.type.equals(that.type) &&
                this.optionality.equals(that.optionality) &&
                this.inherited == that.inherited;
    }

}
