package gutta.apievolution.core.apimodel.consumer;

import gutta.apievolution.core.apimodel.EnumMember;

/**
 * Consumer-specific implementation of an {@link EnumMember}.
 */
public class ConsumerEnumMember extends EnumMember<ConsumerEnumType, ConsumerEnumMember>
        implements ConsumerApiDefinitionElement {

    /**
     * Creates a simple enum member where the public name equals the internal name.
     *
     * @param publicName   The enum member's public name
     * @param owner        The enum type that owns this member
     */
    public static ConsumerEnumMember create(String publicName, ConsumerEnumType owner) {
        return new ConsumerEnumMember(publicName, null, owner);
    }
        
    /**
     * Creates a new enum member from the given data.
     *
     * @param publicName   The enum member's public name
     * @param internalName The enum member's internal name, if any. If {@code null} the
     *                     public name is assumed
     * @param owner        The enum type that owns this member
     */
    public ConsumerEnumMember(final String publicName, final String internalName,
            final ConsumerEnumType owner) {
        super(publicName, internalName, owner);
    }

    @Override
    public int hashCode() {
        return super.hashCode();
    }

    @Override
    public boolean equals(Object that) {
        if (this == that) {
            return true;
        } else if (that instanceof ConsumerEnumMember) {
            return this.stateEquals((ConsumerEnumMember) that);
        } else {
            return false;
        }
    }

    boolean stateEquals(ConsumerEnumMember that) {
        return super.stateEquals(that);
    }

    @Override
    public <R> R accept(ConsumerApiDefinitionElementVisitor<R> visitor) {
        return visitor.handleConsumerEnumMember(this);
    }
}
