package gutta.apievolution.core.apimodel.consumer;

import gutta.apievolution.core.apimodel.EnumMember;
import gutta.apievolution.core.util.EqualityUtil;

/**
 * Consumer-specific implementation of an {@link EnumMember}.
 */
public class ConsumerEnumMember extends EnumMember<ConsumerEnumType, ConsumerEnumMember>
        implements ConsumerApiDefinitionElement {
        
    /**
     * Creates a new enum member from the given data.
     *
     * @param publicName   The enum member's public name
     * @param internalName The enum member's internal name, if any. If {@code null} the
     *                     public name is assumed
     * @param owner        The enum type that owns this member
     */
    ConsumerEnumMember(final String publicName, final String internalName,
            final ConsumerEnumType owner) {
        super(publicName, internalName, owner);
    }

    @Override
    public int hashCode() {
        return super.hashCode();
    }

    @Override
    public boolean equals(Object that) {
        return EqualityUtil.equals(this, that, this::stateEquals);
    }

    boolean stateEquals(ConsumerEnumMember that) {
        return super.stateEquals(that);
    }

    @Override
    public <R> R accept(ConsumerApiDefinitionElementVisitor<R> visitor) {
        return visitor.handleConsumerEnumMember(this);
    }
}
