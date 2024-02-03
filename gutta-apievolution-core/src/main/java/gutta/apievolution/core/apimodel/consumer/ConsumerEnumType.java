package gutta.apievolution.core.apimodel.consumer;

import gutta.apievolution.core.apimodel.EnumType;
import gutta.apievolution.core.util.EqualityUtil;

import static gutta.apievolution.core.apimodel.Conventions.noInternalName;

/**
 * Consumer-specific implementation of an {@link EnumType}.
 */
public class ConsumerEnumType extends EnumType<ConsumerApiDefinition, ConsumerEnumType, ConsumerEnumMember> implements ConsumerUserDefinedType {

    /**
     * Creates a new enum type from the given data.
     *
     * @param publicName   The enum type's public name
     * @param internalName The enum type's internal name, if any. If {@code null} the public name is assumed
     * @param typeId       The enum type's type id
     * @param owner        The API definition that owns this enum type
     */
    ConsumerEnumType(final String publicName, final String internalName, final int typeId, final ConsumerApiDefinition owner) {
        super(publicName, internalName, typeId, owner);
    }

    // Element creators

    /**
     * Creates a new member in this enum type.
     * 
     * @param publicName The member's public name
     * @return The created enum member
     */
    public ConsumerEnumMember newEnumMember(String publicName) {
        return this.newEnumMember(publicName, noInternalName());
    }

    /**
     * Creates a new member in this enum type.
     * 
     * @param publicName   The member's public name
     * @param internalName The member's internal name
     * @return The created enum member
     */
    public ConsumerEnumMember newEnumMember(String publicName, String internalName) {
        return new ConsumerEnumMember(publicName, internalName, this);
    }

    @Override
    public int hashCode() {
        return super.hashCode();
    }

    @Override
    public boolean equals(Object that) {
        return EqualityUtil.equals(this, that, this::stateEquals);
    }

    boolean stateEquals(ConsumerEnumType that) {
        return super.stateEquals(that);
    }

    @Override
    public <R> R accept(ConsumerApiDefinitionElementVisitor<R> visitor) {
        return visitor.handleConsumerEnumType(this);
    }

}
