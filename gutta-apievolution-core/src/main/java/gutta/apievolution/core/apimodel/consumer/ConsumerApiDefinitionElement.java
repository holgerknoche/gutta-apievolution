package gutta.apievolution.core.apimodel.consumer;

interface ConsumerApiDefinitionElement {

    <R> R accept(ConsumerApiDefinitionElementVisitor<R> visitor);

}
