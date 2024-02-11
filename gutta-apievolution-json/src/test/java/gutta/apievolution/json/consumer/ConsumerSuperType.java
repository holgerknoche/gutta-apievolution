package gutta.apievolution.json.consumer;

import com.fasterxml.jackson.annotation.JsonSubTypes;
import com.fasterxml.jackson.annotation.JsonSubTypes.Type;
import com.fasterxml.jackson.annotation.JsonTypeInfo;
import com.fasterxml.jackson.annotation.JsonTypeInfo.Id;
import com.fasterxml.jackson.annotation.JsonTypeName;

@JsonTypeInfo(use = Id.NAME)
@JsonSubTypes({@Type(ConsumerSubTypeA.class), @Type(ConsumerSubTypeB.class)})
@JsonTypeName("ConsumerSuperType")
public class ConsumerSuperType {

}
