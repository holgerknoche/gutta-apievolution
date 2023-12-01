package gutta.apievolution.inprocess.consumer.dynproxy;

import java.util.List;

public interface ConsumerResult {

    String getResultField();

    ConsumerEnum getResultEnum();

    List<ConsumerEnum> getResultList();

    ConsumerRecord getResultRecord();

}
