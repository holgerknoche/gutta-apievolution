package gutta.apievolution.inprocess.provider;

import gutta.apievolution.inprocess.ImplementedBy;

@ImplementedBy(ProviderRecordImpl.class)
public interface ProviderRecord {

    Integer getField();

}
