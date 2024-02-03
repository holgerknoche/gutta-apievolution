package gutta.apievolution.inprocess.customerexample.consumer.dynproxy.v6;

import gutta.apievolution.core.util.EqualityUtil;

import java.util.Objects;

public class POBoxAddressImpl extends AddressImpl implements POBoxAddress {
	
	private Integer boxNo;

	@Override
	public Integer getBoxNo() {
		return this.boxNo;
	}

	@Override
	public void setBoxNo(Integer boxNo) {
		this.boxNo = boxNo;
	}

	@Override
	public int hashCode() {
		return (this.getPostalCode() + this.boxNo);
	}
	
	public boolean equals(Object that) {
		return EqualityUtil.equals(this, that, this::equalsInternal);
	}
	
	private boolean equalsInternal(POBoxAddressImpl that) {
		return super.equals(that) &&
			   Objects.equals(this.boxNo, that.boxNo);
	}
	
}
