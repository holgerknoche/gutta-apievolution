package gutta.apievolution.customerexample.inprocess.consumer.objectmapping.v6;

import gutta.apievolution.core.util.EqualityUtil;

public class POBoxAddress extends Address {
	
	private Integer boxNo;

	public Integer getBoxNo() {
		return this.boxNo;
	}

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
	
	private boolean equalsInternal(POBoxAddress that) {
		return super.equals(that) &&
			   (this.boxNo == that.boxNo);
	}
	
}
