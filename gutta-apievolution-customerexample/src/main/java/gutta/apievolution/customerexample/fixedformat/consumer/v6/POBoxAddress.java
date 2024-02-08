package gutta.apievolution.customerexample.fixedformat.consumer.v6;

import gutta.apievolution.core.util.EqualityUtil;
import gutta.apievolution.fixedformat.objectmapping.TypeId;

@TypeId(4)
public class POBoxAddress extends Address {
	
	private int boxNo;

	public int getBoxNo() {
		return this.boxNo;
	}

	public void setBoxNo(int boxNo) {
		this.boxNo = boxNo;
	}

	@Override
	public int hashCode() {
		return (this.getPostalCode() + this.boxNo);
	}
	
	public boolean equals(Object that) {
	    return EqualityUtil.equals(this, that, this::equals);
	}
	
	private boolean equals(POBoxAddress that) {
		return super.equals(that) &&
			   (this.boxNo == that.boxNo);
	}
	
}
