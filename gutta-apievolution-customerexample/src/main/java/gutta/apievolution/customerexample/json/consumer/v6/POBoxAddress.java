package gutta.apievolution.customerexample.json.consumer.v6;

import com.fasterxml.jackson.annotation.JsonTypeName;
import gutta.apievolution.core.util.EqualityUtil;

@JsonTypeName("POBoxAddress")
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
