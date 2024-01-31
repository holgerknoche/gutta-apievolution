package gutta.apievolution.json.customerexample.consumer.v6;

import com.fasterxml.jackson.annotation.JsonTypeName;

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
		if (this == that) {
			return true;
		} else if (that != null && this.getClass() == that.getClass()) {
			return this.equals((POBoxAddress) that);
		} else {
			return false;
		}
	}
	
	private boolean equals(POBoxAddress that) {
		return super.equals(that) &&
			   (this.boxNo == that.boxNo);
	}
	
}
