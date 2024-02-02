package gutta.apievolution.inprocess.customerexample.provider;

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

}
