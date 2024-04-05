package gutta.apievolution.fixedformat.objectmapping;

import static gutta.apievolution.fixedformat.objectmapping.Flags.*;

abstract class TypeMapper<T> {    
    
    protected boolean isCacheable() {
        return false;
    }

    public final int getMaxLength() {
        return (this.getDataLength() + Flags.FLAGS_SIZE);
    }

    protected abstract int getDataLength();

    public final T readValue(FixedFormatData data) {
        byte flags = data.readFlagsByte();

        switch (flags) {
        case IS_ABSENT:
            return this.handleAbsentValue(data);

        case IS_PRESENT:
            return this.readRegularValue(data);

        case IS_UNREPRESENTABLE:
            return this.handleUnrepresentableValue();

        default:
            throw new InvalidDataException("Unsupported value flags: '" + flags + "'.");
        }
    }

    protected T handleAbsentValue(FixedFormatData data) {
        data.skipBytes(this.getDataLength());
        return null;
    }
    
    protected abstract T readRegularValue(FixedFormatData data);

    protected abstract T handleUnrepresentableValue();

    protected boolean isUnrepresentable(Object value) {
        return false;
    }

    public final void writeValue(Object value, FixedFormatData data) {
        if (value == null) {
            data.writeFlagsByte(IS_ABSENT);
            data.writePadding(this.getDataLength());
        } else if (this.isUnrepresentable(value)) {
            data.writeFlagsByte(IS_UNREPRESENTABLE);
            data.writePadding(this.getDataLength());
        } else {
            data.writeFlagsByte(IS_PRESENT);
            this.writeRegularValue(value, data);
        }
    }

    protected abstract void writeRegularValue(Object value, FixedFormatData data);

}
