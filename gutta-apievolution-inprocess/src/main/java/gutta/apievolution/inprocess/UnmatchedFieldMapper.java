package gutta.apievolution.inprocess;

class UnmatchedFieldMapper implements FieldMapper {

    @Override
    public Object mapField(Object targetObject) {
        // Always null, as the field is unmatched
        return null;
    }

}
