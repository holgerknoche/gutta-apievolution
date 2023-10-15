package gutta.apievolution.inprocess;

public class UnsupportedRevisionException extends RuntimeException {
    
    private static final long serialVersionUID = -5518977849903584494L;

    public UnsupportedRevisionException(int requestedRevision, int[] supportedRevisions) {
        super(String.format("Revision %d is not supported. Supported revisions are: %d.", requestedRevision, supportedRevisions));
    }

}
