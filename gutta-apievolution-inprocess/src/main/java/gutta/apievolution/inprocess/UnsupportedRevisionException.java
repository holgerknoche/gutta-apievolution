package gutta.apievolution.inprocess;

/**
 * This exception denotes that an unsupported provider revision was requested.
 */
public class UnsupportedRevisionException extends RuntimeException {

    private static final long serialVersionUID = -5518977849903584494L;

    UnsupportedRevisionException(int requestedRevision, int[] supportedRevisions) {
        super(String.format("Revision %d is not supported. Supported revisions are: %d.", requestedRevision, supportedRevisions));
    }

}
