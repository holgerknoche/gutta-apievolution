package gutta.apievolution.dsl;

import gutta.apievolution.dsl.parser.ApiRevisionParser;

interface ProviderApiRevisionModelBuilderPass {

    default PredecessorType determinePredecessorType(final ApiRevisionParser.ReplacesClauseContext context) {
        if (context == null) {
            // If no replaces clause is given, we have an implicit predecessor
            return PredecessorType.IMPLICIT;
        } else if (context.nothing != null) {
            // If "nothing" is specified, there is explicitly no predecessor
            return PredecessorType.NONE;
        } else {
            // Otherwise, an explicit predecessor is given
            return PredecessorType.EXPLICIT;
        }
    }

    default PredecessorType determinePredecessorType(final ApiRevisionParser.FieldReplacesClauseContext context) {
        if (context == null) {
            // If no replaces clause is given, we have an implicit predecessor
            return PredecessorType.IMPLICIT;
        } else if (context.nothing != null) {
            // If "nothing" is specified, there is explicitly no predecessor
            return PredecessorType.NONE;
        } else {
            // Otherwise, an explicit predecessor is given
            return PredecessorType.EXPLICIT;
        }
    }

}
