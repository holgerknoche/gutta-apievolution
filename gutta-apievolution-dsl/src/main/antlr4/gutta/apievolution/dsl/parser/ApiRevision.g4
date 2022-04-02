grammar ApiRevision;

apiDefinition:
	annotations+=annotation*
    refToken='api' name=qualifiedName replaces=apiReplacesClause? '{'
	    elements+=userDefinedTypeOrOperation*
	'}'
	EOF
;

apiReplacesClause:
    refToken='replaces' target=qualifiedName
;
	
annotation:
	typeToken=ANNOTATION_NAME '(' value=STRING_LITERAL ')'
;
	
userDefinedTypeOrOperation:
	enumType |
	recordType |
	operation
;

replacesClause:
    refToken='replaces' (itemName=identifier | nothing='nothing')
;

asClause:
    refToken='as' aliasName=identifier
;

enumType:
    refToken='enum' name=identifier replaces=replacesClause? as=asClause? '{'
        members+=enumMember*
    '}'
;

enumMember:
    name=identifier replaces=replacesClause? as=asClause?
;

recordType:
	annotations+=annotation*
    modifiers+=recordModifier* refToken=(K_RECORD | K_EXCEPTION) name=identifier ('extends' superType=identifier)? replaces=replacesClause? as=asClause? '{'
        fields+=field*
    '}'
;

recordModifier:
    K_ABSTRACT |
    K_OPTIONAL |
    K_OPTIN |
    K_MANDATORY
;

field:
    modifier=fieldModifier? type=typeReference name=identifier replaces=fieldReplacesClause? as=asClause?
;

fieldModifier:
    K_OPTIONAL |
    K_OPTIN |
    K_MANDATORY
;

fieldReplacesClause:
    refToken='replaces' (items+=qualifiedName (',' items+=qualifiedName)* | nothing='nothing')
;

typeReference:
    atomicType |
    boundedType |
	userDefinedTypeReference |
	typeReference (unbounded='*' | '[' cardinality=INT_LITERAL ']')
;

atomicType:
	K_INT32 | K_INT64
;
boundedType:
	type='string' ('(' bound=INT_LITERAL ')')? |
	type='numeric' ('(' integerPlaces=INT_LITERAL (',' fractionalPlaces=INT_LITERAL)? ')')?
;

userDefinedTypeReference:
    typeName=identifier
;

operation:
    annotations+=annotation*
    refToken='operation' name=identifier '(' (parameterType=userDefinedTypeReference)? ')' ':' resultType=userDefinedTypeReference ('throws' exceptions+=userDefinedTypeReference (',' exceptions+=userDefinedTypeReference)*)? replaces=replacesClause? as=asClause?
;

qualifiedName:
	parts+=identifier ('.' parts+=identifier)*
;
	
identifier:
	id=ID | literal=LITERAL_ID
;

// ------------- Lexer Rules

K_ABSTRACT:
    'abstract'
;

K_EXCEPTION:
    'exception'
;

K_INT32:
    'int32'
;

K_INT64:
    'int64'
;

K_MANDATORY:
    'mandatory'
;

K_RECORD:
    'record'
;

K_OPTIN:
    'optin'
;

K_OPTIONAL:
    'optional'
;

COMMENT:
	'//' ~[\n]* ('\n' | EOF) -> skip
;

ANNOTATION_NAME:
	'@'[A-Za-z0-9]+
;

INT_LITERAL:
	[0-9]+
;

STRING_LITERAL:
	'"'~["]*'"'
;

LITERAL_ID:
	'\''~[']*'\''
;

ID:
	[A-Za-z][A-Za-z0-9_]*
;
	
WS:
	[ \r\t\n]+ -> skip
;
