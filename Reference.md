DSON Schema Reference
================================================================================
# Primitive Types

| type | description  |
|------|--------------|
|`symbol`             |a symbol in the target syntax. It can be thought of as unquoted string that's limited to certain characters (`[a-zA-Z_-][0-9a-zA-Z_-]*`).|
|`string`             |a string in the target syntax.  it can be single or double qouted.
|`integer`            |a signed integer with no range restriction|
|`uinteger`           |an unsigned integer with no range restriction|
|`listDelimiterSet`   |a 1 to 3 character set of delimiters (see below)|


# Schema Grammar
Note: you can copy/paste this grammar to the "Edit Grammar" tab at http://www.bottlecaps.de/rr/ui to view the railroad diagrams.
```
dsonSchema ::=
    (symbol ':' 'root'? type)*

primitiveType ::=
    'symbol' |
    'string' |
    'integer' |
    'uinteger' |
    'listDelimiterSet'

type ::=
    setofDefinition |
    listDefinition |
    objectDefinition |
    unionDefinition |
    tokenDefinition ';' |
    primitiveType ';' |
    symbol ';'

setofDefinition ::=
    'setof' primitiveType '{' value ( ',' value )* '}'

listDefinition ::=
    'list' listDelimiterSet? type |

objectDefinition ::=
    'object' '{' ( symbol ':' ('optional'|'required'|'nameless'|'terminator'|'delimited' string)* type)* '}'

unionDefinition ::=
    'union' '(' ( symbol ':' type)* '}'

tokenDefinition ::=
    'token' string

symbol ::= [a-zA-Z_-][0-9a-zA-Z_-]*
```

listDelimiterSet is 1 to 3 characters. If it is one character, then it specifies the character used to seperate elements of the list.  If there are 2 characters then it specifies the characters that surround the list.  If it is 3 characters then the outer characters specify the characters that surround the list and the inner character is the element delimiter.

Note: you can use the '_' symbol in an object field to mean that the field has no name.


