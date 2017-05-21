DSON - Domain-Specific Schema Object Notation
================================================================================
DSON is a schema for creating "domain specific" data-interchange formats. DSON can be used to parse/process a domain-specific format or generate code for other programs to do so.  DSON is quite powerful in that it can define a wide variety of domain-specific formats. In fact, it's even powerfull enough to define it's own format.

DSON is influenced by the JSON format in that it ultimately provides a syntax to represent general data structures, but that syntax can be customized to fit the application along with some built in type checking.  Furthermore, since DSON is well-defined, it allows any tool that understands DSON to understand any applications customized syntax.

> Note: the term "target format" refers to the format that DSON is defining.

The extension used for files in the target format is up to the application.  The DSON file that defines this target format should be named "<target-format-extension>.dson", i.e. if the target format file extension is ".mycoolformat", then the dson file defining that format should be named "mycoolformat.dson".

A DSON file consists of a list of named types/definition of the form `name : type`.  For example, the DSON file to define a format that consists only of signed integers would look like this:
```
TheIntegerLanguage : root list integer;
```

In this example we are defining the name `TheIntegerLanguage` to be the root type of our format, and it's a list of signed integers.  A DSON file can have only 1 root type.

There are various kinds of definitions, and some of them will reference other type definitions.  All DSON files have access to the following primitive types:

| type | description  |
|------|--------------|
|`symbol`             |a symbol in the target syntax. It can be thought of as unquoted string that's limited to certain characters.|
|`string`             |a string in the target syntax.  it can be single or double qouted.
|`integer`            |a signed integer with no range restriction|
|`uinteger`           |an unsigned integer with no range restriction|

The `enum` keyword is used to define a set of values that can be used in the target format.  It must be followed by a type that indicates what kind of values it accepts, and a list of values delimited by curly braces and commas, i.e.
```
season  : enum symbol {summer, spring, fall, winter}
hour    : enum uinteger {1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12}
```

The `list` keyword is used to define a 'list' of some type.  It can be followed by a `list delimiter set` and must be followed by a type, i.e.
```
list symbol                  // i.e. foo bar baz
list () string              // i.e. ("hello" 'world')
list (,) uinteger           // i.e. (0, 200, 1)
list [|] symbol              // i.e. [apple | pear | tree]
list |/| enum symbol {a,b,c} // i.e. |c/a  /b/c/ c/a/ b/ a/ c|
```
> Note that depending on where the list is used, certain delimiters could introduce ambiguity in the target format.

### Custom Objects
The `object` keyword is used to define a custom object format. Let's look at an example:
```
person : object
{
    name : string
    age : uinteger
    eyeColor : enum symbol {brown,blue,green}
}
```
This defines an object that the target format can write like this:
```
person
{
    name : "Bob Jones"
    age : 42
    eyeColor : green
}
```
In some cases it might make sense to make an object field "nameless".  This means the field is pulled out of the object's curly-brace section and written before it, like so:
```
person2 : object
{
    name : nameless string
    age : nameless uinteger
    eyeColor : enum symbol {brown,blue,green}
}
// The Target Format:
person2 "Bob Jones" 42
{
    eyeColor:green
}
```
The nameless fields are ordered so must appear in the same order in both the schema and the target format.  In some cases it might make sense to make all the fields nameless.
```
person3 : object
{
    name : nameless string
    age : nameless uinteger
    eyeColor : nameless enum symbol {brown,blue,green}
}
// The Target Format:
person3 "Bob Jones" 42 green
```
In the previous examples we used the name of the object to create one in the target format.  However, when you have a list of objects, there's no need to specify the type every time.  In this case the name of the object is ommitted, i.e.
```
group : list [] person3
// The Target Format:
group [
  "Bob Jones"   42 green
  "Sally Smith" 37 brown
  "Tom Taylor"  24 blue
]
```
The `terminator` modifier can be used to specify a termination type.
```
person4 : object
{
    name : nameless string
    age : nameless uinteger
    eyeColor : nameless enum symbol {brown,blue,green}
    _ : terminator ';'
}
// The Target Format:
person4 "Bob Jones" 42 green;
```
The 'delimited' modifier is used to say that a field uses a delimiter, i.e.
```
person5 : object
{
    name : nameless string
    age : delimited 'age' uinteger
    eyeColor : delimited 'eyesAre' enum symbol {brown,blue,green}
    _ : terminator ';'
}
// The Target Format:
person5 "Bob Jones" age 42 eyesAre green;
```
Here's an example with a self-reference:
```
person : object
{
    name : nameless string
    friends : terminator list {} person
}
ThePersonLanguage : root list person
```
Here's an example of this language
```
"Bob Jones"
{
    "Sally Smith" {}
    "Jenny Jefferson" {}
    "Tom Tailor"
    {
        "Andrew Appleton" {}
        "Bobby June" {}
    }
}
```


