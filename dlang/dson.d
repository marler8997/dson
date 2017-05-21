module dson;

import core.stdc.string : strlen, memcmp;

import std.stdio     : File, writeln, writefln, stdout;
import std.format    : format, formattedWrite;
import std.array     : Appender, appender;
import std.typecons  : Flag, Yes, No, Rebindable, rebindable;
import std.range     : only;
import std.bigint    : BigInt;
import std.bitmanip  : bitfields;
import std.experimental.allocator;

import utf8;

enum TabSpaceCount = 3;


interface IDsonAcceptor
{
    void visit(DsonNamespace);
    void visit(DsonPrimitiveType);
    void visit(DsonListType);
    void visit(DsonSetOfType);
    void visit(DsonTokenType);
    void visit(DsonObjectType);
    void visit(DsonUnionType);
    void visit(DsonReferenceByName);
}

alias depth_t = ushort;

struct DsonWriter
{
    private void delegate(const(char)[]) sink;
    mixin(bitfields!(
        bool, "atNewline", 1,
        ubyte, ""        , 7));
    void tabIfNewline(depth_t depth)
    {
        if(atNewline)
        {
            sink.formattedWrite("%*s",  depth*TabSpaceCount, "");
            atNewline = false;
        }
    }
    void newline()
    {
        if(!atNewline)
        {
            sink("\n");
            atNewline = true;
        }
    }
    void newlineAndTab(depth_t depth)
    {
        newline();
        tabIfNewline(depth);
    }
    void write(const(char)[] str)
    {
        sink(str);
        if(str.length > 0 && str[str.length-1] == '\n')
        {
            atNewline = true;
        }
        else
        {
            atNewline = false;
        }

    }
}

class DsonType
{
    abstract void visit(IDsonAcceptor acceptor);
    final override bool opEquals(Object o)
    {
        assert(0, "CodeBug: Do not use == to compare DsonTypes");
    }
    abstract bool opEquals2(const(DsonType) other) const;
    inout(DsonReferenceByName) asDsonReferenceByName() inout { return null; }
    inout(DsonUnresolved) asDsonUnresolved() inout { return null; }
    void resolveSubTypes()
    {
    }
    bool findCycle(bool[DsonContainerType]* checked, DsonContainerType containerType)
    {
        return false; // A normal DsonType cannot be a DsonContainerType
    }

    @property abstract bool endsWithBrace() const;
    abstract void writeTo(DsonWriter* writer, depth_t depth) const;
    final void toString(scope void delegate(const(char)[]) sink) const
    {
        DsonWriter writer = DsonWriter(sink);
        writeTo(&writer, 0);
    }
}


alias subtypeidx = ushort;

// Means the DsonType can contain type references
class DsonContainerType : DsonType
{
    //@property abstract ushort typeReferenceCount() const;
    //@property abstract inout(DsonTypeRef)* typeReference(ushort index) inout;

    @property abstract subtypeidx subtypeCount() const;
    abstract inout(DsonType) opIndex(subtypeidx index) inout;
    abstract void opIndexAssign(DsonType type, subtypeidx index);


    /*
    final void visitTypeRefs(IDsonAcceptor acceptor)
    {
        ushort subTypeCount = typeReferenceCount();
        foreach(ushort i; 0..subTypeCount)
        {
            auto typeRef = typeReference(i);

            if(!typeRef.isCycle)
            {
                typeRef.type.visit(acceptor);
            }
        }
    }
    */


    final override void resolveSubTypes()
    {
        foreach(subtypeidx i; 0..subtypeCount)
        {
            auto subtype = this[i];
            auto unresolved = subtype.asDsonUnresolved();
            if(unresolved)
            {
                this[i] = unresolved.resolve();
                // NOTE: no need to call resolve on the resolved type, it's a
                //       name reference which means it's defined in some namespace and
                //       resolveSubTypes will be called from there.
            }
            else
            {
                subtype.resolveSubTypes();
            }
        }
    }

    final bool subTypesAreEqual(const(DsonContainerType) other) const
    {
        subtypeidx cachedSubTypeCount = subtypeCount;
        if(cachedSubTypeCount != other.subtypeCount)
        {
            return false;
        }
        foreach(subtypeidx i; 0..cachedSubTypeCount)
        {
            if(!fieldAtEquals(other, i))
            {
                return false;
            }
            if(!this[i].equals(other[i]))
            {
                return false;
            }
            /*
            auto typeRef = typeReference(i);
            auto otherTypeRef = other.typeReference(i);

            if(typeRef.isCycle != otherTypeRef.isCycle)
            {
                return false;
            }
            if(typeRef.symbol != otherTypeRef.symbol)
            {
                return false;
            }
            if(!typeRef.isCycle)
            {
                if(!typeRef.type.equals(otherTypeRef.type))
                {
                    return false;
                }
            }
            */
        }
        return true;
    }
    abstract bool fieldAtEquals(const(DsonContainerType) other, subtypeidx i) const;

    /+
    final override bool findCycle(bool[DsonContainerType]* checked, DsonContainerType containerType)
    {
        //writefln("findCycle %s", *checked);
        if(checked.get(this, false))
        {
            return false;
        }

        if(this is containerType)
        {
            //writeln("findCycle -- return true");
            return true;
        }
        (*checked)[this] = true;
        ushort subTypeCount = typeReferenceCount();
        //writefln("findCycle -- sub type count is %s", subTypeCount);
        foreach(ushort i; 0..subTypeCount)
        {
            //writefln("findCycle -- checking sub type %s", i);
            auto typeRef = typeReference(i);
            if(typeRef.type.findCycle(checked, containerType))
            {
                return true;
            }
        }
        return false;
    }
    +/
}

bool equals(const(DsonType) left, const(DsonType) right)
{
    if(left is null)
    {
        return right is null;
    }
    return left.opEquals2(right);
}

/+
struct DsonTypeRef
{
    DsonType type;
    Symbol symbol;
    mixin(bitfields!(
        bool, "isKeyword", 1,
        bool, "isCycle"  , 1,
        ubyte, ""        , 6));
    this(DsonType type, Symbol symbol = Symbol.null_)
    {
        this.type = type;
        this.symbol = symbol;
    }
    this(DsonType type, Symbol symbol, Flag!"isKeyword" isKeyword)
    {
        this.type = type;
        this.symbol = symbol;
        this.isKeyword = isKeyword;
    }
    this(DsonType type, Symbol symbol, Flag!"isCycle" isCycle)
    {
        this.type = type;
        this.symbol = symbol;
        this.isCycle = isCycle;
    }
    @property auto formatSymbol() const
    {
        struct Formatter
        {
            Symbol symbol;
            bool isKeyword;
            final void toString(scope void delegate(const(char)[]) sink) const
            {
                if(isKeyword)
                {
                    sink("@");
                }
                sink(symbol.value);
            }
        }
        return Formatter(symbol, isKeyword);
    }
}
+/

/*
### Dson Namespaces

A DsonNamespace is just a symbol table.  Anything you define inside the namespace
gets added to the namespaces symbol table.  Anything defined in the global
namespace gets added to the global symbol table.

Symbols in a namespace can have different types of access such as:

private: cannot access outside the namespace
protected: only spies can access outside the namespace
public: anyone can access, but outside the namespace the name must
        be fully-qualified with the namespace.

#### Namespace Spy

Definitions can mark them selves as "spies" on other namespaces.  A spy
can see and access all the symbols in the namespace being spied upon, it also
does not need to qualify these symbols with the namespace.

A definition can also spy on multiple namespaces.

Note: other ideas for public/protected/private;

public/open/free   :  accessible to anyone
protected/obscured : only accesible to spies
private/hidden/secret:    not spyable

*/

// Note, instead of a DsonSchema containing a map, it should probably just
// have a DsonNamespace, or better yet BE a DsonNamespace.

// NOTE: I think a DsonNamespace is really a DsonContainerType!!!
class DsonNamespace : DsonContainerType
{
    enum InitialGlobalArraySize = 100;
    enum InitialScopedArraySize = 40;

    DsonNamespace parent;

    //DsonNameDefinition[Symbol] nameTable;
    DsonNameDefinition[] definitionBuffer;
    subtypeidx definitionCount;
    @property inout(DsonNameDefinition)[] definitions() inout
    {
        return definitionBuffer[0..definitionCount];
    }

    this(DsonNamespace parent)
    {
        this.parent = parent;
    }
    final override void visit(IDsonAcceptor acceptor)
    {
        acceptor.visit(this);
    }

    final override bool opEquals2(const(DsonType) other) const
    {
        auto casted = cast(DsonNamespace)other;
        return casted && equals(casted);
    }
    final bool equals(const(DsonNamespace) other) const
    {
        if(definitionCount != other.definitionCount)
        {
            return false;
        }
        foreach(i; 0..definitionCount)
        {
            auto thisDef = definitionBuffer[i];
            auto otherDef = other.definitionBuffer[i];
            if(!thisDef.equals(otherDef))
            {
                return false;
            }
        }
        return true;
    }

    @property final override subtypeidx subtypeCount() const
    {
        return definitionCount;
    }
    final override inout(DsonType) opIndex(subtypeidx index) inout
    {
        return definitionBuffer[index].type;
    }
    final override void opIndexAssign(DsonType type, subtypeidx index)
    {
        definitionBuffer[index].type = type;
    }

    inout(DsonNameDefinition) lookup(Symbol name) inout
    {
        foreach(definition; definitions)
        {
            if(name == definition.name)
            {
                return definition;
            }
        }
        return null;
    }
    // returns: false if a definition with the same name already exists
    bool add(DsonSchemaParser* parser, DsonNameDefinition definition)
    {
        auto existing = lookup(definition.name);
        if(existing !is null)
        {
            return false; // fail
        }
        if(definitionBuffer is null)
        {
            assert(definitionCount == 0);
            size_t initialSize = (parent is null) ? InitialGlobalArraySize : InitialScopedArraySize;
            definitionBuffer = parser.objectAllocator.makeArray!DsonNameDefinition(initialSize);
            if(definitionBuffer is null)
            {
                assert(0, "makeArray failed");
            }
        }
        else
        {
            if(definitionCount >= definitionBuffer.length)
            {
                auto newLength = definitionBuffer.length * 2;
                if(newLength > subtypeidx.max)
                {
                    if(definitionBuffer.length >= subtypeidx.max)
                    {
                        assert(0, "namespace has too many types!");
                    }
                    newLength = subtypeidx.max;
                }
                if(!parser.objectAllocator.expandArray(definitionBuffer, newLength, null))
                {
                    assert(0, "expandArray failed");
                }
            }
        }
        definitionBuffer[definitionCount++] = definition;
        return true; // success
    }

    final override bool fieldAtEquals(const(DsonContainerType) other, ushort i) const
    {
        // I don't like this cast, not sure the best way to get rid of it
        auto otherNamespace = cast(const(DsonNamespace))other;
        return definitionBuffer[i].name == otherNamespace.definitionBuffer[i].name;
    }
    @property final override bool endsWithBrace() const
    {
        return true;
    }
    final override void writeTo(DsonWriter* writer, depth_t depth) const
    {
        if(parent !is null)
        {
            assert(0, "non global namespace not fully implemented");
        }

        foreach(definition; definitions)
        {
            definition.writeTo(writer, depth);
            writer.newline();
        }
    }
}
// NOTE: should this really be a container type like list?
// NOTE: when resolving sub types, do not recurse into DsonReferenceByName
//       why?
//       1. it can cause an infinite recursive loop
//       2. it is unnecessary because the reference will have resolved
//          through a namespace name table and that name table will
//          be resolved anyway.
class DsonNameDefinition
{
    DsonNamespace namespace;
    Symbol name;
    DsonType type;
    private DsonReferenceByName cachedRefType;
    this(DsonNamespace namespace, Symbol name, DsonType type)
    {
        this.namespace = namespace;
        this.name = name;
        this.type = type;
    }
    final override bool opEquals(Object o)
    {
        assert(0, "CodeBug: Do not use == to compare DsonTypes");
    }
    final bool equals(const(DsonNameDefinition) other) const
    {
        return name == other.name && type.equals(other.type);
    }
    @property DsonReferenceByName refType()
    {
        if(cachedRefType is null)
        {
            cachedRefType = new DsonReferenceByName(this);
        }
        return cachedRefType;
    }
    final void writeTo(DsonWriter* writer, depth_t depth) const
    {
        writer.tabIfNewline(depth);
        writer.sink(name.value);
        writer.sink(" : ");
        type.writeTo(writer, depth);
    }
}

class DsonReferenceByName : DsonType
{
    DsonNameDefinition definition;
    this(DsonNameDefinition definition)
    {
        this.definition = definition;
    }
    final override inout(DsonReferenceByName) asDsonReferenceByName() inout { return this; }
    final override void visit(IDsonAcceptor acceptor)
    {
        acceptor.visit(this);
    }
    final override bool opEquals2(const(DsonType) other) const
    {
        auto casted = cast(DsonReferenceByName)other;
        return casted && equals(casted);
    }
    final bool equals(const(DsonReferenceByName) other) const
    {
        return definition.equals(other.definition);
    }
    @property final override bool endsWithBrace() const
    {
        return false;
    }
    final override void writeTo(DsonWriter* writer, depth_t depth) const
    {
        writer.tabIfNewline(depth);
        writer.sink(definition.name.value);
    }
}

enum DsonPrimitiveTypeTag
{
    symbol,
    string,
    uinteger,
    integer,
}
struct DsonPrimitiveTypeInfo
{
    static immutable DsonPrimitiveTypeInfo[DsonPrimitiveTypeTag.max+1] table = [
        DsonPrimitiveTypeTag.symbol           : immutable DsonPrimitiveTypeInfo("symbol"),
        DsonPrimitiveTypeTag.string           : immutable DsonPrimitiveTypeInfo("string"),
        DsonPrimitiveTypeTag.uinteger         : immutable DsonPrimitiveTypeInfo("uinteger"),
        DsonPrimitiveTypeTag.integer          : immutable DsonPrimitiveTypeInfo("integer"),
    ];
    static assert(() {
        foreach (typeInfo; table)
            assert(typeInfo.symbolString.length);
        return true;
    }());
    string symbolString;
    private this(string symbolString) immutable
    {
        this.symbolString = symbolString;
    }
}
class DsonPrimitiveType : DsonType
{
    __gshared static immutable symbol   = new immutable DsonPrimitiveType(DsonPrimitiveTypeTag.symbol);
    __gshared static immutable string_  = new immutable DsonPrimitiveType(DsonPrimitiveTypeTag.string);
    __gshared static immutable integer  = new immutable DsonPrimitiveType(DsonPrimitiveTypeTag.integer);
    __gshared static immutable uinteger = new immutable DsonPrimitiveType(DsonPrimitiveTypeTag.uinteger);

    DsonPrimitiveTypeTag tag;
    private this(DsonPrimitiveTypeTag tag) immutable
    {
        this.tag = tag;
    }
    final override void visit(IDsonAcceptor acceptor)
    {
        acceptor.visit(this);
    }

    final override bool opEquals2(const(DsonType) other) const
    {
        auto casted = cast(DsonPrimitiveType)other;
        return casted && equals(casted);
    }
    final bool equals(const(DsonPrimitiveType) other) const
    {
        return this.tag == other.tag;
    }

    @property final override bool endsWithBrace() const
    {
        return false;
    }
    final override void writeTo(DsonWriter* writer, depth_t depth) const
    {
        writer.tabIfNewline(depth);
        writer.sink(DsonPrimitiveTypeInfo.table[tag].symbolString);
    }

    final void delegate(DsonValue*) getParser(DsonSchemaParser* parser) const
    {
        final switch(tag)
        {
            case DsonPrimitiveTypeTag.symbol: return &parser.parseSymbolValue;
            case DsonPrimitiveTypeTag.string: return &parser.parseStringValue;
            case DsonPrimitiveTypeTag.uinteger: return &parser.parseUintegerValue;
            case DsonPrimitiveTypeTag.integer: return &parser.parseIntegerValue;
        }
    }
    final bool valueEquals(const(DsonValue)* left, const(DsonValue)* right) const
    {
        final switch(tag)
        {
            case DsonPrimitiveTypeTag.symbol:
                return left.symbol == right.symbol;
            case DsonPrimitiveTypeTag.string:
                return left.string_ == right.string_;
            case DsonPrimitiveTypeTag.uinteger:
            case DsonPrimitiveTypeTag.integer:
                return left.bigint == right.bigint;
        }
    }
    final void valueToString(const(DsonValue)* value, DsonWriter* writer, depth_t depth) const
    {
        writer.tabIfNewline(depth);
        final switch(tag)
        {
            case DsonPrimitiveTypeTag.symbol:
                writer.write(value.symbol.value);
                break;
            case DsonPrimitiveTypeTag.string:
                writer.write(value.string_);
                break;
            case DsonPrimitiveTypeTag.uinteger:
            case DsonPrimitiveTypeTag.integer:
                value.bigint.toString(&writer.write, "");
                break;
        }
    }
}


struct ListDelimiterSet
{
    dchar start = 0;
    dchar element = 0;
    dchar end = 0;
}
class DsonListType : DsonContainerType
{
    ListDelimiterSet delimiterSet;

    DsonType elementType;

    this(ListDelimiterSet delimiterSet, DsonType elementType)
    {
        this.delimiterSet = delimiterSet;
        this.elementType = elementType;
    }
    this(DsonType elementType)
    {
        this.elementType = elementType;
    }
    final override void visit(IDsonAcceptor acceptor)
    {
        acceptor.visit(this);
    }
    @property final override subtypeidx subtypeCount() const { return 1; }
    final override inout(DsonType) opIndex(subtypeidx index) inout
    {
        assert(index == 0, "code bug");
        return elementType;
    }
    final override void opIndexAssign(DsonType type, subtypeidx index)
    {
        assert(index == 0, "code bug");
        elementType = type;
    }
    final override bool opEquals2(const(DsonType) other) const
    {
        auto casted = cast(DsonListType)other;
        return casted && equals(casted);
    }
    final bool equals(const(DsonListType) other) const
    {
        return
            this.delimiterSet.start   == other.delimiterSet.start &&
            this.delimiterSet.element == other.delimiterSet.element &&
            this.delimiterSet.end     == other.delimiterSet.end &&
            subTypesAreEqual(other);
    }
    final override bool fieldAtEquals(const(DsonContainerType) other, ushort i) const
    {
        assert(i == 0, "code bug");
        return true; // nothing more to compare
    }
    @property final override bool endsWithBrace() const
    {
        return elementType.endsWithBrace;
    }
    final override void writeTo(DsonWriter* writer, depth_t depth) const
    {
        writer.tabIfNewline(depth);
        writer.sink("list ");
        if(delimiterSet.start)
        {
            if(delimiterSet.element)
            {
                writer.sink.formattedWrite("%s%s%s ",
                    escape(delimiterSet.start),
                    escape(delimiterSet.element),
                    escape(delimiterSet.end));
            }
            else
            {
                writer.sink.formattedWrite("%s%s ",
                    escape(delimiterSet.start),
                    escape(delimiterSet.end));
            }
        }
        else if(delimiterSet.element)
        {
            writer.sink.formattedWrite("%s ",
                escape(delimiterSet.element));
        }
        elementType.writeTo(writer, depth);
        /*
        if(elementTypeRef.isCycle)
        {
            elementTypeRef.formatSymbol.writeTo(sink);
        }
        else
        {
            elementType.writeTo(writer, depth);
        }
        */
    }
}

union DsonValue
{
    Symbol symbol;
    string string_;
    BigInt bigint;
    this(Symbol symbol)
    {
        this.symbol = symbol;
    }
    this(string string_)
    {
        this.string_ = string_;
    }
    this(BigInt bigint)
    {
        this.bigint = bigint;
    }
}

class DsonSetOfType : DsonType
{
    const(DsonPrimitiveType) elementType;
    DsonValue[] values;
    this(const(DsonPrimitiveType) elementType, DsonValue[] values)
    {
        this.elementType = elementType;
        this.values = values;
    }
    final override void visit(IDsonAcceptor acceptor)
    {
        acceptor.visit(this);
    }
    final override bool opEquals2(const(DsonType) other) const
    {
        auto casted = cast(DsonSetOfType)other;
        return casted && equals(casted);
    }
    final bool equals(const(DsonSetOfType) other) const
    {
        if(!elementType.equals(other.elementType))
        {
            return false;
        }
        if(values.length != other.values.length)
        {
            return false;
        }
        foreach(i; 0..values.length)
        {
            if(!elementType.valueEquals(&values[i], &other.values[i]))
            {
                return false;
            }
        }
        return true;
    }
    @property final override bool endsWithBrace() const
    {
        return true;
    }
    final override void writeTo(DsonWriter* writer, depth_t depth) const
    {
        writer.tabIfNewline(depth);
        writer.write("setof ");
        // TODO: maybe print the values in newlines if
        //       they are too large
        elementType.writeTo(writer, depth);
        writer.write(" {");
        foreach(i, value; values)
        {
            if(i > 0) writer.write(", ");
            elementType.valueToString(&value, writer, depth);
        }
        writer.write("}");
    }
}

class DsonTokenType : DsonType
{
    const(char)[] tokenString;
    this(const(char)[] tokenString)
    {
        this.tokenString = tokenString;
    }
    final override void visit(IDsonAcceptor acceptor)
    {
        acceptor.visit(this);
    }
    final override bool opEquals2(const(DsonType) other) const
    {
        auto casted = cast(DsonTokenType)other;
        return casted && equals(casted);
    }
    final bool equals(const(DsonTokenType) other) const
    {
        return tokenString == other.tokenString;
    }
    @property final override bool endsWithBrace() const
    {
        return false;
    }
    final override void writeTo(DsonWriter* writer, depth_t depth) const
    {
        writer.tabIfNewline(depth);
        writer.write("token \"");
        escape(tokenString).toString(&writer.write);
        writer.write("\"");
    }
}

enum ObjectFieldFlag
{
    none       = 0x00,
    optional   = 0x01,
    preblock   = 0x02,
    postblock  = 0x04,
}
struct DsonObjectField
{
    Symbol name;
    ObjectFieldFlag flags;
    string delimited;
    DsonType type;
}
struct DsonObjectFieldModifiers
{
    ObjectFieldFlag flags;
    string delimited;
}
class DsonObjectType : DsonContainerType
{
    bool useblock;
    DsonObjectField[] fields;
    this(bool useblock, DsonObjectField[] fields)
    {
        this.useblock = useblock;
        this.fields = fields;
    }
    final override void visit(IDsonAcceptor acceptor)
    {
        acceptor.visit(this);
        //visitTypeRefs(acceptor);
    }

    @property final override subtypeidx subtypeCount() const
    {
        assert(fields.length <= subtypeidx.max, "subtypeidx is too small to hold number of subtypes");
        return cast(subtypeidx)fields.length;
    }
    final override inout(DsonType) opIndex(subtypeidx index) inout
    {
        return fields[index].type;
    }
    final override void opIndexAssign(DsonType type, subtypeidx index)
    {
        fields[index].type = type;
    }
    final override bool opEquals2(const(DsonType) other) const
    {
        auto casted = cast(DsonObjectType)other;
        return casted && equals(casted);
    }
    final bool equals(const(DsonObjectType) other) const
    {
        return subTypesAreEqual(other);
    }
    final override bool fieldAtEquals(const(DsonContainerType) other, ushort i) const
    {
        // I don't like this cast, not sure the best way to get rid of it
        auto otherObject = cast(const(DsonObjectType))other;
        return
            fields[i].name      == otherObject.fields[i].name &&
            fields[i].flags     == otherObject.fields[i].flags &&
            fields[i].delimited == otherObject.fields[i].delimited;
    }
    @property final override bool endsWithBrace() const
    {
        return true;
    }
    final override void writeTo(DsonWriter* writer, depth_t depth) const
    {
        writer.tabIfNewline(depth);
        writer.write("object ");
        if(useblock) writer.write("useblock ");
        if(fields.length == 0)
        {
            writer.write("{}");
        }
        else
        {
            writer.newlineAndTab(depth);
            writer.write("{");
            depth++;
            foreach(field; fields)
            {
                writer.newlineAndTab(depth);
                writer.write(field.name.value);
                writer.write(" : ");
                if((field.flags & ObjectFieldFlag.optional)) writer.write("optional ");
                if((field.flags & ObjectFieldFlag.preblock)) writer.write("preblock ");
                if((field.flags & ObjectFieldFlag.postblock)) writer.write("postblock ");
                if(field.delimited !is null)
                {
                    writer.write("delimited \"");
                    writer.write(field.delimited);
                    writer.write("\" ");
                }
                field.type.writeTo(writer, depth);
                if(!field.type.endsWithBrace)
                {
                    writer.write(";");
                }
                /*
                if(field.typeRef.isCycle)
                {
                    field.typeRef.formatSymbol.writeTo(sink);
                    sink(";");
                }
                else
                {
                    field.typeRef.type.writeTo(sink, false, depth);
                    if(!field.typeRef.type.endsWithBrace)
                    {
                        sink(";");
                    }
                }
                */
            }
            depth--;
            writer.newlineAndTab(depth);
            writer.write("}");
            writer.newline();
        }
    }
}
struct DsonUnionField
{
    Symbol name;
    //DsonTypeRef typeRef;
    DsonType type;
}
class DsonUnionType : DsonContainerType
{
    DsonUnionField[] fields;
    this(DsonUnionField[] fields)
    {
        this.fields = fields;
    }
    final override void visit(IDsonAcceptor acceptor)
    {
        acceptor.visit(this);
        //visitTypeRefs(acceptor);
    }
    @property final override subtypeidx subtypeCount() const
    {
        assert(fields.length <= subtypeidx.max, "subtypeidx is too small to hold number of subtypes");
        return cast(subtypeidx)fields.length;
    }
    final override inout(DsonType) opIndex(subtypeidx index) inout
    {
        return fields[index].type;
    }
    final override void opIndexAssign(DsonType type, subtypeidx index)
    {
        fields[index].type = type;
    }
    final override bool opEquals2(const(DsonType) other) const
    {
        auto casted = cast(DsonUnionType)other;
        return casted && equals(casted);
    }
    final bool equals(const(DsonUnionType) other) const
    {
        return subTypesAreEqual(other);
    }
    final override bool fieldAtEquals(const(DsonContainerType) other, ushort i) const
    {
        // I don't like this cast, not sure the best way to get rid of it
        auto otherUnion = cast(const(DsonUnionType))other;
        return fields[i].name == otherUnion.fields[i].name;
    }
    @property final override bool endsWithBrace() const
    {
        return true;
    }
    final override void writeTo(DsonWriter* writer, depth_t depth) const
    {
        writer.tabIfNewline(depth);
        writer.write("union ");
        if(fields.length == 0)
        {
            writer.write("{}");
        }
        else
        {
            writer.newlineAndTab(depth);
            writer.write("{");
            depth++;
            foreach(field; fields)
            {
                writer.newlineAndTab(depth);
                writer.write(field.name.value);
                writer.write(" : ");

                field.type.writeTo(writer, depth);
                if(!field.type.endsWithBrace)
                {
                    writer.write(";");
                }
                /*
                if(field.typeRef.isCycle)
                {
                    field.typeRef.formatSymbol.writeTo(sink);
                    sink(";");
                }
                else
                {
                    field.typeRef.type.writeTo(sink, false, depth);
                    if(!field.typeRef.type.endsWithBrace)
                    {
                        sink(";");
                    }
                }
                */
            }
            depth--;
            writer.newlineAndTab(depth);
            writer.write("}");
            writer.newline();
        }
    }
}

struct ZString
{
    char* ptr;
    alias ptr this;
    this(char[] str)
    {
        assert(str.ptr[str.length] == '\0', "ZString was created with a non-null-terminated string");
        this.ptr = str.ptr;
    }
    this(string str) immutable
    {
        assert(str.ptr[str.length] == '\0', "ZString was created with a non-null-terminated string");
        this.ptr = str.ptr;
    }
}

class DsonUnresolved : DsonType
{
    DsonSchemaParser* parser;
    DsonNamespace namespace;
    uint lineNumber;
    Symbol symbol;
    Flag!"isKeyword" isKeyword;

    this(DsonSchemaParser* parser, DsonNamespace namespace, uint lineNumber,
        Symbol symbol, Flag!"isKeyword" isKeyword)
    {
        this.parser = parser;
        this.namespace = namespace;
        this.lineNumber = lineNumber;
        this.symbol = symbol;
        this.isKeyword = isKeyword;
    }
    final override void visit(IDsonAcceptor acceptor)
    {
        assert(0, "You cannot visit an unresolved DsonType");
        //acceptor.visit(this);
    }
    final override inout(DsonUnresolved) asDsonUnresolved() inout
    {
        return this;
    }
    final override bool opEquals2(const(DsonType) other) const
    {
        auto casted = cast(DsonUnresolved)other;
        return casted && equals(casted);
    }
    final bool equals(const(DsonUnresolved) other) const
    {
        assert(0, "equals not implemented for this type");
    }

    final override void resolveSubTypes()
    {
        assert(0, "code bug: resolveSubTypes should never be called on DsonUnresolved");
    }

    /+
    struct ResolveStackEntry
    {
        DsonUnresolved unresolved;
        ResolveStackEntry* next;
        bool contains(DsonUnresolved unresolved)
        {
            for(ResolveStackEntry* entry = &this; ;entry = entry.next)
            {
                if(entry.unresolved is unresolved)
                {
                    return true;
                }
                if(entry.next is null)
                {
                    return false;
                }
            }
        }
        string buildCycleString(DsonUnresolved from)
        {
            if(unresolved is from || next is null)
            {
                return unresolved.symbol.value;
            }
            return next.buildCycleString(from) ~ " -> " ~ unresolved.symbol.value;
        }
    }
    private static DsonReferenceByName resolveName(ResolveStackEntry entry/*, DsonTypeRef* typeRef*/)
    {
        //writefln("[RESOLVE] '%s'", entry.unresolved.symbol);
        auto resolved = entry.unresolved.namespace.nameTable.get(entry.unresolved.symbol, null);
        if(resolved is null)
        {
            entry.unresolved.parser.throwError(Yes.semanticError, entry.unresolved.lineNumber,
                "unresolved symbol \"%s\"", entry.unresolved.symbol);
        }
        auto unresolved = resolved.type.asDsonUnresolved();
        if(!unresolved)
        {
            //typeRef.symbol = entry.unresolved.symbol;
            //typeRef.isKeyword = entry.unresolved.isKeyword;
            return resolved;
        }
        //writefln("  - IS DsonUnresolved");
        if(entry.contains(unresolved))
        {
            entry.unresolved.parser.throwError(Yes.semanticError, entry.unresolved.lineNumber,
                "circular reference detected (%s)",
                entry.buildCycleString(unresolved) ~ " -> " ~ unresolved.symbol.value);
        }
        return resolveName(ResolveStackEntry(unresolved, &entry)/*, typeRef*/);
    }
    +/
    final DsonReferenceByName resolve()
    {
        auto nameDef = namespace.lookup(symbol);
        if(nameDef is null)
        {
            parser.throwError(Yes.semanticError, lineNumber,
                "unresolved symbol \"%s\"", symbol);
        }
        return nameDef.refType;
    /+
        assert(typeRef.type == this, "code bug");
        typeRef.symbol = symbol;
        typeRef.isKeyword = isKeyword;
        DsonType resolved = resolveName(ResolveStackEntry(this, null), /*typeRef*/);
        if(containerType)
        {
            scope bool[DsonContainerType] checked;
            if(resolved.findCycle(&checked, containerType))
            {
                typeRef.isCycle = true;
            }
        }
        typeRef.type = resolved;
        +/
    }


    @property final override bool endsWithBrace() const
    {
        return false;
    }
    final override void writeTo(DsonWriter* writer, depth_t depth) const
    {
        writer.tabIfNewline(depth);
        if(isKeyword) writer.write("@");
        writer.write(symbol.value);
    }
}

private bool validSymbolFirstChar(dchar c)
{
    return
        (c >= 'a' && c <= 'z') ||
        (c >= 'A' && c <= 'Z') ||
        (c == '_');
}
private bool validSymbolChar(dchar c)
{
    return
        (c >= 'a' && c <= 'z') ||
        (c >= 'A' && c <= 'Z') ||
        (c >= '0' && c <= '9') ||
        (c == '_');
}
bool validDelimiterChar(dchar c)
{
    return
        c == '(' ||
        c == ')' ||
        c == '[' ||
        c == ']' ||
        c == '{' ||
        c == '}' ||
        c == ',' ||
        c == ';' ||
        c == '/' ;
}

// Null terminated unique string
struct Symbol
{
    static immutable Symbol symbol;
    static immutable Symbol string_;
    static immutable Symbol integer;
    static immutable Symbol uinteger;

    static immutable Symbol setof;
    static immutable Symbol list;
    static immutable Symbol object;
    static immutable Symbol union_;
    static immutable Symbol token;

    static immutable Symbol optional;
    static immutable Symbol preblock;
    static immutable Symbol delimited;
    static immutable Symbol postblock;

    static this()
    {
        symbol           = Symbol.create("symbol");
        string_          = Symbol.create("string");
        integer          = Symbol.create("integer");
        uinteger         = Symbol.create("uinteger");

        setof            = Symbol.create("setof");
        list             = Symbol.create("list");
        object           = Symbol.create("object");
        union_           = Symbol.create("union");
        token            = Symbol.create("token");

        optional         = Symbol.create("optional");
        preblock         = Symbol.create("preblock");
        delimited        = Symbol.create("delimited");
        postblock        = Symbol.create("postblock");
    }

    static __gshared Symbol[string] table;
    immutable(char)[] value;
    @property static Symbol null_() { return immutable Symbol(null); }
    private this(string value) immutable
    {
        this.value = value;
    }
    @property bool isNull() { return value.ptr == null; }
    size_t toHash() const @safe pure nothrow
    {
        return cast(size_t)value.ptr;
    }
    static immutable(Symbol) create(T)(T[] value)
        if( is( T == char ) || is( T == const char) || is( T == immutable char) )
    {
        {
            auto existing = table.get(cast(string)value, null_);
            if(existing.value !is null)
            {
                return existing;
            }
        }

        string valueString;
        static if( is( T == immutable char) )
        {
            valueString = value;
        }
        else
        {
            valueString = value.idup;
        }
        table[valueString] = immutable Symbol(valueString);
        return immutable Symbol(valueString);
    }
    static immutable(Symbol) getExisting(const(char)[] value)
    {
        return table.get(cast(string)value, null_);
    }
    final bool opEquals()(auto ref const Symbol other) const
    {
        return this.value.ptr == other.value.ptr;
    }
    void toString(scope void delegate(const(char)[]) sink) const
    {
        sink(value);
    }
}

unittest
{
    {
        auto a = Symbol.getExisting("a");
        assert(a.value is null);
    }
    {
        auto a = Symbol.create("a");
        assert(a.value == "a");
        auto a2 = Symbol.create("a");
        assert(a == a2);
        char[1] buffer;
        buffer[0] = 'a';
        auto a3 = Symbol.create(buffer);
        assert(a == a3);
        buffer[0] = 'b';
        assert(Symbol.create("b") == Symbol.create(buffer));
    }
}



class DsonSchemaParseException : Exception
{
    this(string errorMessage, string filename, uint lineNumber)
    {
        super(errorMessage, filename, lineNumber);
    }
}
class DsonSchemaSemanticException : Exception
{
    this(string errorMessage, string filename, uint lineNumber)
    {
        super(errorMessage, filename, lineNumber);
    }
}

struct DsonSchemaParser
{
    string filename; // used for error messages
    const(char)* schemaCode;
    uint lineNumber;
    DsonNamespace globalNamespace;

    //DsonNamespace currentNamespace;

    // Override the object allocator after the constructor if you need to
    IAllocator objectAllocator;

    const(char)* nextPtr;
    dchar current;
    const(char)* currentPtr;

    this(/*DsonSchema* schema, */DsonNamespace globalNamespace, string filename, const(char)* schemaCode,
        uint lineNumber = 1)
    {
        this.filename = filename;
        this.schemaCode = schemaCode;
        this.lineNumber = lineNumber;
        this.globalNamespace = globalNamespace;
        //this.currentNamespace = globalNamespace;

        this.objectAllocator = theAllocator;

        this.nextPtr = schemaCode;
    }

    Throwable throwParseError(T...)(string fmt, T args)
    {
        throw throwError(No.semanticError, lineNumber, fmt, args);
    }
    Throwable throwSemanticError(T...)(string fmt, T args)
    {
        throw throwError(Yes.semanticError, lineNumber, fmt, args);
    }
    Throwable throwError(T...)(Flag!"semanticError" semanticError, uint lineNumber, string fmt, T args)
    {
        if(semanticError)
            throw new DsonSchemaSemanticException(format(fmt, args), filename, lineNumber);
        else
            throw new DsonSchemaParseException(format(fmt, args), filename, lineNumber);
    }

    /*inline*/ void consumeChar()
    {
        currentPtr = nextPtr;
        current = decodeUtf8(&nextPtr);
    }

    static struct PeekedChar
    {
        dchar nextChar;
        const(char)* nextNextPtr;
    }

    // NOTE: only call if you know you are not currently pointing to the
    //       terminating NULL character
    const PeekedChar peek() in { assert(current != '\0'); } body
    {
        PeekedChar peeked;
        peeked.nextNextPtr = nextPtr;
        peeked.nextChar = decodeUtf8(&peeked.nextNextPtr);
        return peeked;
    }

    void parse()
    {
        // read the first character
        consumeChar();

        for(;;)
        {
            auto symbol = tryPeelSymbol();
            if(symbol.isNull)
            {
                if(current == '\0')
                {
                    break;
                }
                throwParseError("expected a symbol but got %s", escapedToken(currentPtr));
            }

            //writefln("[DEBUG] symbol '%s'", symbol.value);
            peel(':', "after definition name");

            auto definition = new DsonNameDefinition(globalNamespace, symbol, parseType());
            if(!globalNamespace.add(&this, definition))
            {
                throwSemanticError("global symbol '%s' was defined more than once", symbol);
            }
        }

        // TODO: if I implement more than global namespace
        // if(currentNamespace !is globalNamespace)
        //     throwSyntaxError("some namespaces were not ended");

        globalNamespace.resolveSubTypes();
    }

    void skipToNextLine()
    {
        for(;;)
        {
            auto c = decodeUtf8(&nextPtr);
            if(c == '\n')
            {
                lineNumber++;
                return;
            }
            if(c == '\0')
            {
                currentPtr = nextPtr;
                current = '\0';
                return;
            }
        }
    }
    void skipWhitespaceAndComments()
    {
        for(;;)
        {
            if(current == ' ' || current == '\t' || current == '\r')
            {
                //do nothing
            }
            else if(current == '\n')
            {
                lineNumber++;
            }
            else if(current == '/')
            {
                auto next = peek(); // Note: we know current != '\0'
                if(next.nextChar == '/')
                {
                    skipToNextLine();
                    if(current == '\0')
                    {
                        return;
                    }
                }
                else if(next.nextChar == '*')
                {
                    assert(0, "multiline comments not implemented");
                }
                else
                {
                    return; // not a whitespace or comment
                }
            }
            else
            {
                return; // not a whitespace or comment
            }
            consumeChar();
        }
    }

    Symbol tryPeelSymbol()
    {
        skipWhitespaceAndComments();
        if(!validSymbolFirstChar(current))
        {
            return Symbol.null_;
        }
        auto symbolStart = currentPtr;
        for(;;)
        {
            consumeChar();
            if(!validSymbolChar(current))
            {
                return Symbol.create(symbolStart[0..currentPtr-symbolStart]);
            }
        }
    }
    void peel(dchar c, string context)
    {
        skipWhitespaceAndComments();
        if(current != c)
        {
            throw throwParseError("expected \"%s\" %s but got %s", c, context, escapedToken(currentPtr));
        }
        consumeChar();
    }
    bool tryPeel(const(ZString) thing)
    {
        skipWhitespaceAndComments();
        size_t i;
        for(i = 0; thing[i] != '\0' ; i++)
        {
            if(currentPtr[i] != thing[i])
            {
                return false;
            }
        }
        nextPtr = &currentPtr[i+1];
        consumeChar();
        return true;
    }
    DsonType parseType()
    {
        return parseType(tryPeelSymbol());
    }
    DsonType parseType(Symbol firstSymbol)
    {
        if(firstSymbol == Symbol.list)
        {
            return parseListType();
        }
        if(firstSymbol == Symbol.object)
        {
            return parseObjectType();
        }
        if(firstSymbol == Symbol.setof)
        {
            return parseSetOf();
        }
        if(firstSymbol == Symbol.union_)
        {
            return parseUnion();
        }
        if(firstSymbol == Symbol.token)
        {
            auto tokenString = parseString();
            peel(';', "after token type");
            return new DsonTokenType(tokenString);
        }

        {
            auto primitiveType = tryAsPrimitiveType(firstSymbol);
            if(primitiveType !is null)
            {
                peel(';', "after primitive type");
                return primitiveType.unconst;
            }
        }

        if(firstSymbol.isNull)
        {
            // handle types references that are also keywords
            if(current == '@')
            {
                consumeChar();
                return escapedKeywordTypeReference(tryPeelSymbol());
            }

            throw throwParseError("expected a symbol but got %s", escapedToken(currentPtr));
        }
        else
        {
            peel(';', "after type reference");
            // Do not use the allocator for this, it should be removed by the end
            return new DsonUnresolved(&this, globalNamespace, lineNumber, firstSymbol, No.isKeyword);
        }
    }

    DsonUnresolved escapedKeywordTypeReference(Symbol symbol)
    {
        if(symbol.isNull)
        {
            throw throwParseError("expected a symbol after '@' but got %s", escapedToken(currentPtr));
        }
        else if(
            symbol == Symbol.setof   ||
            symbol == Symbol.list    ||
            symbol == Symbol.object  ||
            symbol == Symbol.union_  ||
            symbol == Symbol.token   )
        {
            peel(';', "after keyword-type reference");
            return new DsonUnresolved(&this, globalNamespace, lineNumber, symbol, Yes.isKeyword);
        }
        else
        {
            throw throwParseError("the '@' symbol must be followed by a keyword but got \"%s\"", symbol);
        }
    }

    const(DsonPrimitiveType) tryAsPrimitiveType(Symbol symbol)
    {
        if(symbol == Symbol.string_)
        {
            return DsonPrimitiveType.string_;
        }
        if(symbol == Symbol.symbol)
        {
            return DsonPrimitiveType.symbol;
        }
        if(symbol == Symbol.integer)
        {
            return DsonPrimitiveType.integer;
        }
        if(symbol == Symbol.uinteger)
        {
            return DsonPrimitiveType.uinteger;
        }
        return null;
    }

    // Assumption:  validDelimiterChar(current)
    // automatically skips whitespace and comments
    void parseDelimiterSet(ListDelimiterSet* delimiterSet)
    {
        auto delimiterStart = currentPtr;
        dchar[2] delimiters = void;
        delimiters[0] = current;
        consumeChar();

        if(validDelimiterChar(current))
        {
            delimiters[1] = current;
            consumeChar();
            if(validDelimiterChar(current))
            {
                delimiterSet.start = delimiters[0];
                delimiterSet.element = delimiters[1];
                delimiterSet.end = current;
                consumeChar();
                if(validDelimiterChar(current))
                {
                    throwParseError("delimiter set has too many characters %s", escapedToken(delimiterStart));
                }
            }
            else
            {
                delimiterSet.start = delimiters[0];
                delimiterSet.end = delimiters[1];
            }
        }
        else
        {
            delimiterSet.element = delimiters[0];
        }
    }

    void parseSymbolValue(DsonValue* value)
    {
        value.symbol = tryPeelSymbol();
        if(value.symbol.isNull)
        {
            throw throwParseError("expected a symbol but got %s", escapedToken(currentPtr));
        }
    }
    void parseStringValue(DsonValue* value)
    {
        assert(0, "parseStringValue not implemented");
    }
    void parseUintegerValue(DsonValue* value)
    {
        if(current < '0' || current > '9')
        {
            throw throwParseError("expected a uinteger but got %s", escapedToken(currentPtr));
        }
        auto start = currentPtr;
        for(;;)
        {
            consumeChar();
            if(current < '0' || current > '9')
            {
                break;
            }
        }
        value.bigint = BigInt(start[0..currentPtr-start]);
    }
    void parseIntegerValue(DsonValue* value)
    {
        assert(0, "parseIntegerValue not implemented");
    }

    const(char)[] parseString()
    {
        skipWhitespaceAndComments();
        if(current == '\'')
        {
            assert(0, "single quoted strings not implemented");
        }
        else if(current == '"')
        {
            auto start = currentPtr + 1;
            bool hasEscapes;
            for(;;)
            {
                consumeChar();
                if(current == '"')
                {
                    break;
                }
                if(current == '\\')
                {
                    assert(0, "string escapes not implemented");
                }
                if(current == '\n')
                {
                    throw throwParseError("newlines cannot be inside double-quoted strings");
                }
                if(current == '\0')
                {
                    throw throwParseError("file ended inside double-quoted string");
                }
            }
            if(hasEscapes)
            {
                assert(0, "string escapes not implemented");
            }
            auto value = start[0..currentPtr-start];
            consumeChar();
            return value;
        }
        else
        {
            throw throwParseError("expected a string but got %s", escapedToken(currentPtr));
        }
    }

    DsonSetOfType parseSetOf()
    {
        skipWhitespaceAndComments();
        Rebindable!(const DsonPrimitiveType) elementType;
        {
            auto elementTypeSymbol = tryPeelSymbol();
            if(elementTypeSymbol.isNull)
            {
                throw throwParseError("expected a symbol after setof but got %s", escapedToken(currentPtr));
            }
            elementType = tryAsPrimitiveType(elementTypeSymbol);
            if(elementType is null)
            {
                throw throwParseError("expected a primitive type symbol setof but got %s", elementTypeSymbol);
            }
        }
        peel('{', "after setof type");
        auto parser = elementType.getParser(&this);
        auto builder = appender!(DsonValue[])();
        for(;;)
        {
            skipWhitespaceAndComments();
            if(current == '}')
            {
                consumeChar();
                break;
            }
            if(builder.data.length > 0)
            {
                if(current != ',')
                {
                    throw throwParseError("expected a comma ',' to seperate setof elements but got %s", escapedToken(currentPtr));
                }
                consumeChar();
            }
            DsonValue value;
            parser(&value);
            builder.put(value);
        }
        auto values = objectAllocator.makeArray!DsonValue(builder.data.length);
        values[] = builder.data[];
        return new DsonSetOfType(elementType, values);
    }
    DsonListType parseListType()
    {
        skipWhitespaceAndComments();

        ListDelimiterSet delimiterSet;
        if(validDelimiterChar(current))
        {
            parseDelimiterSet(&delimiterSet);
            skipWhitespaceAndComments();
        }

        return objectAllocator.make!DsonListType(delimiterSet, parseType());
    }
    DsonObjectType parseObjectType()
    {
        bool useblock = tryPeel(immutable ZString("useblock"));
        skipWhitespaceAndComments();
        if(current != '{')
        {
            throwParseError("expected \"{\" to start object definition but got %s",
                escapedToken(currentPtr));
        }
        consumeChar();

        auto builder = appender!(DsonObjectField[])();
        builder.reserve(30); // just a guess
        for(;;)
        {
            skipWhitespaceAndComments();
            if(current == '}')
            {
                consumeChar();
                auto finalArray = objectAllocator.makeArray!DsonObjectField(builder.data.length);
                finalArray[] = builder.data[];
                return objectAllocator.make!DsonObjectType(useblock, finalArray);
            }

            Symbol fieldName = tryPeelSymbol();
            if(fieldName.isNull)
            {
                throwParseError("expected a symbol but got %s", escapedToken(currentPtr));
            }
            peel(':', "after field name");

            DsonObjectFieldModifiers modifiers;
            DsonType fieldType;
            {
                Symbol symbol;
                // parse modifiers
                for(;;)
                {
                    skipWhitespaceAndComments();
                    symbol = tryPeelSymbol();
                    if(!processObjectModifier(&modifiers, symbol))
                    {
                        break;
                    }
                }
                fieldType = parseType(symbol);
            }
            builder.put(DsonObjectField(fieldName, modifiers.flags,
                modifiers.delimited, fieldType));
        }
    }
    bool processObjectModifier(DsonObjectFieldModifiers* modifiers, Symbol symbol)
    {
        if(symbol == Symbol.optional)
        {
            if(modifiers.flags & ObjectFieldFlag.optional)
            {
                throwSemanticError("the 'optional' modifier appeared more than once");
            }
            modifiers.flags |= ObjectFieldFlag.optional;
            return true;
        }
        if(symbol == Symbol.preblock)
        {
            if(modifiers.flags & ObjectFieldFlag.preblock)
            {
                throwSemanticError("the 'preblock' modifier appeared more than once");
            }
            modifiers.flags |= ObjectFieldFlag.preblock;
            return true;
        }
        if(symbol == Symbol.postblock)
        {
            if(modifiers.flags & ObjectFieldFlag.postblock)
            {
                throwSemanticError("the 'postblock' modifier appeared more than once");
            }
            modifiers.flags |= ObjectFieldFlag.postblock;
            return true;
        }
        if(symbol == Symbol.delimited)
        {
            assert(0, "delimited modifier not implemented");
            return true;
        }
        return false; // not an object modifier
    }
    DsonUnionType parseUnion()
    {
        skipWhitespaceAndComments();
        if(current != '{')
        {
            throwParseError("expected \"{\" to start union definition but got %s",
                escapedToken(currentPtr));
        }
        consumeChar();

        auto builder = appender!(DsonUnionField[])();
        builder.reserve(30); // just a guess
        for(;;)
        {
            skipWhitespaceAndComments();
            if(current == '}')
            {
                consumeChar();
                auto finalArray = objectAllocator.makeArray!DsonUnionField(builder.data.length);
                finalArray[] = builder.data[];
                return objectAllocator.make!DsonUnionType(finalArray);
            }

            Symbol fieldName = tryPeelSymbol();
            if(fieldName.isNull)
            {
                throwParseError("expected a symbol but got %s", escapedToken(currentPtr));
            }
            peel(':', "after field name");
            auto fieldType = parseType();
            builder.put(DsonUnionField(fieldName, fieldType));
        }
    }
}

version(unittest)
{
    auto parseDelimiterSet(const(char)[] delimiterSetString)
    {
        if(delimiterSetString.length == 1)
        {
            return ListDelimiterSet('\0', delimiterSetString[0], '\0');
        }
        if(delimiterSetString.length == 2)
        {
            return ListDelimiterSet(delimiterSetString[0], '\0', delimiterSetString[1]);
        }
        if(delimiterSetString.length == 3)
        {
            return ListDelimiterSet(delimiterSetString[0], delimiterSetString[1], delimiterSetString[2]);
        }
        assert(0, format("a list delimiter string must have 1 to 3 characters but this one has %s", delimiterSetString.length));
    }
    // shorthand for Symbol.create
    alias sym = Symbol.create;
    auto n(T)(string s, T t)
    {
        return new DsonNameDefinition(null, Symbol.create(s), t.unconst);
    }
    auto list(string delimiters, const(DsonType) type)
    {
        return new DsonListType(parseDelimiterSet(delimiters), type.unconst);
    }
    auto list(const(DsonType) type)
    {
        return new DsonListType(type.unconst);
    }
    auto findByName(inout(DsonNameDefinition)[] nameDefinitions, Symbol name)
    {
        foreach(nameDef; nameDefinitions)
        {
            if(nameDef.name == name)
            {
                return nameDef;
            }
        }
        return null;
    }
}

unittest
{
    void testImpl(string schemaCode, const(DsonNameDefinition)[] expected, uint lineNumber)
    {
        writefln("test \"%s\"", escape(schemaCode)); stdout.flush();
        assert(schemaCode.ptr[schemaCode.length] == '\0', "test string does not end with NULL");

        scope parsedNamespace = new DsonNamespace(null);
        {
            auto parser = DsonSchemaParser(parsedNamespace, format("test_line_%s", lineNumber), schemaCode.ptr, 1);
            parser.parse();
        }
        foreach(expectedDefinition; expected)
        {
            auto actualDefinition = parsedNamespace.lookup(expectedDefinition.name);
            if(actualDefinition is null)
            {
                assert(0, format("test line %s: parsed schema is missing type: %s",
                    lineNumber, expectedDefinition.name));
            }
            if(!expectedDefinition.type.equals(actualDefinition.type))
            {
                writefln("expected: %s", expectedDefinition.type);
                writefln("actual  : %s", actualDefinition.type);
                assert(0, format("test line %s: symbol '%s' does not match expected", lineNumber, expectedDefinition.name));
            }
        }
        foreach(actualDefinition; parsedNamespace.definitions)
        {
            auto expectedMatch = expected.findByName(actualDefinition.name);
            if(expectedMatch is null)
            {
                assert(0, "parsed schema has extra types");
            }
        }
    }
    void test(uint lineNumber = __LINE__)(string schemaCode, const(DsonNameDefinition)[] expected...)
    {
        testImpl(schemaCode, expected, lineNumber);
    }
    void testError(string schemaCode, int lineNumber = __LINE__)
    {
        writefln("testError \"%s\"", escape(schemaCode)); stdout.flush();
        assert(schemaCode.ptr[schemaCode.length] == '\0', "test string does not end it NULL");
        scope namespace = new DsonNamespace(null);
        auto parser = DsonSchemaParser(namespace, format("test_line_%s", lineNumber), schemaCode.ptr, 1);
        try
        {
            parser.parse();
            assert(0, "expected DsonParseException but did not get one");
        }
        catch(DsonSchemaParseException e)
        {
            writefln("caught expected DsonSchemaParseException: %s", e.msg);
        }
        catch(DsonSchemaSemanticException e)
        {
            writefln("caught expected DsonSchemaSemanticException: %s", e.msg);
        }
    }

    test("", []);
    testError("!");
    testError("!!! foo");
    testError("foo");
    testError("foo : ");

    test("foo : symbol;", n("foo", DsonPrimitiveType.symbol));
    test("a : string;", n("a", DsonPrimitiveType.string_));
    test("bar : integer;", n("bar", DsonPrimitiveType.integer));

    testError("root : root c;"); // unresolved symbol

    testError(`root : root child;
               child : missingSymbol;`); // indirect unresolved symbol

    testError(`root : root child;
               child : grandchild;
               grandchild : missingSymbol;`); // double indirect unresolved symbol

    // multiple roots
    testError(`root1 : root string;
               root2 : root symbol;`);


    // circular reference cases
    testError(`a : root b;
               b : a;`);
    testError(`a : root b;
               b : c;
               c : a;`);
    testError(`a : root b;
               b : c;
               c : b;`);
    testError(`a : root b;
               b : c;
               c : d;
               d : b;`);

               /*
    {
        auto a = new DsonListType(null);
        a.elementType = new DsonNameDefinition(null, sym("b"), a);
        test(`a : root list b;`~
             `b : a;`, n("a", a));
    }
    {
        auto a = new DsonObjectType(false, [DsonObjectField(sym("_"))]);
        a.fields[0].typeRef = DsonTypeRef(a, sym("b"), Yes.isCycle);
        test(`a : root object`   ~
             `{`                 ~
             `    _ : b;`        ~
             `}`                 ~
             `b : a;`, n("a", a));
    }
    {
        auto a = new DsonUnionType([DsonUnionField(sym("_"))]);
        a.fields[0].typeRef = DsonTypeRef(a, sym("b"), Yes.isCycle);
        test(`a : root union`    ~
             `{`                 ~
             `    _ : b;`        ~
             `}`                 ~
             `b : a;`, n("a", a));
    }
    */

               /*
    {
        DsonObjectType ref1 = new DsonObjectType(false, [
            DsonObjectField(sym("a") , ObjectFieldFlag.none,
                null, DsonTypeRef(DsonPrimitiveType.string_.unconst)),
            DsonObjectField(sym("b"), ObjectFieldFlag.none,
                null, DsonTypeRef(DsonPrimitiveType.symbol.unconst)),
        ]);

        test(`theroot : root ref1;
              ref1 : object {
                  a : theroot;
                  b : ref2;
              }
              ref2 : string;`, null, sym("theroot"), ref1);
    }
    {
        DsonObjectType ref1 = new DsonObjectType(false, [
            DsonObjectField(sym("a") , ObjectFieldFlag.none,
                null, DsonTypeRef(DsonPrimitiveType.string_.unconst)),
            DsonObjectField(sym("b"), ObjectFieldFlag.none,
                null, DsonTypeRef(DsonPrimitiveType.symbol.unconst)),
        ]);

        test(`theroot : root ref1;
              ref1 : ref2;
              ref2 : object {
                  a : theroot;
                  b : ref3;
              }
              ref3 : string;`, null, sym("theroot"), ref1);
    }
    testError(`strings : root union
               {
                   string     : string;
                   stringlist : list strings;
               }`);
    */

    // forward reference
    {
        auto bar = n("bar", DsonPrimitiveType.string_);
        test(`foo : bar;
              bar : string;`, n("foo", bar.refType), bar);
    }

    //
    // token
    //
    testError(`bon : token`);
    testError(`bon : token;`);
    test(`bon : token "pop";`, n("bon", new DsonTokenType("pop")));

    //
    // setof
    //
    test(`season : setof symbol {summer,spring,winter,fall}`,
        n("season", new DsonSetOfType(DsonPrimitiveType.symbol, [
            DsonValue(sym("summer")),
            DsonValue(sym("spring")),
            DsonValue(sym("winter")),
            DsonValue(sym("fall"))
        ])));
    test(`hour : setof uinteger {1,2,3,4,5,6,7,8,9,10,11,12}`,
        n("hour", new DsonSetOfType(DsonPrimitiveType.uinteger, [
            DsonValue(BigInt(1)), DsonValue(BigInt( 2)), DsonValue(BigInt( 3)), DsonValue(BigInt( 4)),
            DsonValue(BigInt(5)), DsonValue(BigInt( 6)), DsonValue(BigInt( 7)), DsonValue(BigInt( 8)),
            DsonValue(BigInt(9)), DsonValue(BigInt(10)), DsonValue(BigInt(11)), DsonValue(BigInt(12)),
        ])));

    //
    // list
    //
    test(`foolist : list symbol;`,
        n("foolist", list(DsonPrimitiveType.symbol)));

    test(`csintegers : list , integer;`,
        n("csintegers", list(",", DsonPrimitiveType.integer)));

    test(`foolist2 : list (,) symbol;`,
        n("foolist2", list("(,)", DsonPrimitiveType.symbol)));

    test(`strings : list [] string;`,
        n("strings", list("[]", DsonPrimitiveType.string_)));
    test(`strings : list []string;`,
        n("strings", list("[]", DsonPrimitiveType.string_)));

    testError(`strings : list {,}(string`);

    //
    // object
    //
    testError(`baz : object`);
    testError(`baz : object {`);

    test(`baz : object {}`, n("baz", new DsonObjectType(false, null)));
    test(`foo : object
          {
              field1:string;
              field2 :symbol;
              field3: uinteger;
          }`, n("foo", new DsonObjectType(false, [
            DsonObjectField(sym("field1") , ObjectFieldFlag.none,
                null, DsonPrimitiveType.string_.unconst),
            DsonObjectField(sym("field2"), ObjectFieldFlag.none,
                null, DsonPrimitiveType.symbol.unconst),
            DsonObjectField(sym("field3"), ObjectFieldFlag.none,
                null, DsonPrimitiveType.uinteger.unconst),
          ])));

    //
    // union
    //
    testError(`foo : union`);
    testError(`foo : union {`);
    test(`foo : union{}`, n("foo", new DsonUnionType(null)));
    test(`fruits : union
         {
             a : token "apple";
             b : token "pear";
         }`, n("fruits", new DsonUnionType([
            DsonUnionField(sym("a"), new DsonTokenType("apple")),
            DsonUnionField(sym("b"), new DsonTokenType("pear")),
         ])));

    //
    // defining and using types that are the same as keywords
    //
    {
        auto setof = n("setof", DsonPrimitiveType.string_);
        test(`setof : string;
              root : @setof;`,
              n("root", setof.refType), setof);
    }
    {
        auto list = n("list", DsonPrimitiveType.string_);
        test(`list : string;
              root : @list;`,
              list, n("root", list.refType));
    }
    {
        auto object = n("object", DsonPrimitiveType.string_);
        test(`object : string;
              root : @object;`,
              object, n("root", object.refType));
    }
    {
        auto union_ = n("union", DsonPrimitiveType.string_);
        test(`union : string;
              root : @union;`,
              union_, n("root", union_.refType));
    }
    {
        auto token = n("token", DsonPrimitiveType.string_);
        test(`token : string;
              root : @token;`,
              token, n("root", token.refType));
    }
}


char hexchar(ubyte b) in { assert(b <= 0x0F); } body
{
    return cast(char)(b + ((b <= 9) ? '0' : ('A'-10)));
}


bool isUnreadable(dchar c) pure nothrow @nogc @safe
{
    return c < ' ' || (c > '~' && c < 256);
}

void formatUnreadable(scope void delegate(const(char)[]) sink, dchar c) in { assert(isUnreadable(c)); } body
{
    if(c == '\r') sink("\\r");
    else if(c == '\t') sink("\\t");
    else if(c == '\n') sink("\\n");
    else if(c == '\0') sink("\\0");
    else {
        char[4] buffer;
        buffer[0] = '\\';
        buffer[1] = 'x';
        buffer[2] = hexchar((cast(char)c)>>4);
        buffer[3] = hexchar((cast(char)c)&0xF);
        sink(buffer);
    }
}

void formatEscaped(scope void delegate(const(char)[]) sink, const(char)* ptr, const char* limit)
{
    auto flushPtr = ptr;

    void flush()
    {
        if(ptr > flushPtr)
        {
            sink(flushPtr[0..ptr-flushPtr]);
            flushPtr = ptr;
        }
    }

    for(; ptr < limit;)
    {
        const(char)* nextPtr = ptr;
        dchar c = decodeUtf8(&nextPtr);
        if(isUnreadable(c))
        {
            flush();
            sink.formatUnreadable(c);
        }
        ptr = nextPtr;
    }
    flush();
}

auto guessEndOfToken(const(char)* ptr)
{
    // TODO: should use the first token to determine the kind of token and
    //       then find the end using that information
    for(;;)
    {
        auto c = *ptr;
        if(c == '\0' || c == ' ' || c == '\t' || c == '\r' || c == '\n')
        {
            return ptr;
        }
        decodeUtf8(&ptr);
    }
}
auto escapedToken(const(char)* token)
{
    struct Formatter
    {
        const(char)* token;
        void toString(scope void delegate(const(char)[]) sink) const
        {
            if(*token == '\0')
            {
                sink("EOF");
            }
            else
            {
                sink("\"");
                sink.formatEscaped(token, guessEndOfToken(token));
                sink("\"");
            }
        }
    }
    return Formatter(token);
}
auto escape(const(char)[] str)
{
    struct Formatter
    {
        const(char)* str;
        const(char)* limit;
        void toString(scope void delegate(const(char)[]) sink) const
        {
            sink.formatEscaped(str, limit);
        }
    }
    return Formatter(str.ptr, str.ptr + str.length);
}

auto escape(dchar c)
{
    struct Formatter
    {
        char[4] buffer;
        ubyte size;
        this(dchar c)
        {
            size = encodeUtf8(buffer.ptr, c);
        }
        void toString(scope void delegate(const(char)[]) sink) const
        {
            sink.formatEscaped(buffer.ptr, buffer.ptr + size);
        }
    }
    return Formatter(c);
}
// D's const/immutable semantics are broken, this
// is a necessary hack for escaping them when necessary
T unconst(T)(const(T) obj)
{
  return cast(T)obj;
}
