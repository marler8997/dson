// DSC = DSON Compiler
import std.stdio  : File, stdout;
import std.file   : exists;
import std.conv   : text;
import std.format : format, formattedWrite;
import std.bitmanip : bitfields;

import std.stdio : stderr;

import dson;

void usage()
{
    import std.stdio : writeln;
    writeln("Usage: dsc <dson-file>");
}
int main(string[] args)
{
    import std.stdio : writeln, writefln;

    args = args[1..$];
    if(args.length == 0)
    {
        usage();
        return 0;
    }

    if(args.length > 1)
    {
        writefln("Error: too many command line arguments");
        return 1;
    }
    auto filename = args[0];

    if(!exists(filename))
    {
        writefln("Error: file '%s' does not exist", filename);
        return 1;
    }

    auto schemaCode = readFile(filename);
    scope namespace = new DsonNamespace(null);
    {
        auto parser = DsonSchemaParser(namespace, filename, schemaCode.ptr, 1);
        parser.parse();
    }

    writeln("/+");
    writeln(namespace);
    writeln("+/");
    writeln("import std.typecons : Unique;");
    writeln("import std.bigint   : BigInt;");
    writeln();
    writer.sink = &stdout.write!(const(char)[]);
    generateTypeDefinitions(namespace);
    return 0;
}

string readFile(const(char)[] filename)
{
    File file = File(filename, "rb");
    auto filesize = file.size();
    if(filesize+1 > size_t.max)
    {
        assert(0, text(filename, ": file is too large ", filesize, " > ", size_t.max));
    }
    auto contents = new char[cast(size_t)(filesize+1)]; // add 1 for '\0'
    auto readSize = file.rawRead(contents).length;
    assert(filesize == readSize, text("rawRead only read ", readSize, " bytes of ", filesize, " byte file"));
    contents[cast(size_t)filesize] = '\0';
    import std.stdio : writefln;
    //writefln("[DEBUG] read %s bytes from \"%s\"", filesize, filename);
    return cast(string)contents[0..$-1];
}

struct CodeWriter
{
    private void delegate(const(char)[]) sink;
    mixin(bitfields!(
        bool, "atNewline", 1,
        ubyte, ""        , 7));
    ushort depth;
    private void writeIndentIfAtNewline()
    {
        if(atNewline && depth > 0)
        {
            sink.formattedWrite("%*s",  depth*TabSpaceCount, "");
            atNewline = false;
        }
    }
    void write(const(char)[] str)
    {
        writeIndentIfAtNewline();
        sink(str);
        if(str.length > 0 && str[str.length-1] == '\n')
        {
            assert(0, "currently you cannot explicitly write with a string that has a newline, you can only add newlines using writeln or writefln");
            atNewline = true;
        }
        else
        {
            atNewline = false;
        }
    }
    void writef(T...)(const(char)[] fmt, T args)
    {
        writeIndentIfAtNewline();
        formattedWrite(sink, fmt, args);
        atNewline = false;
    }
    void writeln(const(char)[] str)
    {
        writeIndentIfAtNewline();
        sink(str);
        sink("\n");
        atNewline = true;
    }
    void writefln(T...)(const(char)[] fmt, T args)
    {
        writeIndentIfAtNewline();
        formattedWrite(sink, fmt, args);
        sink("\n");
        atNewline = true;
    }
}
__gshared CodeWriter writer;

void generateTypeDefinitions(DsonNamespace namespace)
{
    foreach(definition; namespace.definitions)
    {
        generateTypeDefinition(definition.name, definition.type);
    }
}

enum AccessModifier
{
    default_,
    private_,
}
string declPrefix(AccessModifier mod)
{
    with(AccessModifier)
    {
        final switch(mod)
        {
            case default_: return "";
            case private_: return "private ";
        }
    }
}

immutable Symbol[] dlangKeywords;
immutable Symbol stringSymbol;
immutable Symbol bigIntSymbol;
immutable Symbol underscoreSymbol;
static this()
{
    dlangKeywords = [
        Symbol.create("null"),
        Symbol.create("bool"),
        Symbol.create("true"),
        Symbol.create("false"),
        Symbol.create("struct"),
        Symbol.create("union"),
        Symbol.create("object"), // not actually a keyword, but you
                                 // can't create a type named 'object'
    ];
    stringSymbol = Symbol.create("string");
    bigIntSymbol = Symbol.create("BigInt");
    underscoreSymbol = Symbol.create("_");
}

Symbol dtype(DsonPrimitiveTypeTag primitive)
{
    with(DsonPrimitiveTypeTag)
    {
        final switch(primitive)
        {
            // todo: I could define a custom 'symbol' type that
            //       indicates a more restricted type of string
            case symbol: return stringSymbol;
            case string: return stringSymbol;
            case uinteger: return bigIntSymbol;
            case integer: return bigIntSymbol;
        }
    }
}

bool isDKeyword(Symbol symbol)
{
    foreach(keyword; dlangKeywords)
    {
        if(symbol == keyword) return true;
    }
    return false;
}
auto fmtTypeName(Symbol typeName)
{
    struct Formatter
    {
        Symbol typeName;
        final void toString(scope void delegate(const(char)[]) sink) const
        {
            sink(typeName.value);
            if(typeName.isDKeyword)
            {
                sink("_");
            }
        }
    }
    return Formatter(typeName);
}
auto fmtIdentifier(Symbol symbol)
{
    struct Formatter
    {
        Symbol symbol;
        final void toString(scope void delegate(const(char)[]) sink) const
        {
            sink(symbol.value);
            if(symbol.isDKeyword)
            {
                sink("_");
            }
        }
    }
    return Formatter(symbol);
}
// need to pass in the fieldType because if it is the same then you need to add the "_" postfix
auto fmtFieldName(Symbol fieldType, Symbol fieldName)
{
    struct Formatter
    {
        Symbol fieldType;
        Symbol fieldName;
        final void toString(scope void delegate(const(char)[]) sink) const
        {
            sink(fieldName.value);
            if(fieldName == fieldType)
            {
                if(fieldType.isDKeyword)
                {
                    sink("__");
                }
                else
                {
                    sink("_");
                }
            }
            else if(fieldName.isDKeyword)
            {
                sink("_");
            }
        }
    }
    return Formatter(fieldType, fieldName);
}

void generateTypeDefinition(Symbol symbol, DsonType type, AccessModifier accessModifier = AccessModifier.default_)
{
    scope typeGenerator = new DCodeTypeDefinitionGenerator(symbol, accessModifier);
    type.visit(typeGenerator);
}

class DCodeTypeDefinitionGenerator : IDsonAcceptor
{
    Symbol symbol;
    AccessModifier accessModifier;
    this(Symbol symbol, AccessModifier accessModifier = AccessModifier.default_)
    {
        this.symbol = symbol;
        this.accessModifier = accessModifier;
    }
    void visit(DsonNamespace)
    {
        assert(0, "not implemented");
    }
    void visit(DsonPrimitiveType type)
    {
        assert(0, "not implemented");
        //writefln("DsonPrimitiveType %s", type.tag);
    }
    void visit(DsonListType type)
    {

        /*
        if(type.elementTypeRef.isCycle)
        {
            assert(0, "cycles not implemented");
        }
        */
        writer.writefln("%sstruct %s", accessModifier.declPrefix, symbol.fmtTypeName);
        writer.writeln("{");
        {
            writer.depth++;
            scope(exit) writer.depth--;

            Symbol elementTypeName;
            {
                auto refType = type.elementType.asDsonReferenceByName();
                if(refType)
                {
                    elementTypeName = refType.definition.name;
                }
                else
                {
                    elementTypeName = Symbol.create("Element");
                    generateTypeDefinition(elementTypeName, type.elementType);
                }
            }
            writer.writefln("%s[] elements;", elementTypeName);
        }
        writer.writeln("}");

        /*
        if(type.elementTypeRef.symbol.isNull)
        {
            assert(0, "not implemented");
        }
        else
        {
            scope nextWriter = new DCodeTypeGenerator(type.elementTypeRef.symbol);
            type.elementTypeRef.type.visit(nextWriter);
        }
        */
    }
    void visit(DsonSetOfType type)
    {
        final switch(type.elementType.tag)
        {
            case DsonPrimitiveTypeTag.symbol:
            writer.writefln("enum %s", symbol.fmtTypeName);
            writer.writeln("{");
            {
                writer.depth++;
                scope(exit) writer.depth--;
                foreach(value; type.values)
                {
                    writer.writefln("%s,", value.symbol.fmtIdentifier);
                }
            }
            writer.writeln("}");
            break;
            case DsonPrimitiveTypeTag.string: assert(0, "not implemented");
            case DsonPrimitiveTypeTag.uinteger: assert(0, "not implemented");
            case DsonPrimitiveTypeTag.integer: assert(0, "not implemented");
        }
    }
    void visit(DsonTokenType type)
    {
        assert(0, "not implemented");
        //writefln("DsonTokenType");
    }
    void visit(DsonObjectType type)
    {
        writer.writefln("%sstruct %s", accessModifier.declPrefix, symbol.fmtTypeName);
        writer.writeln("{");
        {
            writer.depth++;
            scope(exit) writer.depth--;
            foreach(field; type.fields)
            {
                scope fieldGenerator = new DCodeFieldGenerator(type, field.name);
                field.type.visit(fieldGenerator);
            }
        }
        writer.writeln("}");
    }
    void visit(DsonUnionType type)
    {
        writer.writefln("%sstruct %s", accessModifier.declPrefix, symbol.fmtTypeName);
        writer.writeln("{");
        {
            writer.depth++;
            scope(exit) writer.depth--;

            writer.writeln("enum Tag");
            writer.writeln("{");
            {
                writer.depth++;
                scope(exit) writer.depth--;
                foreach(field; type.fields)
                {
                    writer.writefln("%s,", field.name.fmtIdentifier);
                }
            }
            writer.writeln("}");
            writer.writeln("Tag tag;");
            writer.writeln("union");
            writer.writeln("{");
            {
                writer.depth++;
                scope(exit) writer.depth--;
                foreach(field; type.fields)
                {
                    scope fieldGenerator = new DCodeFieldGenerator(type, field.name);
                    field.type.visit(fieldGenerator);
                }
            }
            writer.writeln("}");
        }
        writer.writeln("}");
    }
    void visit(DsonReferenceByName type)
    {
        writer.writefln("alias %s = %s;", symbol, type.definition.name);
    }
}


class DCodeFieldGenerator : IDsonAcceptor
{
    DsonContainerType parentType;
    Symbol symbol;
    this(DsonContainerType parentType, Symbol symbol)
    {
        this.parentType = parentType;
        this.symbol = symbol;
    }
    void visit(DsonNamespace)
    {
        assert(0, "not implemented");
    }
    void visit(DsonPrimitiveType type)
    {
        writer.writefln("%s %s;",
            type.tag.dtype, fmtFieldName(type.tag.dtype, symbol));
    }
    void visit(DsonListType type)
    {
        Symbol typeSymbol = Symbol.create(format("%s_list", symbol));
        generateTypeDefinition(typeSymbol, type);
        writer.writefln("%s %s;",
            typeSymbol, fmtFieldName(typeSymbol, symbol));
    }
    void visit(DsonSetOfType type)
    {
        writer.writeln("// TODO: this is a setof, need to implement the restricted set of values");
        writer.writefln("%s %s;",
            type.elementType.tag.dtype, fmtFieldName(type.elementType.tag.dtype, symbol));
    }
    void visit(DsonTokenType type)
    {
        // TODO: if this is an object (not a union) and the
        //       token is optional, then this should be a bool
        if(symbol != underscoreSymbol)
        {
            writer.writefln("static struct %s_token { }", symbol);
            writer.writefln("%s_token %s;", symbol, fmtFieldName(Symbol.null_, symbol));
        }
    }
    void visit(DsonObjectType type)
    {
        Symbol typeSymbol = Symbol.create(format("%s_object", symbol));
        generateTypeDefinition(typeSymbol, type);
        writer.writefln("%s %s;",
            typeSymbol, fmtFieldName(typeSymbol, symbol));
    }
    void visit(DsonUnionType type)
    {
        Symbol typeSymbol = Symbol.create(format("%s_union", symbol));
        generateTypeDefinition(typeSymbol, type);
        writer.writefln("%s %s;",
            typeSymbol, fmtFieldName(typeSymbol, symbol));
    }
    void visit(DsonReferenceByName type)
    {
        // need to check if the reference has a cycle to
        // the parent type. If it does then the field must be
        // a reference instead of an inline struct.  The correct type
        // of reference would be the c++ unique_ptr, not sure about the D equivalent.
        bool foundCycle = checkForCycle(parentType, type);

        if(foundCycle)
        {
            writer.writef("Unique!(%s)", type.definition.name.fmtTypeName);
        }
        else
        {
            type.definition.name.fmtTypeName.toString(&writer.write);
        }
        writer.writefln(" %s;", fmtFieldName(type.definition.name, symbol));
    }
}

bool checkForCycle(DsonContainerType containerType, DsonReferenceByName typeReference)
{
    //stderr.writeln("--------------------------");
    //stderr.writefln("checkForCycle on '%s'", typeReference.definition.name);
    scope cycleChecker = new DCodeCycleChecker(containerType);
    typeReference.definition.type.visit(cycleChecker);
    //stderr.writefln("foundCycle = %s", cycleChecker.foundCycle);
    //stderr.writeln("--------------------------");
    return cycleChecker.foundCycle;
}

class DCodeCycleChecker : IDsonAcceptor
{
    DsonContainerType containerType;
    Symbol symbol;
    bool foundCycle;
    this(DsonContainerType containerType)
    {
        this.containerType = containerType;
    }
    void visit(DsonNamespace)
    {
        assert(0, "not implemented");
    }
    void visit(DsonPrimitiveType type)
    {
        // Primitive types cannot be containers,
        // so no need to check if(type is containerType)
    }
    void visit(DsonListType type)
    {
        if(type is containerType)
        {
            foundCycle = true;
            return;
        }
        type.elementType.visit(this);
    }
    void visit(DsonSetOfType type)
    {
        // Currently SetOf can only contain primitive types
        // which cannot be containers,
        // so no need to check if(type is containerType)
    }
    void visit(DsonTokenType type)
    {
        // Token types cannot be containers,
        // so no need to check if(type is containerType)
    }

    void visit(DsonObjectType type)
    {
        if(type is containerType)
        {
            foundCycle = true;
            return;
        }
        foreach(field; type.fields)
        {
            field.type.visit(this);
            if(foundCycle)
            {
                return;
            }
        }
    }
    void visit(DsonUnionType type)
    {
        if(type is containerType)
        {
            foundCycle = true;
            return;
        }
        foreach(field; type.fields)
        {
            field.type.visit(this);
            if(foundCycle)
            {
                return;
            }
        }
    }
    void visit(DsonReferenceByName type)
    {
        if(type.definition.type is containerType)
        {
            foundCycle = true;
            return;
        }
        return; // don't follow references when looking for cycles
    }
}
