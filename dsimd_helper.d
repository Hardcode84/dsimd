module dsimd_helper;

import core.simd;
import std.traits;



struct Vec(T, size_t Len)
{
pure:
nothrow:
@nogc:
    static assert(Len > 0);
    enum UseVec = Len == 2 || Len == 4 || Len == 8 || Len == 16;
    static if(UseVec)
    {
        alias VecT = Vector!(T[Len]);
    }
    else
    {
        alias VecT = T[Len];
    }
    alias Type = T;
    VecT data;

    enum length = Len;

    this(T val)
    {
        static if(UseVec)
        {
            T[Len] temp = val;
            data = loadUnaligned!VecT(temp.ptr);
        }
        else
        {
            data = val;
        }
    }

    this(VecT val)
    {
        data = val;
    }

    this(in T[] val)
    {
        assert(val.length == Len);
        static if(UseVec)
        {
            data = loadUnaligned!VecT(val.ptr);
        }
        else
        {
            data[] = val[0..Len];
        }
    }

    auto opBinary(string Op)(Vec val) inout
    {
        static if(UseVec)
        {
            mixin("VecT tmp = data "~Op~" val.data;");
        }
        else
        {
            mixin("VecT tmp = data[] "~Op~" val.data[];");
        }
        Vec ret = tmp[];
        return ret;
    }

    auto opUnary(string Op)() inout
    {
        static if(UseVec)
        {
            mixin("VecT tmp = "~Op~" data;");
        }
        else
        {
            mixin("VecT tmp = "~Op~" data[];");
        }
        Vec ret = tmp[];
        return ret;
    }

    auto opSlice() inout
    {
        return data[];
    }

    auto ref opIndex(size_t i) inout
    {
        assert(i < data.length);
        return data[i];
    }
}

auto gather(T, V)(in T[] data, in V indices)
{
    debug
    {
        foreach(ind; indices[])
        {
            assert(ind >= 0 && ind < data.length);
        }
    }
    enum Len = V.length;

    static if (V.UseVec)
    {
        return vec_gather(data, indices);
    }
    else
    {
        Vec!(T, V.length) ret = void;
        foreach(i; 0..Len)
        {
            ret[i] = data[indices[i]];
        }
        return ret;
    }
}

private:

version(LDC):
import ldc.simd;

string genGatherFunc(T, size_t Len)(string arg, string alignVal, string maskval, string defval)
{
    import std.format;
    enum Typename = llvmType!T;
    enum Typeletter = Typename[0..1];
    enum TypeSize = T.sizeof * 8;
    return "<%1$s x %2$s> @llvm.masked.gather.v%1$s%3$s%4$s.v%1$sp0%3$s%4$s(<%1$s x %2$s*> %8$s, i32 %5$s, <%1$s x i1> %6$s, <%1$s x %2$s> %7$s)".
    format(Len, Typename, Typeletter[0..1], TypeSize, alignVal, maskval, defval, arg);
}

string genGatherDecl(T, size_t Len)()
{
    import std.format;
    enum Typename = llvmType!T;
    enum Typeletter = Typename[0..1];
    enum Align = T.alignof;
    return "declare " ~ genGatherFunc!(T, Len)("", "", "", "");
}

string genGatherDef(T, IT, size_t Len)()
{
    import std.format;
    import std.range;
    import std.array;
    enum Typename = llvmType!T;
    enum Typeletter = Typename[0..1];
    enum Align = T.alignof;
    enum AlignStr = format("%s", Align);
    enum ITypename = llvmType!IT;
    enum string Mask = "<" ~ "i1 1".repeat(Len).join(", ") ~ ">";
    return `
    %%ptrs = getelementptr %2$s, %2$s* %%0, <%1$s x %3$s> %%1
    %%r = call %4$s
    ret <%1$s x %2$s> %%r`.format(Len, Typename, ITypename, genGatherFunc!(T, Len)("%ptrs" , AlignStr, Mask, "undef"));
}

pure:
nothrow:
@nogc:

pragma(LDC_inline_ir)
    R inlineIREx(string prefix, string code, string suffix, R, P...)(P);

auto vec_gather(T, V)(in T[] data, in V indices)
{
    enum Len = V.length;
    enum decl = genGatherDecl!(T,Len)();
    enum def = genGatherDef!(T, V.Type, Len)();
    alias RetT = Vec!(T, Len);
    static assert(RetT.UseVec);

    alias inlineIREx!(decl, def, "", RetT.VecT, const T*, V.VecT) g;
    return RetT(g(data.ptr, indices.data));
}

private template llvmType(T)
{
    static if(is(T == float))
        enum llvmType = "float";
    else static if(is(T == double))
        enum llvmType = "double";
    else static if(is(T == byte) || is(T == ubyte) || is(T == void))
        enum llvmType = "i8";
    else static if(is(T == short) || is(T == ushort))
        enum llvmType = "i16";
    else static if(is(T == int) || is(T == uint))
        enum llvmType = "i32";
    else static if(is(T == long) || is(T == ulong))
        enum llvmType = "i64";
    else
        static assert(0,
            "Can't determine llvm type for D type " ~ T.stringof);
}
