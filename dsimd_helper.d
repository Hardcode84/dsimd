module dsimd_helper;

import core.simd;
import ldc.simd;
import std.traits;

pure:
nothrow:
@nogc:

struct Vec(T, size_t Len)
{
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
}