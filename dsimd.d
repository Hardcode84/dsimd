module dsimd;

pure:

struct Dynamic;
struct Static(size_t L)
{
    enum Len = L;
}

auto load(LenT = Dynamic, T)(T[] data)
{
    static if(IsStaticLen!LenT)
    {
        assert(LenT.Len == data.length);
    }
    return LoadOp!(LenT, T)(data);
}

auto load(T, size_t Len)(ref T[Len] data)
{
    return LoadOp!(Static!Len, T)(data[]);
}

auto store(LenT = Dynamic, T)(T[] data)
{
    static if(IsStaticLen!LenT)
    {
        assert(LenT.Len == data.length);
    }
    return StoreProxy!(LenT, T)(data);
}

auto store(T, size_t Len)(ref T[Len] data)
{
    return StoreProxy!(Static!Len, T)(data[]);
}

auto fill(T)(T val)
{
    return ConstOp!T(val);
}

private:
template CheckLenT(T)
{
    enum bool CheckLenT = is(T == Dynamic) || is(T == Static!I, size_t I);
}

template IsStaticLen(T)
{
    static assert(CheckLenT!T, format("Invalid len type %s", T.stringof));
    enum bool IsStaticLen = !is(T == Dynamic);
}

import std.format;
import std.traits;
import std.algorithm;
import std.typetuple;

import dsimd_helper;

struct LoadOp(LenT, T)
{
    T[] data;
    static if(IsStaticLen!LenT)
    {
        enum staticLen = true;
        enum length = LenT.Len;
    }
    else
    {
        enum staticLen = false;
        auto length() const { return data.length; }
    }

    static string codegenDecl(ref int i, string src)
    {
        return format("const data%s = getValue!(%s)(%s);\n", i, i, src);
    }
    static string codegenDef(ref int i, string src, string len)
    {
        return format("Vec!(Unqual!(%s),%s) val%s = data%s[%s..%s + %s];\n", T.stringof, len, i, i, src, src, len);
    }
    template GetIndex(size_t I)
    {
        enum GetIndex = I;
    }
    auto ref access(size_t I, size_t Counter)()
    {
        return data;
    }

    mixin Operators;
}

struct ConstOp(T)
{
    T Value;

    enum staticLen = true;
    enum length = size_t.max;

    static string codegenDecl(ref int i, string src)
    {
        return format("const const%s = getValue!(%s)(%s);\n", i, i, src);
    }
    static string codegenDef(ref int i, string src, string len)
    {
        return format("Vec!(Unqual!(%s),%s) val%s = const%s;\n", T.stringof, len, i, i);
    }
    template GetIndex(size_t I)
    {
        enum GetIndex = I;
    }
    auto ref access(size_t I, size_t Counter)()
    {
        return Value;
    }

    mixin Operators;
}

struct UnaryOp(PrevOp, string Op)
{
    PrevOp prev;

    alias staticLen = PrevOp.staticLen;
    static if(staticLen)
    {
        enum length = PrevOp.length;
    }
    else
    {
        auto length() const { return prev.length; }
    }

    static string codegenDecl(ref int i, string src)
    {
        const str = prev.codegenDecl(i, src);
        i++;
        return str;
    }
    static string codegenDef(ref int i, string src, string len)
    {
        const str = prev.codegenDef(i, src, len);
        const prv = i++;
        return str ~ format("auto val%s = %sval%s;\n", i, Op, prv);
    }
    template GetIndex(size_t I)
    {
        enum GetIndex = PrevOp.GetIndex!I + 1;
    }
    auto ref access(size_t I, size_t Counter)()
    {
        enum Ind = GetIndex!I;
        static if(Ind == Counter)
        {
            return data;
        }
        else
        {
            return prev.access!(I,Counter);
        }
    }

    mixin Operators;
}

struct BinaryOp(PrevOp1, PrevOp2, string Op)
{
    PrevOp1 prev1;
    PrevOp2 prev2;

    enum staticLen = PrevOp1.staticLen && PrevOp2.staticLen;
    static if(staticLen)
    {
        enum length = min(PrevOp1.length, PrevOp2.length);
    }
    else
    {
        auto length() const { return min(prev1.length, prev2.length); }
    }

    static string codegenDecl(ref int i, string src)
    {
        const str1 = prev1.codegenDecl(i, src);
        i++;
        const str2 = prev2.codegenDecl(i, src);
        i++;
        return str1 ~ str2;
    }
    static string codegenDef(ref int i, string src, string len)
    {
        const str1 = prev1.codegenDef(i, src, len);
        const prv1 = i++;
        const str2 = prev2.codegenDef(i, src, len);
        const prv2 = i++;
        return str1 ~ str2 ~ format("auto val%s = val%s %s val%s;\n", i, prv1, Op, prv2);
    }
    template GetIndex(size_t I)
    {
        enum GetIndex = PrevOp2.GetIndex!(PrevOp1.GetIndex!I + 1) + 1;
    }
    auto ref access(size_t I, size_t Counter)()
    {
        enum Ind1 = PrevOp1.GetIndex!I;
        enum Ind2 = PrevOp2.GetIndex!(Ind1 + 1);
        static assert(Counter <= Ind2);
        static if(Counter <= Ind1)
        {
            return prev1.access!(I,Counter);
        }
        else
        {
            return prev2.access!(Ind1 + 1,Counter);
        }
    }

    mixin Operators;
}

struct StoreOp(PrevOp, LenT, T)
{
    PrevOp prev;
    T[] data;

    static if(IsStaticLen!LenT)
    {
        enum staticLen = PrevOp.staticLen;
        static if(PrevOp.staticLen)
        {
            enum length = min(LenT.Len, PrevOp.length);
        }
        else
        {
            auto length() const { return min(LenT.Len, prev.length); }
        }
    }
    else
    {
        enum staticLen = false;
        auto length() const { return min(data.length, prev.length); }
    }

    static string codegenDecl(ref int i, string src)
    {
        const str = prev.codegenDecl(i, src);
        i++;
        return str ~ format("auto data%s = getValue!(%s)(%s);\n", i, i, src);
    }
    static string codegenDef(ref int i, string src, string len)
    {
        const str = prev.codegenDef(i, src, len);
        const prv = i++;
        return str ~ format("auto val%s = val%s;\ndata%s[%s..%s + %s] = val%s[];\n", i, prv, i, src, src, len, i);
    }
    template GetIndex(size_t I)
    {
        enum GetIndex = PrevOp.GetIndex!I + 1;
    }
    auto ref access(size_t I, size_t Counter)()
    {
        enum Ind = GetIndex!I;
        static if(Ind == Counter)
        {
            return data;
        }
        else
        {
            return prev.access!(I,Counter);
        }
    }

    mixin Operators;
}

mixin template Operators()
{
    enum SimdOpDefined = true;

    auto opUnary(string Op)() inout
    {
        return UnaryOp!(Unqual!(typeof(this)), Op)(this);
    }

    auto opBinary(string Op, T)(T val) inout if(is(typeof(T.SimdOpDefined)))
    {
        return BinaryOp!(Unqual!(typeof(this)), T, Op)(this, val);
    }

    auto opBinary(string Op, T)(T val) inout if(isNumeric!T)
    {
        auto op = ConstOp!(T)(val);
        return BinaryOp!(Unqual!(typeof(this)), typeof(op), Op)(this, op);
    }
}

auto ref getValue(size_t I, T)(ref T op)
{
    return op.access!(0, I)();
}

string codegenDecl(Op, string src)()
{
    int i = 0;
    return Op.codegenDecl(i, src);
}

string codegenDef(Op, string src, string len)()
{
    int i = 0;
    return Op.codegenDef(i, src, len);
}

string codegenFunc(Op, size_t len)()
{
    const decl = codegenDecl!(Op, "op");
    static if (Op.staticLen)
    {
        enum fullLen = Op.length;
        const rem = fullLen % len;
        if (0 == rem)
        {
            const str = codegenDef!(Op, "i", format("%s", len));
            return decl ~ `
for (size_t i = 0; i < %s; i += %s)
{
%s
}
            `.format(fullLen, len, str);
        }
        else
        {
            const str1 = codegenDef!(Op, "i", format("%s", len));
            const str2 = codegenDef!(Op, format("%s", fullLen - rem), format("%s", rem));
            return decl ~ `
for (size_t i = 0; i < %s; i += %s)
{
%s
}
%s
            `.format(fullLen - rem, len, str1, str2);
        }
    }
    else
    {
        const str1 = codegenDef!(Op, "i", format("%s", len));
        const str2 = codegenDef!(Op, "i", "1");
        return decl ~ `
const rem = op.length %% %s;
const len = op.length - rem;
for (size_t i = 0; i < len; i += %s)
{
%s
}
foreach (i; len..len + rem)
{
%s
}
        `.format(len, len, str1, str2);
    }
}

string codegen(size_t I, Op, size_t Len, string Attr)()
{
    return `
{
pragma(inline, true) void func%s(ref Op op) %s
{
%s
}
func%s(op);
}
    `.format(I, Attr, codegenFunc!(Op, Len), I);
}

struct StoreProxy(LenT, T)
{
    T[] data;

    auto opAssign(Op)(Op op)
    {
        auto store = StoreOp!(Op, LenT, T)(op, data);
        return Computation!(typeof(store))(store);
    }
}

template TupleRange(int from, int to) if (from <= to)
{
    static if (from >= to)
    {
        alias TupleRange = TypeTuple!();
    } 
    else
    {
        alias TupleRange = TypeTuple!(from, TupleRange!(from + 1, to));
    }
}

struct Computation(Op)
{
    Op op;
    bool deffered = true;

    this(this) @disable;

    pragma(inline, true)
    ~this()
    {
        if (deffered)
        {
            apply();
        }
    }

    struct DefaultSettings
    {
        enum VecSizes = [1,2,4,8,16];
        enum Dump = false;
    }

    pragma(inline, true)
    void apply(alias VecSize = 4, Settings = DefaultSettings)()
    {
        deffered = false;
        foreach(I; TupleRange!(0,Settings.VecSizes.length))
        {
            if (VecSize == Settings.VecSizes[I])
            {
                enum str = codegen!(I, Op, Settings.VecSizes[I], "")();
                static if(Settings.Dump)
                {
                    pragma(msg, str);
                }
                mixin(str);
                return;
            }
        }
        assert(false);
    }
}

unittest
{
    void testAssign(size_t VecSize, L1, L2)()
    {
        const int[] src = [1,2,3,4,5];
        int[] dst = [0,0,0,0,0];
        static if (0 == VecSize)
        {
            store!L1(dst) = load!L2(src);
        }
        else
        {
            (store!L1(dst) = load!L2(src)).apply!VecSize;
        }
        assert(src == dst);
    }
    void testAssignStatic(size_t VecSize, L1, L2)()
    {
        static if (is(L1 == Dynamic))
        {
            const int[] src = [1,2,3,4,5];
        }
        else
        {
            const int[5] src = [1,2,3,4,5];
        }

        static if (is(L2 == Dynamic))
        {
            int[] dst = [0,0,0,0,0];
        }
        else
        {
            int[5] dst = [0,0,0,0,0];
        }

        static if (0 == VecSize)
        {
            store!L1(dst) = load!L2(src);
        }
        else
        {
            (store!L1(dst) = load!L2(src)).apply!VecSize;
        }
        assert(src == dst);
    }
    void testFill(size_t VecSize, L1, L2, T)(T val)
    {
        T[] dst = [0,0,0,0,0];
        T[] result = [val,val,val,val,val];
        static if (0 == VecSize)
        {
            store!L1(dst) = fill(val);
        }
        else
        {
            (store!L1(dst) = fill(val)).apply!VecSize;
        }
        assert(result == dst);
    }
    void testUnary(size_t VecSize, L1, L2)()
    {
        const int[] src = [1,2,3,4,5];
        int[] dst = [0,0,0,0,0];
        int[] result = [-1,-2,-3,-4,-5];
        static if (0 == VecSize)
        {
            store!L1(dst) = -load!L2(src);
        }
        else
        {
            (store!L1(dst) = -load!L2(src)).apply!VecSize;
        }
        assert(result == dst);
    }
    void testBinary1(size_t VecSize, L1, L2)()
    {
        const int[] src = [1,2,3,4,5];
        int[] dst = [0,0,0,0,0];
        int[] result = [1 * 2, 2 * 2, 3 * 2, 4 * 2, 5 * 2];
        static if (0 == VecSize)
        {
            store!L1(dst) = load!L2(src) * 2;
        }
        else
        {
            (store!L1(dst) = load!L2(src) * 2).apply!VecSize;
        }
        assert(result == dst);
    }
    void testBinary2(size_t VecSize, L1, L2)()
    {
        const int[] src1 = [1,2,3,4,5];
        const int[] src2 = [6,7,8,9,10];
        int[] dst = [0,0,0,0,0];
        int[] result = [1 + (6  + 1) * 2,
                        2 + (7  + 1) * 2,
                        3 + (8  + 1) * 2,
                        4 + (9  + 1) * 2,
                        5 + (10 + 1) * 2];
        static if (0 == VecSize)
        {
            store!L1(dst) = load!L2(src1) + (load(src2) + 1) * 2;
        }
        else
        {
            (store!L1(dst) = load!L2(src1) + (load(src2) + 1) * 2).apply!VecSize;
        }
        assert(result == dst);
    }

    void test(size_t VecSize, L1, L2)()
    {
        testAssign!(VecSize, L1, L2);
        testAssignStatic!(VecSize, L1, L2);
        testFill!(VecSize, L1, L2)(5);
        testFill!(VecSize, L1, L2)(5.0f);
        testFill!(VecSize, L1, L2)(5.0);
        testUnary!(VecSize, L1, L2);
        testBinary1!(VecSize, L1, L2);
        testBinary2!(VecSize, L1, L2);
    }
    test!(0,Dynamic,Dynamic)();
    test!(0,Static!5,Dynamic)();
    test!(0,Dynamic,Static!5)();
    test!(0,Static!5,Static!5)();
    test!(4,Dynamic,Dynamic)();
    test!(4,Static!5,Dynamic)();
    test!(4,Dynamic,Static!5)();
    test!(4,Static!5,Static!5)();
    test!(8,Dynamic,Dynamic)();
    test!(8,Static!5,Dynamic)();
    test!(8,Dynamic,Static!5)();
    test!(8,Static!5,Static!5)();
}