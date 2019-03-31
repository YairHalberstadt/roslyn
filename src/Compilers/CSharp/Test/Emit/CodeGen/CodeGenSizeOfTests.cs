using System;
using Microsoft.CodeAnalysis.CSharp.Test.Utilities;
using Microsoft.CodeAnalysis.Test.Utilities;
using Xunit;

namespace Microsoft.CodeAnalysis.CSharp.UnitTests.CodeGen
{
    public class CodeGenSizeOfTests : EmitMetadataTestBase
    {
        [Fact]
        public void SizeOfConstant()
        {
            var text = @"
using System;

class C
{
    static void Main()
    {
        Console.WriteLine(sizeof(sbyte));
        Console.WriteLine(sizeof(byte));
        Console.WriteLine(sizeof(short));
        Console.WriteLine(sizeof(ushort));
        Console.WriteLine(sizeof(int));
        Console.WriteLine(sizeof(uint));
        Console.WriteLine(sizeof(long));
        Console.WriteLine(sizeof(ulong));
        Console.WriteLine(sizeof(char));
        Console.WriteLine(sizeof(float));
        Console.WriteLine(sizeof(double));
        Console.WriteLine(sizeof(bool));
        Console.WriteLine(sizeof(decimal));
    }
}
";
            var expectedOutput = @"
1
1
2
2
4
4
8
8
2
4
8
1
16
".Trim();
            CompileAndVerify(text, expectedOutput: expectedOutput).VerifyIL("C.Main", @"
{
  // Code size       80 (0x50)
  .maxstack  1
  IL_0000:  ldc.i4.1
  IL_0001:  call       ""void System.Console.WriteLine(int)""
  IL_0006:  ldc.i4.1
  IL_0007:  call       ""void System.Console.WriteLine(int)""
  IL_000c:  ldc.i4.2
  IL_000d:  call       ""void System.Console.WriteLine(int)""
  IL_0012:  ldc.i4.2
  IL_0013:  call       ""void System.Console.WriteLine(int)""
  IL_0018:  ldc.i4.4
  IL_0019:  call       ""void System.Console.WriteLine(int)""
  IL_001e:  ldc.i4.4
  IL_001f:  call       ""void System.Console.WriteLine(int)""
  IL_0024:  ldc.i4.8
  IL_0025:  call       ""void System.Console.WriteLine(int)""
  IL_002a:  ldc.i4.8
  IL_002b:  call       ""void System.Console.WriteLine(int)""
  IL_0030:  ldc.i4.2
  IL_0031:  call       ""void System.Console.WriteLine(int)""
  IL_0036:  ldc.i4.4
  IL_0037:  call       ""void System.Console.WriteLine(int)""
  IL_003c:  ldc.i4.8
  IL_003d:  call       ""void System.Console.WriteLine(int)""
  IL_0042:  ldc.i4.1
  IL_0043:  call       ""void System.Console.WriteLine(int)""
  IL_0048:  ldc.i4.s   16
  IL_004a:  call       ""void System.Console.WriteLine(int)""
  IL_004f:  ret
}
");
        }

        [Fact]
        public void SizeOfNonConstant()
        {
            var text = @"
using System;

class C
{
    static void Main()
    {
        Console.WriteLine(sizeof(S));
        Console.WriteLine(sizeof(Outer.Inner));
        Console.WriteLine(sizeof(S1));
        unsafe
        {
            Console.WriteLine(sizeof(int*));
            Console.WriteLine(sizeof(void*));
        }
    }
}

struct S
{
    public byte b;
}

struct S1
{
    public string s;
}

class Outer
{
    public struct Inner
    {
        public char c;
    }
}
";
            bool isx86 = (IntPtr.Size == 4);
            string expectedOutput;

            if (isx86)
            {
                expectedOutput = @"
1
2
4
4
4
".Trim();
            }
            else
            {
                expectedOutput = @"
1
2
8
8
8
".Trim();
            }

            CompileAndVerify(text, options: TestOptions.UnsafeReleaseExe, expectedOutput: expectedOutput).VerifyIL("C.Main", @"
{
  // Code size       56 (0x38)
  .maxstack  1
  IL_0000:  sizeof     ""S""
  IL_0006:  call       ""void System.Console.WriteLine(int)""
  IL_000b:  sizeof     ""Outer.Inner""
  IL_0011:  call       ""void System.Console.WriteLine(int)""
  IL_0016:  sizeof     ""S1""
  IL_001c:  call       ""void System.Console.WriteLine(int)""
  IL_0021:  sizeof     ""int*""
  IL_0027:  call       ""void System.Console.WriteLine(int)""
  IL_002c:  sizeof     ""void*""
  IL_0032:  call       ""void System.Console.WriteLine(int)""
  IL_0037:  ret
}
");
        }

        [Fact]
        public void SizeOfEnum()
        {
            var text = @"
using System;

class C
{
    static void Main()
    {
        Console.WriteLine(sizeof(E1));
        Console.WriteLine(sizeof(E2));
        Console.WriteLine(sizeof(E3));
    }
}

enum E1 { A }
enum E2 : byte { A }
enum E3 : long { A }
";
            var expectedOutput = @"
4
1
8
".Trim();
            CompileAndVerify(text, expectedOutput: expectedOutput).VerifyIL("C.Main", @"
{
  // Code size       19 (0x13)
  .maxstack  1
  IL_0000:  ldc.i4.4
  IL_0001:  call       ""void System.Console.WriteLine(int)""
  IL_0006:  ldc.i4.1
  IL_0007:  call       ""void System.Console.WriteLine(int)""
  IL_000c:  ldc.i4.8
  IL_000d:  call       ""void System.Console.WriteLine(int)""
  IL_0012:  ret
}
");
        }

        [Fact]
        public void SizeOfTypeParameter()
        {
            var text = @"
using System;

class C
{
    static void Main()
    {
        PrintSize<int>();
        PrintSize<S1>();
        PrintSize<string>();
        PrintSize<I>();
    }

    static void PrintSize<T>() => Console.WriteLine(sizeof(T));
}

interface I {}

struct S1
{
    public string s;
}

";
            bool isx86 = (IntPtr.Size == 4);
            string expectedOutput;

            if (isx86)
            {
                expectedOutput = @"
4
4
4
4
".Trim();
            }
            else
            {
                expectedOutput = @"
4
8
8
8
".Trim();
            }

            CompileAndVerify(text, expectedOutput: expectedOutput).VerifyIL("C.Main", @"
{
  // Code size       21 (0x15)
  .maxstack  0
  IL_0000:  call       ""void C.PrintSize<int>()""
  IL_0005:  call       ""void C.PrintSize<S1>()""
  IL_000a:  call       ""void C.PrintSize<string>()""
  IL_000f:  call       ""void C.PrintSize<I>()""
  IL_0014:  ret
}
").VerifyIL("C.PrintSize<T>", @"
{
  // Code size       12 (0xc)
  .maxstack  1
  IL_0000:  sizeof     ""T""
  IL_0006:  call       ""void System.Console.WriteLine(int)""
  IL_000b:  ret
}
");
        }
    }
}
