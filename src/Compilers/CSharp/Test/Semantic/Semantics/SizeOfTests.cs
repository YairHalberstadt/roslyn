using System;
using System.Collections.Generic;
using System.Linq;
using Microsoft.CodeAnalysis.CSharp.Symbols;
using Microsoft.CodeAnalysis.CSharp.Syntax;
using Microsoft.CodeAnalysis.CSharp.Test.Utilities;
using Roslyn.Test.Utilities;
using Xunit;

namespace Microsoft.CodeAnalysis.CSharp.UnitTests
{
    public class SizeOfTests : CompilingTestBase
    {
        [Fact]
        public void SizeOfInIterator()
        {
            var text = @"
struct S
{
    System.Collections.Generic.IEnumerable<int> M()
    {
        yield return sizeof(S);
    }
}
";
            CreateCompilation(text).VerifyDiagnostics();
        }

        [Fact]
        public void SizeOfNonType1()
        {
            var text = @"
struct S
{
    void M()
    {
        S s = new S();
        int i = 0;

        i = sizeof(s);
        i = sizeof(i);
        i = sizeof(M);
    }
}
";
            // Not identical to Dev10, but same meaning.
            CreateCompilation(text).VerifyDiagnostics(
                // (9,20): error CS0118: 's' is a variable but is used like a type
                //         i = sizeof(s);
                Diagnostic(ErrorCode.ERR_BadSKknown, "s").WithArguments("s", "variable", "type"),
                // (10,20): error CS0118: 'i' is a variable but is used like a type
                //         i = sizeof(i);
                Diagnostic(ErrorCode.ERR_BadSKknown, "i").WithArguments("i", "variable", "type"),
                // (11,20): error CS0246: The type or namespace name 'M' could not be found (are you missing a using directive or an assembly reference?)
                //         i = sizeof(M);
                Diagnostic(ErrorCode.ERR_SingleTypeNameNotFound, "M").WithArguments("M"),
                // (6,11): warning CS0219: The variable 's' is assigned but its value is never used
                //         S s = new S();
                Diagnostic(ErrorCode.WRN_UnreferencedVarAssg, "s").WithArguments("s"));
        }

        [Fact]
        public void SizeOfNonType2()
        {
            var text = @"
struct S
{
    void M()
    {
        int i;
        i = sizeof(void);
        i = sizeof(this); //parser error
        i = sizeof(x => x); //parser error
    }
}
";
            // Not identical to Dev10, but same meaning.
            CreateCompilation(text).VerifyDiagnostics(
                // (7,20): error CS1547: Keyword 'void' cannot be used in this context
                //         i = sizeof(void);
                Diagnostic(ErrorCode.ERR_NoVoidHere, "void"),
                // (8,20): error CS1031: Type expected
                //         i = sizeof(this); //parser error
                Diagnostic(ErrorCode.ERR_TypeExpected, "this"),
                // (8,20): error CS1026: ) expected
                //         i = sizeof(this); //parser error
                Diagnostic(ErrorCode.ERR_CloseParenExpected, "this"),
                // (8,20): error CS1002: ; expected
                //         i = sizeof(this); //parser error
                Diagnostic(ErrorCode.ERR_SemicolonExpected, "this"),
                // (8,24): error CS1002: ; expected
                //         i = sizeof(this); //parser error
                Diagnostic(ErrorCode.ERR_SemicolonExpected, ")"),
                // (8,24): error CS1513: } expected
                //         i = sizeof(this); //parser error
                Diagnostic(ErrorCode.ERR_RbraceExpected, ")"),
                // (9,22): error CS1026: ) expected
                //         i = sizeof(x => x); //parser error
                Diagnostic(ErrorCode.ERR_CloseParenExpected, "=>"),
                // (9,22): error CS1002: ; expected
                //         i = sizeof(x => x); //parser error
                Diagnostic(ErrorCode.ERR_SemicolonExpected, "=>"),
                // (9,22): error CS1513: } expected
                //         i = sizeof(x => x); //parser error
                Diagnostic(ErrorCode.ERR_RbraceExpected, "=>"),
                // (9,26): error CS1002: ; expected
                //         i = sizeof(x => x); //parser error
                Diagnostic(ErrorCode.ERR_SemicolonExpected, ")"),
                // (9,26): error CS1513: } expected
                //         i = sizeof(x => x); //parser error
                Diagnostic(ErrorCode.ERR_RbraceExpected, ")"),
                // (9,20): error CS0246: The type or namespace name 'x' could not be found (are you missing a using directive or an assembly reference?)
                //         i = sizeof(x => x); //parser error
                Diagnostic(ErrorCode.ERR_SingleTypeNameNotFound, "x").WithArguments("x"),
                // (9,25): error CS0103: The name 'x' does not exist in the current context
                //         i = sizeof(x => x); //parser error
                Diagnostic(ErrorCode.ERR_NameNotInContext, "x").WithArguments("x"));
        }

        [Fact, WorkItem(529318, "http://vstfdevdiv:8080/DevDiv2/DevDiv/_workitems/edit/529318")]
        public void SizeOfNull()
        {
            string text = @"
class Program
{
    int F1 = sizeof(null);
}
";
            CreateCompilation(text).VerifyDiagnostics(
                // (4,21): error CS1031: Type expected
                //     int F1 = sizeof(null);
                Diagnostic(ErrorCode.ERR_TypeExpected, "null"),
                // (4,21): error CS1026: ) expected
                //     int F1 = sizeof(null);
                Diagnostic(ErrorCode.ERR_CloseParenExpected, "null"),
                // (4,21): error CS1003: Syntax error, ',' expected
                //     int F1 = sizeof(null);
                Diagnostic(ErrorCode.ERR_SyntaxError, "null").WithArguments(",", "null"));
        }

        [Fact]
        public void SizeOfInt()
        {
            string text = @"
class Program
{
    public int F1 = sizeof(int);
}
";
            CreateCompilation(text).VerifyDiagnostics();
        }

        [Fact]
        public void SizeOfUnmanagedStruct()
        {
            string text = @"
struct A {}
class Program
{
    int F1 = sizeof(A);
}
";
            CreateCompilation(text).VerifyDiagnostics();
        }

        [Fact]
        public void SizeOfManagedStruct()
        {
            string text = @"
struct A 
{
    public string B;
}
class Program
{
    public int F1 = sizeof(A);
}
";
            CreateCompilation(text).VerifyDiagnostics(
                // (4,19): warning CS0649: Field 'A.B' is never assigned to, and will always have its default value null
                //     public string B;
                Diagnostic(ErrorCode.WRN_UnassignedInternalField, "B").WithArguments("A.B", "null").WithLocation(4, 19));
        }

        [Fact]
        public void SizeOfObject()
        {
            string text = @"
class Program
{
    int F1 = sizeof(object);
}
";
            CreateCompilation(text).VerifyDiagnostics(
                // (4,14): error CS8424: 'sizeof(object)' is not allowed as 'object' is known to be a reference type.
                //     int F1 = sizeof(object);
                Diagnostic(ErrorCode.ERR_SizeOfReferenceType, "sizeof(object)").WithArguments("object").WithLocation(4, 14));
        }

        [Fact]
        public void SizeOfInterface()
        {
            string text = @"
using System;
class Program
{
    int F1 = sizeof(IComparable);
}
";
            CreateCompilation(text).VerifyDiagnostics(
                // (5,14): error CS8424: 'sizeof(IComparable)' is not allowed as 'IComparable' is known to be a reference type. Use 'System.IntPtr.Size' instead.
                //     int F1 = sizeof(IComparable);
                Diagnostic(ErrorCode.ERR_SizeOfReferenceType, "sizeof(IComparable)").WithArguments("System.IComparable").WithLocation(5, 14));
        }

        [Fact]
        public void SizeOfUnconstrainedTypeParameter()
        {
            string text = @"
class Program<T>
{
    int F1 = sizeof(T);
}
";
            CreateCompilation(text).VerifyDiagnostics();
        }

        [Fact]
        public void SizeOfTypeParameterConstrainedToStruct()
        {
            string text = @"
class Program<T> where T : struct
{
    int F1 = sizeof(T);
}
";
            CreateCompilation(text).VerifyDiagnostics();
        }

        [Fact]
        public void SizeOfTypeParameterConstrainedToClass()
        {
            string text = @"
class Program<T> where T : class
{
    int F1 = sizeof(T);
}
";
            CreateCompilation(text).VerifyDiagnostics(
                // (4,14): error CS8424: 'sizeof(T)' is not allowed as 'T' is known to be a reference type. Use 'System.IntPtr.Size' instead.
                //     int F1 = sizeof(T);
                Diagnostic(ErrorCode.ERR_SizeOfReferenceType, "sizeof(T)").WithArguments("T").WithLocation(4, 14));
        }

        [Fact]
        public void SizeOfTypeParameterConstrainedToInterface()
        {
            string text = @"
using System;
class Program<T> where T : IComparable
{
    int F1 = sizeof(T);
}
";
            CreateCompilation(text).VerifyDiagnostics();
        }

        [Fact]
        public void SizeOfTypeParameterConstrainedToManaged()
        {
            string text = @"
class Program<T> where T : struct
{
    int F1 = sizeof(T);
}
";
            CreateCompilation(text).VerifyDiagnostics();
        }

        [Fact]
        public void SizeOfTypeParameterConstrainedToEnum()
        {
            string text = @"
class Program<T> where T : System.Enum
{
    int F1 = sizeof(T);
}
";
            CreateCompilation(text).VerifyDiagnostics();
        }

        [Fact]
        public void SizeOfTypeParameterConstrainedToObject()
        {
            string text = @"
class Program<T> where T : object
{
    int F1 = sizeof(T);
}
";
            CreateCompilation(text).VerifyDiagnostics();
        }

        [Fact]
        public void SizeOfTypeParameterConstrainedToNullableObject()
        {
            string text = @"
#nullable enable
class Program<T> where T : object?
{
    int F1 = sizeof(T);
}
";
            CreateCompilation(text).VerifyDiagnostics(
                // (3,28): error CS0702: Constraint cannot be special class 'object?'
                // class Program<T> where T : object?
                Diagnostic(ErrorCode.ERR_SpecialTypeAsBound, "object?").WithArguments("object?").WithLocation(3, 28));
        }

        [Fact]
        public void SizeOfTypeParameterConstrainedToReferenceType()
        {
            string text = @"
using System.Collections.Generic;
class Program<T> where T : List<int>
{
    int F1 = sizeof(T);
}
";
            CreateCompilation(text).VerifyDiagnostics(
                // (5,14): error CS8424: 'sizeof(T)' is not allowed as 'T' is known to be a reference type.
                //     int F1 = sizeof(T);
                Diagnostic(ErrorCode.ERR_SizeOfReferenceType, "sizeof(T)").WithArguments("T").WithLocation(5, 14));
        }

        [Fact]
        public void SizeOfTypeParameterConstrainedToNotAType()
        {
            string text = @"
class Program<T> where T : A
{
    int F1 = sizeof(T);
}
";
            CreateCompilation(text).VerifyDiagnostics(
                // (2,28): error CS0246: The type or namespace name 'A' could not be found (are you missing a using directive or an assembly reference?)
                // class Program<T> where T : A
                Diagnostic(ErrorCode.ERR_SingleTypeNameNotFound, "A").WithArguments("A").WithLocation(2, 28));
        }

        [Fact]
        public void SizeOfTypeParameterConstrainedToUnconstrainedTypeParameter()
        {
            string text = @"
class Program<T, U> where T : U
{
    int F1 = sizeof(T);
}
";
            CreateCompilation(text).VerifyDiagnostics();
        }

        [Fact]
        public void SizeOfTypeParameterConstrainedToTypeParameterConstrainedToClass()
        {
            string text = @"
class Program<T, U> where T : U where U : class
{
    int F1 = sizeof(T);
}
";
            // This should be legal, since U could be object

            CreateCompilation(text).VerifyDiagnostics();
        }

        [Fact]
        public void SizeOfTypeParameterConstrainedToTypeParameterConstrainedToClassFollowedByTypeParameterConstrainesToReferenceType()
        {
            string text = @"
using System.Collections.Generic;
class Program<T, U, V> where T : U, V where U : class where V : List<int>
{
    int F1 = sizeof(T);
}
";
            CreateCompilation(text).VerifyDiagnostics(
                // (5,14): error CS8424: 'sizeof(T)' is not allowed as 'T' is known to be a reference type.
                //     int F1 = sizeof(T);
                Diagnostic(ErrorCode.ERR_SizeOfReferenceType, "sizeof(T)").WithArguments("T").WithLocation(5, 14));
        }

        [Fact]
        public void SizeOfTypeParameterConstrainedToTypeParameterConstrainedToInterface()
        {
            string text = @"
using System;
class Program<T, U> where T : U where U : IComparable
{
    int F1 = sizeof(T);
}
";
            CreateCompilation(text).VerifyDiagnostics();
        }

        [Fact]
        public void SizeOfTypeParameterConstrainedToTypeParameterConstrainedToReferenceType1()
        {
            string text = @"
using System.Collections.Generic;
class Program<T, U> where T : U where U : List<int>
{
    int F1 = sizeof(T);
}
";
            CreateCompilation(text).VerifyDiagnostics(
                // (5,14): error CS8424: 'sizeof(T)' is not allowed as 'T' is known to be a reference type.
                //     int F1 = sizeof(T);
                Diagnostic(ErrorCode.ERR_SizeOfReferenceType, "sizeof(T)").WithArguments("T").WithLocation(5, 14));
        }

        [Fact]
        public void SizeOfTypeParameterConstrainedToTypeParameterConstrainedToReferenceType2()
        {
            string text = @"
using System.Collections.Generic;
class Program<U> where U : List<int>
{
    class Inner<T> where T : U
    {
        int F1 = sizeof(T);
    }
}
";
            CreateCompilation(text).VerifyDiagnostics(
                // (7,18): error CS8424: 'sizeof(T)' is not allowed as 'T' is known to be a reference type.
                //         int F1 = sizeof(T);
                Diagnostic(ErrorCode.ERR_SizeOfReferenceType, "sizeof(T)").WithArguments("T").WithLocation(7, 18));
        }

        [Fact]
        public void SizeOfTypeParameterConstrainedToTypeParameterConstrainedToReferenceType3()
        {
            string text = @"
using System.Collections.Generic;
class Program<T, U, V> where T : U where U : V where V : List<int>
{
    int F1 = sizeof(T);
}
";
            CreateCompilation(text).VerifyDiagnostics(
                // (5,14): error CS8424: 'sizeof(T)' is not allowed as 'T' is known to be a reference type.
                //     int F1 = sizeof(T);
                Diagnostic(ErrorCode.ERR_SizeOfReferenceType, "sizeof(T)").WithArguments("T").WithLocation(5, 14));
        }

        [Fact]
        public void SizeOfTypeParameterConstrainedToTypeParameterConstrainedToObject()
        {
            string text = @"
class Program<T, U> where T : U where U : object
{
    int F1 = sizeof(T);
}
";
            CreateCompilation(text).VerifyDiagnostics();
        }

        [Fact]
        public void SizeOfUnconstrainedGeneric()
        {
            string text = @"
struct A<T>
{
}

class Program
{
    int F1 = sizeof(A);
    int F2 = sizeof(A<>);
}
";
            CreateCompilation(text).VerifyDiagnostics(
                // (8,21): error CS0305: Using the generic type 'A<T>' requires 1 type arguments
                //     int F1 = sizeof(A);
                Diagnostic(ErrorCode.ERR_BadArity, "A").WithArguments("A<T>", "type", "1").WithLocation(8, 21),
                // (9,21): error CS7003: Unexpected use of an unbound generic name
                //     int F2 = sizeof(A<>);
                Diagnostic(ErrorCode.ERR_UnexpectedUnboundGenericName, "A<>").WithLocation(9, 21));
        }

        private static readonly Dictionary<SpecialType, int> s_specialTypeSizeOfMap = new Dictionary<SpecialType, int>
        {
            { SpecialType.System_SByte, 1 },
            { SpecialType.System_Byte, 1 },
            { SpecialType.System_Int16, 2 },
            { SpecialType.System_UInt16, 2 },
            { SpecialType.System_Int32, 4 },
            { SpecialType.System_UInt32, 4 },
            { SpecialType.System_Int64, 8 },
            { SpecialType.System_UInt64, 8 },
            { SpecialType.System_Char, 2 },
            { SpecialType.System_Single, 4 },
            { SpecialType.System_Double, 8 },
            { SpecialType.System_Boolean, 1 },
            { SpecialType.System_Decimal, 16 },
        };

        [Fact]
        public void SizeOfSemanticModelSpecialTypes()
        {
            var text = @"
class Program
{
    static void Main()
    {
        int x;
        x = sizeof(sbyte);
        x = sizeof(byte);
        x = sizeof(short);
        x = sizeof(ushort);
        x = sizeof(int);
        x = sizeof(uint);
        x = sizeof(long);
        x = sizeof(ulong);
        x = sizeof(char);
        x = sizeof(float);
        x = sizeof(double);
        x = sizeof(bool);
        x = sizeof(decimal); //Supported by dev10, but not spec.
    }
}
";
            var compilation = CreateCompilation(text);
            var tree = compilation.SyntaxTrees.Single();
            var model = compilation.GetSemanticModel(tree);

            var syntaxes = tree.GetCompilationUnitRoot().DescendantNodes().OfType<SizeOfExpressionSyntax>();

            foreach (var syntax in syntaxes)
            {
                var typeSummary = model.GetSemanticInfoSummary(syntax.Type);
                var type = typeSummary.Symbol as TypeSymbol;

                Assert.NotNull(type);
                Assert.Equal(0, typeSummary.CandidateSymbols.Length);
                Assert.Equal(CandidateReason.None, typeSummary.CandidateReason);
                Assert.NotEqual(SpecialType.None, type.SpecialType);
                Assert.Equal(type, typeSummary.Type);
                Assert.Equal(type, typeSummary.ConvertedType);
                Assert.Equal(Conversion.Identity, typeSummary.ImplicitConversion);
                Assert.Null(typeSummary.Alias);
                Assert.False(typeSummary.IsCompileTimeConstant);
                Assert.False(typeSummary.ConstantValue.HasValue);


                var sizeOfSummary = model.GetSemanticInfoSummary(syntax);

                Assert.Null(sizeOfSummary.Symbol);
                Assert.Equal(0, sizeOfSummary.CandidateSymbols.Length);
                Assert.Equal(CandidateReason.None, sizeOfSummary.CandidateReason);
                Assert.Equal(SpecialType.System_Int32, sizeOfSummary.Type.SpecialType);
                Assert.Equal(sizeOfSummary.Type, sizeOfSummary.ConvertedType);
                Assert.Equal(Conversion.Identity, sizeOfSummary.ImplicitConversion);
                Assert.Equal(0, sizeOfSummary.MethodGroup.Length);
                Assert.Null(sizeOfSummary.Alias);
                Assert.True(sizeOfSummary.IsCompileTimeConstant);
                Assert.Equal(s_specialTypeSizeOfMap[type.SpecialType], sizeOfSummary.ConstantValue);
            }
        }

        [Fact]
        public void SizeOfSemanticModelEnum()
        {
            var text = @"
class Program
{
    static void Main()
    {
        int x;
        x = sizeof(E1);
        x = sizeof(E2);
    }
}

enum E1 : short
{
    A
}

enum E2 : long
{
    A
}
";
            var compilation = CreateCompilation(text);
            var tree = compilation.SyntaxTrees.Single();
            var model = compilation.GetSemanticModel(tree);

            var syntaxes = tree.GetCompilationUnitRoot().DescendantNodes().OfType<SizeOfExpressionSyntax>();

            foreach (var syntax in syntaxes)
            {
                var typeSummary = model.GetSemanticInfoSummary(syntax.Type);
                var type = typeSummary.Symbol as TypeSymbol;

                Assert.NotNull(type);
                Assert.Equal(0, typeSummary.CandidateSymbols.Length);
                Assert.Equal(CandidateReason.None, typeSummary.CandidateReason);
                Assert.Equal(TypeKind.Enum, type.TypeKind);
                Assert.Equal(type, typeSummary.Type);
                Assert.Equal(type, typeSummary.ConvertedType);
                Assert.Equal(Conversion.Identity, typeSummary.ImplicitConversion);
                Assert.Null(typeSummary.Alias);
                Assert.False(typeSummary.IsCompileTimeConstant);
                Assert.False(typeSummary.ConstantValue.HasValue);


                var sizeOfSummary = model.GetSemanticInfoSummary(syntax);

                Assert.Null(sizeOfSummary.Symbol);
                Assert.Equal(0, sizeOfSummary.CandidateSymbols.Length);
                Assert.Equal(CandidateReason.None, sizeOfSummary.CandidateReason);
                Assert.Equal(SpecialType.System_Int32, sizeOfSummary.Type.SpecialType);
                Assert.Equal(sizeOfSummary.Type, sizeOfSummary.ConvertedType);
                Assert.Equal(Conversion.Identity, sizeOfSummary.ImplicitConversion);
                Assert.Equal(0, sizeOfSummary.MethodGroup.Length);
                Assert.Null(sizeOfSummary.Alias);
                Assert.True(sizeOfSummary.IsCompileTimeConstant);
                Assert.Equal(s_specialTypeSizeOfMap[type.GetEnumUnderlyingType().SpecialType], sizeOfSummary.ConstantValue);
            }
        }

        [Fact]
        public void SizeOfSemanticModelUnmanaged()
        {
            var text = @"
struct Outer
{
    unsafe static void Main()
    {
        int x;
        x = sizeof(Outer);
        x = sizeof(Inner);
        x = sizeof(Outer*);
        x = sizeof(Inner*);
        x = sizeof(Outer**);
        x = sizeof(Inner**);
    }

    struct Inner
    {
    }
}
";
            var compilation = CreateCompilation(text);
            var tree = compilation.SyntaxTrees.Single();
            var model = compilation.GetSemanticModel(tree);

            var syntaxes = tree.GetCompilationUnitRoot().DescendantNodes().OfType<SizeOfExpressionSyntax>();

            foreach (var syntax in syntaxes)
            {
                var typeSummary = model.GetSemanticInfoSummary(syntax.Type);
                var type = typeSummary.Symbol as TypeSymbol;

                Assert.NotNull(type);
                Assert.Equal(0, typeSummary.CandidateSymbols.Length);
                Assert.Equal(CandidateReason.None, typeSummary.CandidateReason);
                Assert.Equal(SpecialType.None, type.SpecialType);
                Assert.Equal(type, typeSummary.Type);
                Assert.Equal(type, typeSummary.ConvertedType);
                Assert.Equal(Conversion.Identity, typeSummary.ImplicitConversion);
                Assert.Null(typeSummary.Alias);
                Assert.False(typeSummary.IsCompileTimeConstant);
                Assert.False(typeSummary.ConstantValue.HasValue);


                var sizeOfSummary = model.GetSemanticInfoSummary(syntax);

                Assert.Null(sizeOfSummary.Symbol);
                Assert.Equal(0, sizeOfSummary.CandidateSymbols.Length);
                Assert.Equal(CandidateReason.None, sizeOfSummary.CandidateReason);
                Assert.Equal(SpecialType.System_Int32, sizeOfSummary.Type.SpecialType);
                Assert.Equal(sizeOfSummary.Type, sizeOfSummary.ConvertedType);
                Assert.Equal(Conversion.Identity, sizeOfSummary.ImplicitConversion);
                Assert.Equal(0, sizeOfSummary.MethodGroup.Length);
                Assert.Null(sizeOfSummary.Alias);
                Assert.False(sizeOfSummary.IsCompileTimeConstant);
                Assert.False(sizeOfSummary.ConstantValue.HasValue);
            }
        }
    }
}
