// Copyright (c) Microsoft.  All Rights Reserved.  Licensed under the Apache License, Version 2.0.  See License.txt in the project root for license information.

using Microsoft.CodeAnalysis.CSharp.Syntax;
using Microsoft.CodeAnalysis.CSharp.Test.Utilities;
using Microsoft.CodeAnalysis.Test.Utilities;
using Xunit;

namespace Microsoft.CodeAnalysis.CSharp.UnitTests
{
    public partial class IOperationTests : SemanticModelTestBase
    {
        [CompilerTrait(CompilerFeature.IOperation)]
        [Fact]
        public void TestSizeOfExpression()
        {
            string source = @"
using System;

class C
{
    void M(int i)
    {
        i = /*<bind>*/sizeof(int)/*</bind>*/;
    }
}
";
            string expectedOperationTree = @"
ISizeOfOperation (OperationKind.SizeOf, Type: System.Int32, Constant: 4) (Syntax: 'sizeof(int)')
  TypeOperand: System.Int32
";
            var expectedDiagnostics = DiagnosticDescription.None;

            VerifyOperationTreeAndDiagnosticsForTest<SizeOfExpressionSyntax>(source, expectedOperationTree, expectedDiagnostics);
        }

        [CompilerTrait(CompilerFeature.IOperation)]
        [Fact]
        public void TestSizeOfExpression_ManagedStructTypeArgument()
        {
            string source = @"
using System;

struct S
{
    string Str;
    void M(int i)
    {
        i = /*<bind>*/sizeof(S)/*</bind>*/;
    }
}
";
            string expectedOperationTree = @"
ISizeOfOperation (OperationKind.SizeOf, Type: System.Int32) (Syntax: 'sizeof(S)')
  TypeOperand: S
";
            var expectedDiagnostics = new DiagnosticDescription[] {
                // file.cs(6,12): warning CS0169: The field 'S.Str' is never used
                //     string Str;
                Diagnostic(ErrorCode.WRN_UnreferencedField, "Str").WithArguments("S.Str").WithLocation(6, 12)
            };

            VerifyOperationTreeAndDiagnosticsForTest<SizeOfExpressionSyntax>(source, expectedOperationTree, expectedDiagnostics);
        }

        [CompilerTrait(CompilerFeature.IOperation)]
        [Fact]
        public void TestSizeOfExpression_ReferenceTypeArgument()
        {
            string source = @"
using System;

class C
{
    void M(int i)
    {
        i = /*<bind>*/sizeof(C)/*</bind>*/;
    }
}
";
            string expectedOperationTree = @"
ISizeOfOperation (OperationKind.SizeOf, Type: System.Int32, IsInvalid) (Syntax: 'sizeof(C)')
  TypeOperand: C
";
            var expectedDiagnostics = new DiagnosticDescription[] {
                // file.cs(8,23): error CS8424: 'sizeof(C)' is not allowed as 'C' is known to be a reference type.
                //         i = /*<bind>*/sizeof(C)/*</bind>*/;
                Diagnostic(ErrorCode.ERR_SizeOfReferenceType, "sizeof(C)").WithArguments("C").WithLocation(8, 23)
            };

            VerifyOperationTreeAndDiagnosticsForTest<SizeOfExpressionSyntax>(source, expectedOperationTree, expectedDiagnostics);
        }

        [CompilerTrait(CompilerFeature.IOperation)]
        [Fact]
        public void TestSizeOfExpression_GenericTypeArgument()
        {
            string source = @"
using System;

class C
{
    void M<T>(int i)
    {
        i = /*<bind>*/sizeof(T)/*</bind>*/;
    }
}
";
            string expectedOperationTree = @"
ISizeOfOperation (OperationKind.SizeOf, Type: System.Int32) (Syntax: 'sizeof(T)')
  TypeOperand: T
";

            var expectedDiagnostics = DiagnosticDescription.None;

            VerifyOperationTreeAndDiagnosticsForTest<SizeOfExpressionSyntax>(source, expectedOperationTree, expectedDiagnostics);
        }

        [CompilerTrait(CompilerFeature.IOperation)]
        [Fact]
        public void TestSizeOfExpression_PointerTypeArgument()
        {
            string source = @"
using System;

class C
{
    unsafe void M(int i)
    {
        i = /*<bind>*/sizeof(void**)/*</bind>*/;
    }
}
";
            string expectedOperationTree = @"
ISizeOfOperation (OperationKind.SizeOf, Type: System.Int32) (Syntax: 'sizeof(void**)')
  TypeOperand: System.Void**
";
            var expectedDiagnostics = DiagnosticDescription.None;

            VerifyOperationTreeAndDiagnosticsForTest<SizeOfExpressionSyntax>(source, expectedOperationTree, expectedDiagnostics, compilationOptions: TestOptions.UnsafeReleaseDll);
        }

        [CompilerTrait(CompilerFeature.IOperation)]
        [Fact]
        public void TestSizeOfExpression_ErrorTypeArgument()
        {
            string source = @"
using System;

class C
{
    void M(int i)
    {
        i = /*<bind>*/sizeof(UndefinedType)/*</bind>*/;
    }
}
";
            string expectedOperationTree = @"
ISizeOfOperation (OperationKind.SizeOf, Type: System.Int32, IsInvalid) (Syntax: 'sizeof(UndefinedType)')
  TypeOperand: UndefinedType
";
            var expectedDiagnostics = new DiagnosticDescription[] {
                // CS0246: The type or namespace name 'UndefinedType' could not be found (are you missing a using directive or an assembly reference?)
                //         i = /*<bind>*/sizeof(UndefinedType)/*</bind>*/;
                Diagnostic(ErrorCode.ERR_SingleTypeNameNotFound, "UndefinedType").WithArguments("UndefinedType").WithLocation(8, 30),
            };

            VerifyOperationTreeAndDiagnosticsForTest<SizeOfExpressionSyntax>(source, expectedOperationTree, expectedDiagnostics);
        }

        [CompilerTrait(CompilerFeature.IOperation)]
        [Fact]
        public void TestSizeOfExpression_IdentifierArgument()
        {
            string source = @"
using System;

class C
{
    void M(int i)
    {
        i = /*<bind>*/sizeof(i)/*</bind>*/;
    }
}
";
            string expectedOperationTree = @"
ISizeOfOperation (OperationKind.SizeOf, Type: System.Int32, IsInvalid) (Syntax: 'sizeof(i)')
  TypeOperand: i
";
            var expectedDiagnostics = new DiagnosticDescription[] {
                // CS0118: 'i' is a variable but is used like a type
                //         i = /*<bind>*/sizeof(i)/*</bind>*/;
                Diagnostic(ErrorCode.ERR_BadSKknown, "i").WithArguments("i", "variable", "type").WithLocation(8, 30),
            };

            VerifyOperationTreeAndDiagnosticsForTest<SizeOfExpressionSyntax>(source, expectedOperationTree, expectedDiagnostics);
        }

        [CompilerTrait(CompilerFeature.IOperation)]
        [Fact]
        public void TestSizeOfExpression_ExpressionArgument()
        {
            string source = @"
using System;

class C
{
    void M(int i)
    {
        i = /*<bind>*/sizeof(M2()/*</bind>*/);
    }

    int M2() => 0;
}
";
            string expectedOperationTree = @"
IInvalidOperation (OperationKind.Invalid, Type: ?, IsInvalid) (Syntax: 'sizeof(M2()')
  Children(1):
      ISizeOfOperation (OperationKind.SizeOf, Type: System.Int32, IsInvalid) (Syntax: 'sizeof(M2')
        TypeOperand: M2
";
            var expectedDiagnostics = new DiagnosticDescription[] {
                // CS1026: ) expected
                //         i = /*<bind>*/sizeof(M2()/*</bind>*/);
                Diagnostic(ErrorCode.ERR_CloseParenExpected, "(").WithLocation(8, 32),
                // CS1002: ; expected
                //         i = /*<bind>*/sizeof(M2()/*</bind>*/);
                Diagnostic(ErrorCode.ERR_SemicolonExpected, ")").WithLocation(8, 45),
                // CS1513: } expected
                //         i = /*<bind>*/sizeof(M2()/*</bind>*/);
                Diagnostic(ErrorCode.ERR_RbraceExpected, ")").WithLocation(8, 45),
                // CS0246: The type or namespace name 'M2' could not be found (are you missing a using directive or an assembly reference?)
                //         i = /*<bind>*/sizeof(M2()/*</bind>*/);
                Diagnostic(ErrorCode.ERR_SingleTypeNameNotFound, "M2").WithArguments("M2").WithLocation(8, 30),
            };

            VerifyOperationTreeAndDiagnosticsForTest<InvocationExpressionSyntax>(source, expectedOperationTree, expectedDiagnostics);
        }

        [CompilerTrait(CompilerFeature.IOperation)]
        [Fact]
        public void TestSizeOfExpression_MissingArgument()
        {
            string source = @"
using System;

class C
{
    void M(int i)
    {
        i = /*<bind>*/sizeof()/*</bind>*/;
    }
}
";
            string expectedOperationTree = @"
ISizeOfOperation (OperationKind.SizeOf, Type: System.Int32, IsInvalid) (Syntax: 'sizeof()')
  TypeOperand: ?
";
            var expectedDiagnostics = new DiagnosticDescription[] {
                // CS1031: Type expected
                //         i = /*<bind>*/sizeof()/*</bind>*/;
                Diagnostic(ErrorCode.ERR_TypeExpected, ")").WithLocation(8, 30),
            };

            VerifyOperationTreeAndDiagnosticsForTest<SizeOfExpressionSyntax>(source, expectedOperationTree, expectedDiagnostics);
        }

        [CompilerTrait(CompilerFeature.IOperation, CompilerFeature.Dataflow)]
        [Fact]
        public void SizeOfFlow_01()
        {
            string source = @"
public class C
{
    void M(int i)
    /*<bind>*/{
        i = sizeof(int); 
    }/*</bind>*/
}
";
            var expectedDiagnostics = DiagnosticDescription.None;

            string expectedFlowGraph = @"
Block[B0] - Entry
    Statements (0)
    Next (Regular) Block[B1]
Block[B1] - Block
    Predecessors: [B0]
    Statements (1)
        IExpressionStatementOperation (OperationKind.ExpressionStatement, Type: null) (Syntax: 'i = sizeof(int);')
          Expression: 
            ISimpleAssignmentOperation (OperationKind.SimpleAssignment, Type: System.Int32) (Syntax: 'i = sizeof(int)')
              Left: 
                IParameterReferenceOperation: i (OperationKind.ParameterReference, Type: System.Int32) (Syntax: 'i')
              Right: 
                ISizeOfOperation (OperationKind.SizeOf, Type: System.Int32, Constant: 4) (Syntax: 'sizeof(int)')
                  TypeOperand: System.Int32

    Next (Regular) Block[B2]
Block[B2] - Exit
    Predecessors: [B1]
    Statements (0)
";
            VerifyFlowGraphAndDiagnosticsForTest<BlockSyntax>(source, expectedFlowGraph, expectedDiagnostics);
        }
    }
}
