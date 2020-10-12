﻿// Licensed to the .NET Foundation under one or more agreements.
// The .NET Foundation licenses this file to you under the MIT license.
// See the LICENSE file in the project root for more information.

#nullable disable

using System.IO;
using System.Linq;

namespace CSharpSyntaxGenerator
{
    internal class TestWriter : AbstractFileWriter
    {
        private TestWriter(TextWriter writer, Tree tree) : base(writer, tree)
        {
        }

        public static void Write(TextWriter writer, Tree tree)
        {
            new TestWriter(writer, tree).WriteFile();
        }

        private void WriteFile()
        {
            WriteLine("// <auto-generated />");
            WriteLine();
            WriteLine("using Microsoft.CodeAnalysis.CSharp.Syntax;");
            WriteLine("using Roslyn.Utilities;");
            WriteLine("using Xunit;");
            WriteLine("using InternalSyntaxFactory = Microsoft.CodeAnalysis.CSharp.Syntax.InternalSyntax.SyntaxFactory;");
            WriteLine();

            WriteLine("namespace Microsoft.CodeAnalysis.CSharp.UnitTests");
            OpenBlock();

            WriteLine();
            WriteLine("public partial class GreenNodeTests");
            OpenBlock();

            WriteLine("#region Green Generators");
            WriteNodeGenerators(isGreen: true);
            WriteLine("#endregion Green Generators");
            WriteLine();

            WriteLine("#region Green Factory and Property Tests");
            WriteFactoryPropertyTests(isGreen: true);
            WriteLine("#endregion Green Factory and Property Tests");
            WriteLine();

            WriteLine("#region Green Rewriters");
            WriteRewriterTests();
            WriteLine("#endregion Green Rewriters");

            CloseBlock();

            WriteLine();
            WriteLine("public partial class RedNodeTests");
            OpenBlock();

            WriteLine("#region Red Generators");
            WriteNodeGenerators(isGreen: false);
            WriteLine("#endregion Red Generators");
            WriteLine();

            WriteLine("#region Red Factory and Property Tests");
            WriteFactoryPropertyTests(isGreen: false);
            WriteLine("#endregion Red Factory and Property Tests");
            WriteLine();

            WriteLine("#region Red Rewriters");
            WriteRewriterTests();
            WriteLine("#endregion Red Rewriters");

            CloseBlock();

            CloseBlock();
        }

        private void WriteNodeGenerators(bool isGreen)
        {
            var nodes = Tree.Types.Where(n => !(n is PredefinedNode) && !(n is AbstractNode));
            bool first = true;
            foreach (var node in nodes)
            {
                if (!first)
                {
                    WriteLine();
                }
                first = false;
                WriteNodeGenerator((Node)node, isGreen);
            }
        }

        private void WriteNodeGenerator(Node node, bool isGreen)
        {
            var valueFields = node.Fields.Where(n => !IsNodeOrNodeList(n.Type));
            var nodeFields = node.Fields.Where(n => IsNodeOrNodeList(n.Type));

            var internalNamespace = isGreen ? "Microsoft.CodeAnalysis.Syntax.InternalSyntax." : "";
            var csharpNamespace = isGreen ? "Syntax.InternalSyntax." : "";
            var syntaxFactory = isGreen ? "InternalSyntaxFactory" : "SyntaxFactory";

            var strippedName = StripPost(node.Name, "Syntax");

            WriteLine($"private static {csharpNamespace}{node.Name} Generate{strippedName}()");

            Write($"    => {syntaxFactory}.{strippedName}(");
            //instantiate node

            bool first = true;

            if (node.Kinds.Count > 1)
            {
                Write($"SyntaxKind.{node.Kinds[0].Name}"); //TODO: other kinds?
                first = false;
            }

            foreach (var field in nodeFields)
            {
                if (!first)
                {
                    Write(", ");
                }
                first = false;

                if (IsOptional(field))
                {
                    if (isGreen)
                    {
                        Write("null");
                    }
                    else
                    {
                        Write($"default({field.Type})");
                    }
                }
                else if (IsAnyList(field.Type))
                {
                    string typeName;
                    if (isGreen)
                    {
                        typeName = internalNamespace + field.Type.Replace("<", "<" + csharpNamespace);
                    }
                    else
                    {
                        typeName = (field.Type == "SyntaxList<SyntaxToken>") ? "SyntaxTokenList" : field.Type;
                    }
                    Write($"new {typeName}()");
                }
                else if (field.Type == "SyntaxToken")
                {
                    var kind = ChooseValidKind(field);
                    var leadingTrivia = isGreen ? "null, " : string.Empty;
                    var trailingTrivia = isGreen ? ", null" : string.Empty;
                    if (kind == "IdentifierToken")
                    {
                        Write($"{syntaxFactory}.Identifier(\"{field.Name}\")");
                    }
                    else if (kind == "StringLiteralToken")
                    {
                        Write($"{syntaxFactory}.Literal({leadingTrivia}\"string\", \"string\"{trailingTrivia})");
                    }
                    else if (kind == "CharacterLiteralToken")
                    {
                        Write($"{syntaxFactory}.Literal({leadingTrivia}\"a\", 'a'{trailingTrivia})");
                    }
                    else if (kind == "NumericLiteralToken")
                    {
                        Write($"{syntaxFactory}.Literal({leadingTrivia}\"1\", 1{trailingTrivia})");
                    }
                    else
                    {
                        Write($"{syntaxFactory}.Token(SyntaxKind.{ChooseValidKind(field)})");
                    }
                }
                else if (field.Type == "CSharpSyntaxNode")
                {
                    Write($"{syntaxFactory}.IdentifierName({syntaxFactory}.Identifier(\"{field.Name}\"))");
                }
                else
                {
                    //drill down to a concrete type
                    var type = field.Type;
                    while (true)
                    {
                        var subTypes = ChildMap[type];
                        if (!subTypes.Any())
                        {
                            break;
                        }
                        type = subTypes.First();
                    }
                    Write($"Generate{StripPost(type, "Syntax")}()");
                }
            }

            foreach (var field in valueFields)
            {
                if (!first)
                {
                    Write(", ");
                }
                first = false;

                Write($"new {field.Type}()");
            }

            WriteLine(");");
        }

        private void WriteFactoryPropertyTests(bool isGreen)
        {
            var nodes = Tree.Types.Where(n => !(n is PredefinedNode) && !(n is AbstractNode));
            bool first = true;
            foreach (var node in nodes)
            {
                if (!first)
                {
                    WriteLine();
                }
                first = false;
                WriteFactoryPropertyTest((Node)node, isGreen);
            }
        }

        private void WriteFactoryPropertyTest(Node node, bool isGreen)
        {
            var valueFields = node.Fields.Where(n => !IsNodeOrNodeList(n.Type));
            var nodeFields = node.Fields.Where(n => IsNodeOrNodeList(n.Type));

            var strippedName = StripPost(node.Name, "Syntax");

            WriteLine("[Fact]");
            WriteLine($"public void Test{strippedName}FactoryAndProperties()");
            OpenBlock();

            WriteLine($"var node = Generate{strippedName}();");

            WriteLine();

            //check properties
            {
                string withStat = null;
                foreach (var field in nodeFields)
                {
                    if (IsOptional(field))
                    {
                        if (!isGreen && field.Type == "SyntaxToken")
                        {
                            WriteLine($"Assert.Equal(SyntaxKind.None, node.{field.Name}.Kind());");
                        }
                        else
                        {
                            WriteLine($"Assert.Null(node.{field.Name});");
                        }
                    }
                    else if (field.Type == "SyntaxToken")
                    {
                        if (!isGreen)
                        {
                            WriteLine($"Assert.Equal(SyntaxKind.{ChooseValidKind(field)}, node.{field.Name}.Kind());");
                        }
                        else
                        {
                            WriteLine($"Assert.Equal(SyntaxKind.{ChooseValidKind(field)}, node.{field.Name}.Kind);");
                        }
                    }
                    else
                    {
                        if (field.Type == "SyntaxToken")
                        {
                            WriteLine($"Assert.NotEqual(default, node.{field.Name});");
                        }
                        else if (
                            field.Type == "SyntaxTokenList" ||
                            field.Type.StartsWith("SyntaxList<") ||
                            field.Type.StartsWith("SeparatedSyntaxList<"))
                        {
                            WriteLine($"Assert.Equal(default, node.{field.Name});");
                        }
                        else
                        {
                            WriteLine($"Assert.NotNull(node.{field.Name});");
                        }
                    }

                    if (!isGreen)
                    {
                        withStat += $".With{field.Name}(node.{field.Name})";
                    }
                }

                foreach (var field in valueFields)
                {
                    WriteLine($"Assert.Equal(new {field.Type}(), node.{field.Name});");
                    if (!isGreen)
                    {
                        withStat += $".With{field.Name}(node.{field.Name})";
                    }
                }

                if (!isGreen && withStat != null)
                {
                    WriteLine($"var newNode = node{withStat};");
                    WriteLine("Assert.Equal(node, newNode);");
                }
            }

            if (isGreen)
            {
                WriteLine();
                WriteLine("AttachAndCheckDiagnostics(node);");
            }

            CloseBlock();
        }

        private void WriteRewriterTests()
        {
            var nodes = Tree.Types.Where(n => !(n is PredefinedNode) && !(n is AbstractNode));
            bool first = true;
            foreach (var node in nodes)
            {
                if (!first)
                {
                    WriteLine();
                }
                first = false;
                WriteTokenDeleteRewriterTest((Node)node);
                WriteLine();
                WriteIdentityRewriterTest((Node)node);
            }
        }

        private void WriteTokenDeleteRewriterTest(Node node)
        {
            var valueFields = node.Fields.Where(n => !IsNodeOrNodeList(n.Type));
            var nodeFields = node.Fields.Where(n => IsNodeOrNodeList(n.Type));

            var strippedName = StripPost(node.Name, "Syntax");

            WriteLine("[Fact]");
            WriteLine($"public void Test{strippedName}TokenDeleteRewriter()");
            OpenBlock();

            WriteLine($"var oldNode = Generate{strippedName}();");
            WriteLine("var rewriter = new TokenDeleteRewriter();");
            WriteLine("var newNode = rewriter.Visit(oldNode);");

            WriteLine();
            WriteLine("if(!oldNode.IsMissing)");
            OpenBlock();
            WriteLine("Assert.NotEqual(oldNode, newNode);");
            CloseBlock();

            WriteLine();
            WriteLine("Assert.NotNull(newNode);");
            WriteLine("Assert.True(newNode.IsMissing, \"No tokens => missing\");");

            CloseBlock();
        }

        private void WriteIdentityRewriterTest(Node node)
        {
            var valueFields = node.Fields.Where(n => !IsNodeOrNodeList(n.Type));
            var nodeFields = node.Fields.Where(n => IsNodeOrNodeList(n.Type));

            var strippedName = StripPost(node.Name, "Syntax");

            WriteLine("[Fact]");
            WriteLine($"public void Test{strippedName}IdentityRewriter()");
            OpenBlock();

            WriteLine($"var oldNode = Generate{strippedName}();");
            WriteLine("var rewriter = new IdentityRewriter();");
            WriteLine("var newNode = rewriter.Visit(oldNode);");

            WriteLine();

            WriteLine("Assert.Same(oldNode, newNode);");

            CloseBlock();
        }

        //guess a reasonable kind if there are no constraints
        private static string ChooseValidKind(Field field)
        {
            return field.Kinds.Any() ? field.Kinds[0].Name : "IdentifierToken";
        }
    }
}
