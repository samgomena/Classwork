import compiler.*;
import lexer.*;
import parser.*;
import ast.*;
import java.io.FileReader;

public class MiniCompiler {
  public static void main(String[] args) {
    Handler handler = new SimpleHandler();
    try {
      if (args.length!=1) {
        throw new Failure("This program requires exactly one argument");
      }

      Stmt prog = frontEnd(handler, args[0] + ".mini");
      middleEnd(handler, prog);
      backEnd(args[0], prog);

    } catch (Failure f) {
      handler.report(f);
    } catch (Exception e) { 
      handler.report(new Failure("Exception: " + e));
      e.printStackTrace();
    }     
  }     

  // Read program:
  private static Stmt frontEnd(Handler handler, String input) throws Exception {
    FileReader reader = new FileReader(input);
    Source     source = new JavaSource(handler, input, reader);
    MiniLexer  lexer  = new MiniLexer(handler, source);
    MiniParser parser = new MiniParser(handler, lexer);
    Stmt       prog   = parser.parseProgram();
    if (handler.hasFailures()) {
      throw new Failure("Aborting: errors detected during syntax analysis");
    }
    return prog;
  }

  // Analyze program:
  private static void middleEnd(Handler handler, Stmt prog) throws Exception {
    // new TextOutput(System.out).toText(prog);
    // new IndentOutput(System.out).indent(prog);
    //new DotOutput("ast.dot").toDot(prog);
    new ScopeAnalysis(handler).analyze(prog);
    new TypeAnalysis(handler).analyze(prog);
    // new DotOutput("ast.dot").toDot(prog);
    new InitAnalysis(handler).analyze(prog);
  }

  // Generate executable version of program:
  private static void backEnd(String name, Stmt prog) throws Exception {
    // Optimization:
    prog.simplify();

    // Output compiled program:
    String asm = name + ".s";
    new IA32Output(asm).generateAssembly(prog);
    System.out.println("Assembly code output: " + asm);

    // Invoke assembler (via gcc) to produce executable:
    Runtime.getRuntime()
           .exec("gcc -m32 -o " + name + " " + asm + " runtime.c")
           .waitFor();
    System.out.println("Executable program: " + name);
  }
}
