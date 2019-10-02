package parser;

import compiler.Phase;
import compiler.Handler;
import compiler.Failure;
import compiler.Position;
import lexer.*;
import ast.*;

public class MiniParser extends Phase implements MiniTokens {
    private MiniLexer lexer;

    public MiniParser(Handler handler, MiniLexer lexer) {
        super(handler);
        this.lexer = lexer;
    }
  
    // -----------------------------------------------------------
    // program : stmts

    /** A Block that holds the sequence of statements in this program.
     */
    private Block program;

    /** Run the parser on the given input file and return the resulting
     *  Block.
     */
    public Block parseProgram() throws Failure {
        if (program==null) {
            // Force lexer to read first token
            lexer.nextToken();
            // And then parse the input stream
            program = new Block(parseStmts(0, ENDINPUT));
        }
        return program;
    }

    /** Return an array corresponding to a sequence of statements.
     *  The parameter n indicates how many previous statements have
     *  been read so that we can determine the correct array size when
     *  we reach the end of the input.  The parameter term indicates
     *  the token that will terminate the input (either ENDINPUT at
     *  the top-level of a program or '}' inside a block).
     */
    private Stmt[] parseStmts(int n, int term) throws Failure {
        if (lexer.getToken()==term) {
            return new Stmt[n];
        } else {
            Stmt   s     = parseStmt();
            Stmt[] stmts = parseStmts(n+1, term);
            stmts[n]     = s;
            return stmts;
        }
    }

    // -----------------------------------------------------------
    // stmt    : ;
    //         | ID = expr ;
    //         | { stmts }
    //         | WHILE ( expr ) stmt
    //         | IF ( expr ) stmt [ ELSE stmt ]
    //         | PRINT expr ;
    //         | type ID { , ID } ;
    // stmts   : /* empty */
    //         | stmts stmt

    private Stmt parseStmt() throws Failure {
        switch (lexer.getToken()) {
            case ';'   : return parseEmpty();
            case '{'   : return parseBlock();
            case WHILE : return parseWhile();
            case IF    : return parseIf();
            case PRINT : return parsePrint();
            default    : {
                Position pos  = lexer.getPos();
                Type     type = maybeParseType();
                if (type!=null) {
                    Id[] vars = parseIds(0);
                    require(';');
                    return new VarDecl(pos, type, vars);
                } else {
                    Expr exp = parseExpr();
                    require(';');
                    return new ExprStmt(pos, exp);
                }
            }
        }
    }

    private Stmt parseEmpty() throws Failure {
        Stmt empty = new Empty(lexer.getPos());
        lexer.nextToken(/* ; */);
        return empty;
    }

    private Stmt parseBlock() throws Failure {
        lexer.nextToken(/* { */);
        Stmt[] stmts = parseStmts(0, '}');
        lexer.nextToken(/* } */);
        return new Block(stmts);
    }

    private Stmt parseWhile() throws Failure {
        Position pos = lexer.getPos();
        lexer.nextToken(/* WHILE */);
        Expr test = parseTest();
        Stmt body = parseStmt();
        return new While(pos, test, body);
    }

    private Stmt parseIf() throws Failure {
        Position pos = lexer.getPos();
        lexer.nextToken(/* IF */);
        Expr test = parseTest();
        Stmt ifTrue  = parseStmt();
        Stmt ifFalse = lexer.match(ELSE) ? parseStmt() : new Empty(lexer.getPos());
        return new If(pos, test, ifTrue, ifFalse);
    }

    private Expr parseTest() throws Failure {
        require('(');
        Expr test = parseExpr();
        require(')');
        return test;
    }

    private Stmt parsePrint() throws Failure {
        Position pos = lexer.getPos();
        lexer.nextToken(/* PRINT */);
        Expr expr = parseExpr();
        require(';');
        return new Print(pos, expr);
    }

    private Id[] parseIds(int n) throws Failure {
        if (lexer.getToken()==ID) {
            Id   id  = new Id(lexer.getPos(), lexer.getLexeme());
            lexer.nextToken(/* ID */);
            Id[] ids = lexer.match(',') ? parseIds(n+1) : new Id[n+1];
            ids[n]   = id;
            return ids;
        }
        throw unexpectedToken();
    }

    // -----------------------------------------------------------
    // type    : INT
    //         | BOOLEAN

    private Type maybeParseType() {
        switch (lexer.getToken()) {
            case INT     : lexer.nextToken(/* INT */);
                           return Type.INT;

            case BOOLEAN : lexer.nextToken(/* BOOLEAN */);
                           return Type.BOOLEAN;

            default      : return null;
        }
    }

    // -----------------------------------------------------------
    // primary : INTLIT
    //         | TRUE
    //         | FALSE
    //         | ID
    //         | ( Expr )

    private Expr parsePrimary() throws Failure {
        switch (lexer.getToken()) {
            case INTLIT : {
                Expr e = new IntLit(lexer.getPos(), lexer.getNum());
                lexer.nextToken(/* INTLIT */);
                return e;
            }

            case TRUE : {
                Expr e = new BoolLit(lexer.getPos(), true);
                lexer.nextToken(/* TRUE */);
                return e;
            }

            case FALSE : {
                Expr e = new BoolLit(lexer.getPos(), false);
                lexer.nextToken(/* FALSE */);
                return e;
            }

            case ID     : {
                Expr e = new Id(lexer.getPos(), lexer.getLexeme());
                lexer.nextToken(/* ID */);
                return e;
            }

            case '('    : {
                lexer.nextToken(/* ( */);
                Expr e = parseExpr();
                require(')');
                return e;
            }

            default     : throw unexpectedToken();
        }
    }

    // -----------------------------------------------------------
    // unary   : + unary
    //         | - unary
    //         | ~ unary
    //         | ! unary
    //         | primary

    private Expr parseUnary() throws Failure {
        switch (lexer.getToken()) {
            case '+' : {
                Position pos = lexer.getPos();
                lexer.nextToken(/* + */);
                return new UPlus(pos, parseUnary());
            }

            case '-' : {
                Position pos = lexer.getPos();
                lexer.nextToken(/* - */);
                return new UMinus(pos, parseUnary());
            }

            case '!' : {
                Position pos = lexer.getPos();
                lexer.nextToken(/* ! */);
                return new LNot(pos, parseUnary());
            }

            case '~' : {
                Position pos = lexer.getPos();
                lexer.nextToken(/* ~ */);
                return new BNot(pos, parseUnary());
            }

            default  : return parsePrimary();
        }
    }

    // -----------------------------------------------------------
    // mult    : mult * unary
    //         | unary

    private Expr parseMult() throws Failure {
        Expr e = parseUnary();
        for (;;) {
            if (lexer.getToken()=='*') {
                Position pos = lexer.getPos();
                lexer.nextToken(/* * */);
                e = new Mul(pos, e, parseUnary());
            } else {
                return e;
            }
        }
    }

    // -----------------------------------------------------------
    // add     : add + mult
    //         | add - mult
    //         | mult

    private Expr parseAdd() throws Failure {
        Expr e = parseMult();
        for (;;) {
            if (lexer.getToken()=='+') {
                Position pos = lexer.getPos();
                lexer.nextToken(/* + */);
                e = new Add(pos, e, parseMult());
            } else if (lexer.getToken()=='-') {
                Position pos = lexer.getPos();
                lexer.nextToken(/* - */);
                e = new Sub(pos, e, parseMult());
            } else {
                return e;
            }
        }
    }

    // -----------------------------------------------------------
    // rel     | rel < add
    //         | rel > add
    //         | rel <= add
    //         | rel >= add
    //         | add

    private Expr parseRel() throws Failure {
        Expr e = parseAdd();
        for (;;) {
            if (lexer.getToken()=='<') {
                Position pos = lexer.getPos();
                lexer.nextToken(/* < */);
                e = new Lt(pos, e, parseAdd());
            } else if (lexer.getToken()==LTE) {
                Position pos = lexer.getPos();
                lexer.nextToken(/* <= */);
                e = new Lte(pos, e, parseAdd());
            } else if (lexer.getToken()=='>') {
                Position pos = lexer.getPos();
                lexer.nextToken(/* > */);
                e = new Gt(pos, e, parseAdd());
            } else if (lexer.getToken()==GTE) {
                Position pos = lexer.getPos();
                lexer.nextToken(/* >= */);
                e = new Gte(pos, e, parseAdd());
            } else {
                return e;
            }
        }
    }

    // -----------------------------------------------------------
    // eql     : eql == rel
    //         | eql != rel
    //         | rel

    private Expr parseEql() throws Failure {
        Expr e = parseRel();
        for (;;) {
            if (lexer.getToken()==EQEQ) {
                Position pos = lexer.getPos();
                lexer.nextToken(/* == */);
                e = new Eql(pos, e, parseRel());
            } else if (lexer.getToken()==NEQ) {
                Position pos = lexer.getPos();
                lexer.nextToken(/* != */);
                e = new Neq(pos, e, parseRel());
            } else {
                return e;
            }
        }
    }

    // -----------------------------------------------------------
    // band    : band & eql
    //         | eql

    private Expr parseBAnd() throws Failure {
        Expr e = parseEql();
        while (lexer.getToken()=='&') {
            Position pos = lexer.getPos();
            lexer.nextToken(/* & */);
            e = new BAnd(pos, e, parseEql());
        }
        return e;
    }

    // -----------------------------------------------------------
    // bxor    : bxor ^ band
    //         | band

    private Expr parseBXor() throws Failure {
        Expr e = parseBAnd();
        while (lexer.getToken()=='^') {
            Position pos = lexer.getPos();
            lexer.nextToken(/* ^ */);
            e = new BXor(pos, e, parseBAnd());
        }
        return e;
    }

    // -----------------------------------------------------------
    // bor     : bor | bxor
    //         | bxor

    private Expr parseBOr() throws Failure {
        Expr e = parseBXor();
        while (lexer.getToken()=='|') {
            Position pos = lexer.getPos();
            lexer.nextToken(/* | */);
            e = new BOr(pos, e, parseBXor());
        }
        return e;
    }

    // -----------------------------------------------------------
    // land    : land && bor
    //         | bor

    private Expr parseLAnd() throws Failure {
        Expr e = parseBOr();
        while (lexer.getToken()==LAND) {
            Position pos = lexer.getPos();
            lexer.nextToken(/* && */);
            e = new LAnd(pos, e, parseBOr());
        }
        return e;
    }

    // -----------------------------------------------------------
    // lor     : lor || land
    //         | land

    private Expr parseLOr() throws Failure {
        Expr e = parseLAnd();
        while (lexer.getToken()==LOR) {
            Position pos = lexer.getPos();
            lexer.nextToken(/* || */);
            e = new LOr(pos, e, parseLAnd());
        }
        return e;
    }

    // -----------------------------------------------------------
    // expr    : ID = expr
    //         | lor

    private Expr parseExpr() throws Failure {
        Expr e = parseLOr();
        if (lexer.getToken()=='=') {
            Position pos = lexer.getPos();
            lexer.nextToken(/* = */);
            e = e.assignTo(pos, parseExpr());
        }
        return e;
    }

    // -----------------------------------------------------------

    private Failure unexpectedToken() {
        return new Failure(lexer.getPos(), "Unexpected " + lexer.tokenName());
    }

    private void require(int tok) throws Failure {
        if (lexer.getToken()!=tok) {
            throw new Failure(lexer.getPos(), "Missing '" + (char)tok + "'");
        }
        lexer.nextToken(/* tok */);
    }

}
