import java.io.*;
import java.util.*;

// **********************************************************************
// The ASTnode class defines the nodes of the abstract-syntax tree that
// represents a Mini program.
//
// Internal nodes of the tree contain pointers to children, organized
// either in a list (for nodes that may have a variable number of 
// children) or as a fixed set of fields.
//
// The nodes for literals and ids contain line and character number
// information; for string literals and identifiers, they also contain a
// string; for integer literals, they also contain an integer value.
//
// Here are all the different kinds of AST nodes and what kinds of children
// they have.  All of these kinds of AST nodes are subclasses of "ASTnode".
// Indentation indicates further subclassing:
//
//     Subclass            Kids
//     --------            ----
//     ProgramNode         DeclListNode
//     DeclListNode        linked list of DeclNode
//     DeclNode:
//       VarDeclNode       TypeNode, IdNode, int
//       FnDeclNode        TypeNode, IdNode, FormalsListNode, FnBodyNode
//       FormalDeclNode    TypeNode, IdNode
//       StructDeclNode    IdNode, DeclListNode
//
//     FormalsListNode     linked list of FormalDeclNode
//     FnBodyNode          DeclListNode, StmtListNode
//     StmtListNode        linked list of StmtNode
//     ExpListNode         linked list of ExpNode
//
//     TypeNode:
//       IntNode           -- none --
//       BoolNode          -- none --
//       VoidNode          -- none --
//       StructNode        IdNode
//
//     StmtNode:
//       AssignStmtNode      AssignNode
//       PostIncStmtNode     ExpNode
//       PostDecStmtNode     ExpNode
//       ReadStmtNode        ExpNode
//       WriteStmtNode       ExpNode
//       IfStmtNode          ExpNode, DeclListNode, StmtListNode
//       IfElseStmtNode      ExpNode, DeclListNode, StmtListNode,
//                                    DeclListNode, StmtListNode
//       WhileStmtNode       ExpNode, DeclListNode, StmtListNode
//       CallStmtNode        CallExpNode
//       ReturnStmtNode      ExpNode
//
//     ExpNode:
//       IntLitNode          -- none --
//       StrLitNode          -- none --
//       TrueNode            -- none --
//       FalseNode           -- none --
//       IdNode              -- none --
//       DotAccessNode       ExpNode, IdNode
//       AssignNode          ExpNode, ExpNode
//       CallExpNode         IdNode, ExpListNode
//       UnaryExpNode        ExpNode
//         UnaryMinusNode
//         NotNode
//       BinaryExpNode       ExpNode ExpNode
//         PlusNode     
//         MinusNode
//         TimesNode
//         DivideNode
//         AndNode
//         OrNode
//         EqualsNode
//         NotEqualsNode
//         LessNode
//         GreaterNode
//         LessEqNode
//         GreaterEqNode
//
// Here are the different kinds of AST nodes again, organized according to
// whether they are leaves, internal nodes with linked lists of kids, or
// internal nodes with a fixed number of kids:
//
// (1) Leaf nodes:
//        IntNode,   BoolNode,  VoidNode,  IntLitNode,  StrLitNode,
//        TrueNode,  FalseNode, IdNode
//
// (2) Internal nodes with (possibly empty) linked lists of children:
//        DeclListNode, FormalsListNode, StmtListNode, ExpListNode
//
// (3) Internal nodes with fixed numbers of kids:
//        ProgramNode,     VarDeclNode,     FnDeclNode,     FormalDeclNode,
//        StructDeclNode,  FnBodyNode,      StructNode,     AssignStmtNode,
//        PostIncStmtNode, PostDecStmtNode, ReadStmtNode,   WriteStmtNode   
//        IfStmtNode,      IfElseStmtNode,  WhileStmtNode,  CallStmtNode
//        ReturnStmtNode,  DotAccessNode,   AssignExpNode,  CallExpNode,
//        UnaryExpNode,    BinaryExpNode,   UnaryMinusNode, NotNode,
//        PlusNode,        MinusNode,       TimesNode,      DivideNode,
//        AndNode,         OrNode,          EqualsNode,     NotEqualsNode,
//        LessNode,        GreaterNode,     LessEqNode,     GreaterEqNode
//
// **********************************************************************

// **********************************************************************
// ASTnode class (base class for all other kinds of nodes)
// **********************************************************************

abstract class ASTnode { 
    // every subclass must provide an unparse operation
    abstract public void unparse(PrintWriter p, int indent);

    // this method can be used by the unparse methods to do indenting
    protected void doIndent(PrintWriter p, int indent) {
        for (int k=0; k<indent; k++) p.print(" ");
    }
}

// **********************************************************************
// ProgramNode,  DeclListNode, FormalsListNode, FnBodyNode,
// StmtListNode, ExpListNode
// **********************************************************************

class ProgramNode extends ASTnode {
    public ProgramNode(DeclListNode L) {
        myDeclList = L;
    }

    /**
     * nameAnalysis
     * Creates an empty symbol table for the outermost scope, then processes
     * all of the globals, struct defintions, and functions in the program.
     */
    public void nameAnalysis() {
        SymTable symTab = new SymTable();
        myDeclList.nameAnalysis(symTab, 1, 0); //myGlobal = 1; offset = 0;
        if(myDeclList.noMain == 1) {
            ErrMsg.fatal(0, 0, "No main function");
        }
    }
    
    /**
     * typeCheck
     */
    public void typeCheck() {
        myDeclList.typeCheck();
    }

    public void partialEvaluate() {
        myDeclList.partialEvaluate();
    }
    
    public void codeGen() {
        myDeclList.codeGen();
    }

    public void unparse(PrintWriter p, int indent) {
        myDeclList.unparse(p, indent);
    }

    // 1 kid
    private DeclListNode myDeclList;
}

class DeclListNode extends ASTnode {
    public DeclListNode(List<DeclNode> S) {
        myDecls = S;
    }

    /**
     * nameAnalysis
     * Given a symbol table symTab, process all of the decls in the list.
     */
    public void nameAnalysis(SymTable symTab) {
        nameAnalysis(symTab, symTab);
    }
    
    public void nameAnalysis(SymTable symTab, int global, int offset) {
        myGlobal = global;
        myOffset = offset;
        nameAnalysis(symTab, symTab);
    }
    /**
     * nameAnalysis
     * Given a symbol table symTab and a global symbol table globalTab
     * (for processing struct names in variable decls), process all of the 
     * decls in the list.
     */    
    public void nameAnalysis(SymTable symTab, SymTable globalTab) {
        for (DeclNode node : myDecls) {
            if (node instanceof VarDeclNode) {
                mySym = ((VarDeclNode)node).nameAnalysis(symTab, globalTab, myGlobal);
                mySym.isGlobal = myGlobal;
                mySym.setOffset(myOffset);
                myOffset += 4; //TODO - assuming its always 4
            } else {
                SemSym tempSym = node.nameAnalysis(symTab);
                if (node instanceof FnDeclNode) {
                    if(((FnDeclNode)node).returnId().matches("main")) {
                        noMain = 0;
                    }
                }
            }
        }
    }    
    
    /**
     * typeCheck
     */
    public void typeCheck() {
        for (DeclNode node : myDecls) {
            node.typeCheck();
        }
    }
    
    public void partialEvaluate() {
        for (DeclNode node : myDecls) {
            node.partialEvaluate();
        }
    }
    
    public void codeGen() {
        for (DeclNode node : myDecls) {
            node.codeGen();
        }
    }

    public void unparse(PrintWriter p, int indent) {
        Iterator it = myDecls.iterator();
        try {
            while (it.hasNext()) {
                ((DeclNode)it.next()).unparse(p, indent);
            }
        } catch (NoSuchElementException ex) {
            System.err.println("unexpected NoSuchElementException in DeclListNode.print");
            System.exit(-1);
        }
    }

    public int length() {
        return myDecls.size();
    }

    public int returnOffset() {
        return myOffset;
    }

    // list of kids (DeclNodes)
    private List<DeclNode> myDecls;
    public int noMain = 1;
    private int myOffset = 0;
    private int myGlobal = 0;
    private SemSym mySym;
}

class FormalsListNode extends ASTnode {
    public FormalsListNode(List<FormalDeclNode> S) {
        myFormals = S;
    }

    /**
     * nameAnalysis
     * Given a symbol table symTab, do:
     * for each formal decl in the list
     *     process the formal decl
     *     if there was no error, add type of formal decl to list
     */
    public List<Type> nameAnalysis(SymTable symTab) {
        List<Type> typeList = new LinkedList<Type>();
        for (FormalDeclNode node : myFormals) {
            SemSym sym = node.nameAnalysis(symTab);
            sym.setOffset(myOffset);
            myOffset += 4;
            if (sym != null) {
                typeList.add(sym.getType());
            }
        }
        return typeList;
    }    

    /**
     * Return the number of formals in this list.
     */
    public int returnOffset() {
        return myOffset;
    }

    public int length() {
        return myFormals.size();
    }
    
    public void unparse(PrintWriter p, int indent) {
        Iterator<FormalDeclNode> it = myFormals.iterator();
        if (it.hasNext()) { // if there is at least one element
            it.next().unparse(p, indent);
            while (it.hasNext()) {  // print the rest of the list
                p.print(", ");
                it.next().unparse(p, indent);
            }
        } 
    }

    // list of kids (FormalDeclNodes)
    private List<FormalDeclNode> myFormals;
    private int myOffset = 0;
}

class FnBodyNode extends ASTnode {
    public FnBodyNode(DeclListNode declList, StmtListNode stmtList) {
        myDeclList = declList;
        myStmtList = stmtList;
    }

    /**
     * nameAnalysis
     * Given a symbol table symTab, do:
     * - process the declaration list
     * - process the statement list
     */
    public void nameAnalysis(SymTable symTab, int offset) {
        myDeclList.nameAnalysis(symTab, 0, offset); //0 is for global. if myDeclList is in fnBody its not global
        myStmtList.nameAnalysis(symTab);
    }    
 
    /**
     * typeCheck
     */
    public void typeCheck(Type retType) {
        myStmtList.typeCheck(retType);
    }    
          
    public void partialEvaluate() {
        myStmtList.partialEvaluate();
    }    
          
    public void codeGen() {
        myStmtList.codeGen();
    }
    public void unparse(PrintWriter p, int indent) {
        myDeclList.unparse(p, indent);
        myStmtList.unparse(p, indent);
    }

    public int returnOffset() {
        return myDeclList.returnOffset();
    }

    // 2 kids
    private DeclListNode myDeclList;
    private StmtListNode myStmtList;
}

class StmtListNode extends ASTnode {
    public StmtListNode(List<StmtNode> S) {
        myStmts = S;
    }

    /**
     * nameAnalysis
     * Given a symbol table symTab, process each statement in the list.
     */
    public void nameAnalysis(SymTable symTab) {
        for (StmtNode node : myStmts) {
            node.nameAnalysis(symTab);
        }
    }    
    
    /**
     * typeCheck
     */
    public void typeCheck(Type retType) {
        for(StmtNode node : myStmts) {
            node.typeCheck(retType);
        }
    }
    
    public int partialEvaluate() {
        for(StmtNode node : myStmts) {
            if(node.partialEvaluate() == -1) { //TODO - verify functionality
                listStatic = -1;
            }
        }
        return listStatic;
    }

    public void setDynamic() {
        for(StmtNode node : myStmts) {
            node.setDynamic(); //TODO - verify functionality
        }
    }
    
    public void codeGen() {
        for(StmtNode node : myStmts) {
            node.codeGen();
        }
    }
    
    public void unparse(PrintWriter p, int indent) {
        Iterator<StmtNode> it = myStmts.iterator();
        while (it.hasNext()) {
            it.next().unparse(p, indent);
        }
    }

    // list of kids (StmtNodes)
    private List<StmtNode> myStmts;
    private int listStatic = 1;
}

class ExpListNode extends ASTnode {
    public ExpListNode(List<ExpNode> S) {
        myExps = S;
    }
    
    public int size() {
        return myExps.size();
    }
    
    /**
     * nameAnalysis
     * Given a symbol table symTab, process each exp in the list.
     */
    public void nameAnalysis(SymTable symTab) {
        for (ExpNode node : myExps) {
            node.nameAnalysis(symTab);
        }
    }
    
    /**
     * typeCheck
     */
    public void typeCheck(List<Type> typeList) {
        int k = 0;
        try {
            for (ExpNode node : myExps) {
                Type actualType = node.typeCheck();     // actual type of arg
                
                if (!actualType.isErrorType()) {        // if this is not an error
                    Type formalType = typeList.get(k);  // get the formal type
                    if (!formalType.equals(actualType)) {
                        ErrMsg.fatal(node.lineNum(), node.charNum(),
                                     "Type of actual does not match type of formal");
                    }
                }
                k++;
            }
        } catch (NoSuchElementException e) {
            System.err.println("unexpected NoSuchElementException in ExpListNode.typeCheck");
            System.exit(-1);
        }
    }

    public void partialEvaluate() {
        for (ExpNode node : myExps) {
            node.partialEvaluate();
        }
    }

    public void codeGen() {
        for (ExpNode node : myExps) {
            node.codeGen();
        }
    }
    
    public void unparse(PrintWriter p, int indent) {
        Iterator<ExpNode> it = myExps.iterator();
        if (it.hasNext()) { // if there is at least one element
            it.next().unparse(p, indent);
            while (it.hasNext()) {  // print the rest of the list
                p.print(", ");
                it.next().unparse(p, indent);
            }
        } 
    }

    // list of kids (ExpNodes)
    private List<ExpNode> myExps;
}

// **********************************************************************
// DeclNode and its subclasses
// **********************************************************************

abstract class DeclNode extends ASTnode {
    /**
     * Note: a formal decl needs to return a sym
     */
    abstract public SemSym nameAnalysis(SymTable symTab);

    abstract public String returnId();

    abstract public void setOffset(int offset);

    // default version of typeCheck for non-function decls
    public void typeCheck() { }

    public void partialEvaluate() { }

    public void codeGen() { }
}

class VarDeclNode extends DeclNode {
    public VarDeclNode(TypeNode type, IdNode id, int size) {
        myType = type;
        myId = id;
        mySize = size;
    }

    /**
     * nameAnalysis (overloaded)
     * Given a symbol table symTab, do:
     * if this name is declared void, then error
     * else if the declaration is of a struct type, 
     *     lookup type name (globally)
     *     if type name doesn't exist, then error
     * if no errors so far,
     *     if name has already been declared in this scope, then error
     *     else add name to local symbol table     
     *
     * symTab is local symbol table (say, for struct field decls)
     * globalTab is global symbol table (for struct type names)
     * symTab and globalTab can be the same
     */
    public SemSym nameAnalysis(SymTable symTab) {
        return nameAnalysis(symTab, symTab);
    }
    
    public SemSym nameAnalysis(SymTable symTab, SymTable globalTab, int global) { //it'll be called this way only from the global decllist
        myGlobal = global;
        if (myGlobal == 1) {
            myId.setGlobal();
        }
        return nameAnalysis(symTab, globalTab);
    }

    public SemSym nameAnalysis(SymTable symTab, SymTable globalTab) {
        boolean badDecl = false;
        String name = myId.name();
        SemSym sym = null;
        IdNode structId = null;

        if (myType instanceof VoidNode) {  // check for void type
            ErrMsg.fatal(myId.lineNum(), myId.charNum(), 
                         "Non-function declared void");
            badDecl = true;        
        }
        
        else if (myType instanceof StructNode) {
            structId = ((StructNode)myType).idNode();
            sym = globalTab.lookupGlobal(structId.name());
            
            // if the name for the struct type is not found, 
            // or is not a struct type
            if (sym == null || !(sym instanceof StructDefSym)) {
                ErrMsg.fatal(structId.lineNum(), structId.charNum(), 
                             "Invalid name of struct type");
                badDecl = true;
            }
            else {
                structId.link(sym);
            }
        }

        if (symTab.lookupLocal(name) != null) {
            ErrMsg.fatal(myId.lineNum(), myId.charNum(), 
                         "Multiply declared identifier");
            badDecl = true;            
        }
        
        if (!badDecl) {  // insert into symbol table
            try {
                if (myType instanceof StructNode) {
                    sym = new StructSym(structId);
                }
                else {
                    sym = new SemSym(myType.type());
                }
                symTab.addDecl(name, sym);
                myId.link(sym);
            } catch (DuplicateSymException ex) {
                System.err.println("Unexpected DuplicateSymException " +
                                   " in VarDeclNode.nameAnalysis");
                System.exit(-1);
            } catch (EmptySymTableException ex) {
                System.err.println("Unexpected EmptySymTableException " +
                                   " in VarDeclNode.nameAnalysis");
                System.exit(-1);
            }
        }
        
        return sym;
    }    

    public String returnId() {
        return myId.name();
    }

    public void setOffset(int offset) {
    }

    public void codeGen() {
        if(myGlobal == 1) {
            Codegen.generate(".data");
            Codegen.generate(".align 2");
            Codegen.generateLabeled("_" + myId.name(), ".space 4", "");
        }
    }

    public void unparse(PrintWriter p, int indent) {
        doIndent(p, indent);
        myType.unparse(p, 0);
        p.print(" ");
        p.print(myId.name());
        p.println(";");
    }

    // 3 kids
    private TypeNode myType;
    private IdNode myId;
    private int mySize;  // use value NOT_STRUCT if this is not a struct type

    public static int NOT_STRUCT = -1;
    //private int myOffset;
    private int myGlobal = 0;
}

class FnDeclNode extends DeclNode {
    public FnDeclNode(TypeNode type,
                      IdNode id,
                      FormalsListNode formalList,
                      FnBodyNode body) {
        myType = type;
        myId = id;
        myFormalsList = formalList;
        myBody = body;
    }

    /**
     * nameAnalysis
     * Given a symbol table symTab, do:
     * if this name has already been declared in this scope, then error
     * else add name to local symbol table
     * in any case, do the following:
     *     enter new scope
     *     process the formals
     *     if this function is not multiply declared,
     *         update symbol table entry with types of formals
     *     process the body of the function
     *     exit scope
     */
    public SemSym nameAnalysis(SymTable symTab) {
        String name = myId.name();
        FnSym sym = null;
        
        if (symTab.lookupLocal(name) != null) {
            ErrMsg.fatal(myId.lineNum(), myId.charNum(),
                         "Multiply declared identifier");
        }
        
        else { // add function name to local symbol table
            try {
                sym = new FnSym(myType.type(), myFormalsList.length());
                symTab.addDecl(name, sym);
                myId.link(sym);
            } catch (DuplicateSymException ex) {
                System.err.println("Unexpected DuplicateSymException " +
                                   " in FnDeclNode.nameAnalysis");
                System.exit(-1);
            } catch (EmptySymTableException ex) {
                System.err.println("Unexpected EmptySymTableException " +
                                   " in FnDeclNode.nameAnalysis");
                System.exit(-1);
            }
        }
        
        symTab.addScope();  // add a new scope for locals and params
        
        // process the formals
        List<Type> typeList = myFormalsList.nameAnalysis(symTab);
        if (sym != null) {
            sym.addFormals(typeList);
        }
       
        myFormalsSize = myFormalsList.returnOffset();
        myBody.nameAnalysis(symTab, myFormalsSize +8); // process the function body
        myLocalsSize = myBody.returnOffset();

        try {
            symTab.removeScope();  // exit scope
        } catch (EmptySymTableException ex) {
            System.err.println("Unexpected EmptySymTableException " +
                               " in FnDeclNode.nameAnalysis");
            System.exit(-1);
        }
        
        return null;
    } 

    public String returnId() {
        return myId.name();
    }

    public void setOffset(int offset) {
    }

    public int returnBodyOffset() {
        return myBody.returnOffset();
    }
       
    /**
     * typeCheck
     */
    public void typeCheck() {
        myBody.typeCheck(myType.type());
    }
        
    public void partialEvaluate() {
        myBody.partialEvaluate();
    }
        
    public void codeGen() {
        if(myId.name().matches("main")) {
            Codegen.generate(".text");
            Codegen.generate(".globl main");
            Codegen.genLabel("main");
            //Codegen.genLabel("__start");
        } else {
            Codegen.generate(".text");
            Codegen.genLabel("_"+myId.name());
        }
        Codegen.genPush(Codegen.RA);
        Codegen.genPush(Codegen.FP);
        Codegen.generate("addu", Codegen.FP, Codegen.SP, myFormalsSize + 8); //8 is for RA and FP
        Codegen.generate("subu", Codegen.SP, Codegen.SP, myLocalsSize);

        myBody.codeGen();

        //Codegen.genLabel(Codegen.nextLabel()); //for each return statement either write new code or jump to label
        Codegen.generateIndexed("lw", Codegen.RA, Codegen.FP, -myFormalsSize);//RA is stored at 8 mem locations lower than FP
        Codegen.generate("move", Codegen.T0, Codegen.FP);
        Codegen.generateIndexed("lw", Codegen.FP, Codegen.FP, -(myFormalsSize +4));
        Codegen.generate("move", Codegen.SP, Codegen.T0);
        if(myId.name().matches("main")) {
            Codegen.generate("li", Codegen.V0, 10);
            Codegen.generate("syscall");
        } else {
            Codegen.generate("jr", Codegen.RA);
        }
    }

    public void unparse(PrintWriter p, int indent) {
        doIndent(p, indent);
        myType.unparse(p, 0);
        p.print(" ");
        p.print(myId.name());
        p.print("(");
        myFormalsList.unparse(p, 0);
        p.println(") {");
        myBody.unparse(p, indent+4);
        p.println("}\n");
    }

    // 4 kids
    private TypeNode myType;
    private IdNode myId;
    private FormalsListNode myFormalsList;
    private FnBodyNode myBody;
    private int myFormalsSize;
    private int myLocalsSize;
    private int myGlobal = 0;
}

class FormalDeclNode extends DeclNode {
    public FormalDeclNode(TypeNode type, IdNode id) {
        myType = type;
        myId = id;
    }

    /**
     * nameAnalysis
     * Given a symbol table symTab, do:
     * if this formal is declared void, then error
     * else if this formal is already in the local symble table,
     *     then issue multiply declared error message and return null
     * else add a new entry to the symbol table and return that Sym
     */
    public SemSym nameAnalysis(SymTable symTab) {
        String name = myId.name();
        boolean badDecl = false;
        SemSym sym = null;
        
        if (myType instanceof VoidNode) {
            ErrMsg.fatal(myId.lineNum(), myId.charNum(), 
                         "Non-function declared void");
            badDecl = true;        
        }

        //if(myType instanceof StructNode) { //assuming formals cant be struct
        //    myOffset = (StructNode)myType.returnOffset();
        //} else {
        //    myOffset = 4;
        //}

        
        if (symTab.lookupLocal(name) != null) {
            ErrMsg.fatal(myId.lineNum(), myId.charNum(), 
                         "Multiply declared identifier");
            badDecl = true;
        }
        
        if (!badDecl) {  // insert into symbol table
            try {
                sym = new SemSym(myType.type());
                symTab.addDecl(name, sym);
                myId.link(sym);
            } catch (DuplicateSymException ex) {
                System.err.println("Unexpected DuplicateSymException " +
                                   " in VarDeclNode.nameAnalysis");
                System.exit(-1);
            } catch (EmptySymTableException ex) {
                System.err.println("Unexpected EmptySymTableException " +
                                   " in VarDeclNode.nameAnalysis");
                System.exit(-1);
            }
        }
        
        return sym;
    }    

    public String returnId() {
        return myId.name();
    }
    
    public void setOffset(int offset) {
    }

    public void codeGen() {
    }

    public void unparse(PrintWriter p, int indent) {
        myType.unparse(p, 0);
        p.print(" ");
        p.print(myId.name());
    }

    // 2 kids
    private TypeNode myType;
    private IdNode myId;
    private int myOffset = -1;
    private int myGlobal = 0;
}

class StructDeclNode extends DeclNode {
    public StructDeclNode(IdNode id, DeclListNode declList) {
        myId = id;
        myDeclList = declList;
    }

    /**
     * nameAnalysis
     * Given a symbol table symTab, do:
     * if this name is already in the symbol table,
     *     then multiply declared error (don't add to symbol table)
     * create a new symbol table for this struct definition
     * process the decl list
     * if no errors
     *     add a new entry to symbol table for this struct
     */
    public SemSym nameAnalysis(SymTable symTab) {
        String name = myId.name();
        boolean badDecl = false;
        
        if (symTab.lookupLocal(name) != null) {
            ErrMsg.fatal(myId.lineNum(), myId.charNum(), 
                         "Multiply declared identifier");
            badDecl = true;            
        }

        SymTable structSymTab = new SymTable();
        
        // process the fields of the struct
        myDeclList.nameAnalysis(structSymTab, symTab);
        //myOffset = myDeclList.returnOffset();
        
        if (!badDecl) {
            try {   // add entry to symbol table
                StructDefSym sym = new StructDefSym(structSymTab);
                symTab.addDecl(name, sym);
                myId.link(sym);
            } catch (DuplicateSymException ex) {
                System.err.println("Unexpected DuplicateSymException " +
                                   " in StructDeclNode.nameAnalysis");
                System.exit(-1);
            } catch (EmptySymTableException ex) {
                System.err.println("Unexpected EmptySymTableException " +
                                   " in StructDeclNode.nameAnalysis");
                System.exit(-1);
            }
        }
        
        return null;
    }    

    public String returnId() {
        return myId.name();
    }
    
    public void setOffset(int offset) {
    }
    
    public void codeGen() {
    }

    public void unparse(PrintWriter p, int indent) {
        doIndent(p, indent);
        p.print("struct ");
        p.print(myId.name());
        p.println("{");
        myDeclList.unparse(p, indent+4);
        doIndent(p, indent);
        p.println("};\n");

    }

    // 2 kids
    private IdNode myId;
    private DeclListNode myDeclList;
    private int myOffset;
    private int myGlobal = 0;
}

// **********************************************************************
// TypeNode and its Subclasses
// **********************************************************************

abstract class TypeNode extends ASTnode {
    /* all subclasses must provide a type method */
    abstract public Type type();
}

class IntNode extends TypeNode {
    public IntNode() {
    }

    /**
     * type
     */
    public Type type() {
        return new IntType();
    }
    
    public void unparse(PrintWriter p, int indent) {
        p.print("int");
    }
}

class BoolNode extends TypeNode {
    public BoolNode() {
    }

    /**
     * type
     */
    public Type type() {
        return new BoolType();
    }
    
    public void unparse(PrintWriter p, int indent) {
        p.print("bool");
    }
}

class VoidNode extends TypeNode {
    public VoidNode() {
    }
    
    /**
     * type
     */
    public Type type() {
        return new VoidType();
    }
    
    public void unparse(PrintWriter p, int indent) {
        p.print("void");
    }
}

class StructNode extends TypeNode {
    public StructNode(IdNode id) {
        myId = id;
    }

    public IdNode idNode() {
        return myId;
    }
    
    /**
     * type
     */
    public Type type() {
        return new StructType(myId);
    }
    
    public void unparse(PrintWriter p, int indent) {
        p.print("struct ");
        p.print(myId.name());
    }
    
    // 1 kid
    private IdNode myId;
}

// **********************************************************************
// StmtNode and its subclasses
// **********************************************************************

abstract class StmtNode extends ASTnode {
    public void setDynamic() {};
    abstract public void nameAnalysis(SymTable symTab);
    abstract public void typeCheck(Type retType);
    abstract public int partialEvaluate();
    abstract public void codeGen();
}

class AssignStmtNode extends StmtNode {
    public AssignStmtNode(AssignNode assign) {
        myAssign = assign;
    }

    /**
     * nameAnalysis
     * Given a symbol table symTab, perform name analysis on this node's child
     */
    public void nameAnalysis(SymTable symTab) {
        myAssign.nameAnalysis(symTab);
    }
    
    /**
     * typeCheck
     */
    public void typeCheck(Type retType) {
        myAssign.typeCheck();
    }
        
    public int partialEvaluate() {
        return myAssign.partialEvaluate();
    }

    public void setDynamic() {
        myAssign.setDynamic();
    }

    public void codeGen() {
        myAssign.codeGen();
        Codegen.genPop(Codegen.T0);
    }

    public void unparse(PrintWriter p, int indent) {
        doIndent(p, indent);
        myAssign.unparse(p, -1); // no parentheses
        p.println(";");
    }

    // 1 kid
    private AssignNode myAssign;
}

class PostIncStmtNode extends StmtNode {
    public PostIncStmtNode(ExpNode exp) {
        myExp = exp;
    }
    
    /**
     * nameAnalysis
     * Given a symbol table symTab, perform name analysis on this node's child
     */
    public void nameAnalysis(SymTable symTab) {
        myExp.nameAnalysis(symTab);
    }
    
    /**
     * typeCheck
     */
    public void typeCheck(Type retType) {
        Type type = myExp.typeCheck();
        
        if (!type.isErrorType() && !type.isIntType()) {
            ErrMsg.fatal(myExp.lineNum(), myExp.charNum(),
                         "Arithmetic operator applied to non-numeric operand");
        }
    }
        
    public int partialEvaluate() {
        valNode = myExp.partialEvaluate();
        if(valNode != -1) {
            myExp.storeVal(valNode);
            myExp.setStatic(1);
            return 1;
        } else {
            myExp.setStatic(0);
            return -1;
        }
    }

    public void setDynamic() {
        myExp.setStatic(0);
    }

    public void codeGen() {
        if (myExp.getStatic() != 1) { //EDIT
            myExp.codeGen();
        } else {
            Codegen.generate("li", Codegen.T0, valNode);
            Codegen.genPush(Codegen.T0);
        }
        Codegen.generate("li", Codegen.T1, 1);
        Codegen.generate("add", Codegen.T0, Codegen.T0, Codegen.T1);
        Codegen.genPush(Codegen.T0);
        myExp.genAddr(); //at this point addr is on top, value below it on stack
        Codegen.genPop(Codegen.T1); //addr popped 
        Codegen.genPop(Codegen.T0); //value popped
        Codegen.generateIndexed("sw", Codegen.T0, Codegen.T1, 0);
    }

    public void unparse(PrintWriter p, int indent) {
        doIndent(p, indent);
        myExp.unparse(p, 0);
        p.println("++;");
    }

    // 1 kid
    private ExpNode myExp;
    //EDIT
    private int valNode;
}

class PostDecStmtNode extends StmtNode {
    public PostDecStmtNode(ExpNode exp) {
        myExp = exp;
    }

    /**
     * nameAnalysis
     * Given a symbol table symTab, perform name analysis on this node's child
     */
    public void nameAnalysis(SymTable symTab) {
        myExp.nameAnalysis(symTab);
    }
    
    /**
     * typeCheck
     */
    public void typeCheck(Type retType) {
        Type type = myExp.typeCheck();
        
        if (!type.isErrorType() && !type.isIntType()) {
            ErrMsg.fatal(myExp.lineNum(), myExp.charNum(),
                         "Arithmetic operator applied to non-numeric operand");
        }
    }
        
    public int partialEvaluate() {
        valNode = myExp.partialEvaluate();
        if(valNode != -1) {
            myExp.storeVal(valNode);
            myExp.setStatic(1);
            return 1;
        } else {
            myExp.setStatic(0);
            return -1;
        }
    }

    public void setDynamic() {
        myExp.setStatic(0);
    }

    public void codeGen() {
        if (myExp.getStatic() != 1) { //EDIT
            myExp.codeGen();
        } else {
            Codegen.generate("li", Codegen.T0, valNode);
            Codegen.genPush(Codegen.T0);
        }
        Codegen.generate("li", Codegen.T1, -1);
        Codegen.generate("add", Codegen.T0, Codegen.T0, Codegen.T1);
        Codegen.genPush(Codegen.T0);
        myExp.genAddr(); //at this point addr is on top, value below it on stack
        Codegen.genPop(Codegen.T1); //addr popped 
        Codegen.genPop(Codegen.T0); //value popped
        Codegen.generateIndexed("sw", Codegen.T0, Codegen.T1, 0);
    }

    public void unparse(PrintWriter p, int indent) {
        doIndent(p, indent);
        myExp.unparse(p, 0);
        p.println("--;");
    }
    
    // 1 kid
    private ExpNode myExp;
    //EDIT
    private int valNode;
}

class ReadStmtNode extends StmtNode {
    public ReadStmtNode(ExpNode e) {
        myExp = e;
    }

    /**
     * nameAnalysis
     * Given a symbol table symTab, perform name analysis on this node's child
     */
    public void nameAnalysis(SymTable symTab) {
        myExp.nameAnalysis(symTab);
    }    
 
    /**
     * typeCheck
     */
    public void typeCheck(Type retType) {
        Type type = myExp.typeCheck();
        
        if (type.isFnType()) {
            ErrMsg.fatal(myExp.lineNum(), myExp.charNum(),
                         "Attempt to read a function");
        }
        
        if (type.isStructDefType()) {
            ErrMsg.fatal(myExp.lineNum(), myExp.charNum(),
                         "Attempt to read a struct name");
        }
        
        if (type.isStructType()) {
            ErrMsg.fatal(myExp.lineNum(), myExp.charNum(),
                         "Attempt to read a struct variable");
        }
    }
    
    public int partialEvaluate() {
        myExp.setStatic(0);
        return -1;
    }

    public void codeGen() {
        Codegen.generate("li", Codegen.V0, 5);
        Codegen.generate("syscall");
        myExp.genAddr();
        Codegen.genPop(Codegen.T0);
        Codegen.generateIndexed("sw", Codegen.V0, Codegen.T0, 0);
    }

    public void unparse(PrintWriter p, int indent) {
        doIndent(p, indent);
        p.print("cin >> ");
        myExp.unparse(p, 0);
        p.println(";");
    }

    // 1 kid (actually can only be an IdNode or an ArrayExpNode)
    private ExpNode myExp;
}

class WriteStmtNode extends StmtNode {
    public WriteStmtNode(ExpNode exp) {
        myExp = exp;
    }

    /**
     * nameAnalysis
     * Given a symbol table symTab, perform name analysis on this node's child
     */
    public void nameAnalysis(SymTable symTab) {
        myExp.nameAnalysis(symTab);
    }

    /**
     * typeCheck
     */
    public void typeCheck(Type retType) {
        Type type = myExp.typeCheck();
        myType = type;
        
        if (type.isFnType()) {
            ErrMsg.fatal(myExp.lineNum(), myExp.charNum(),
                         "Attempt to write a function");
        }
        
        if (type.isStructDefType()) {
            ErrMsg.fatal(myExp.lineNum(), myExp.charNum(),
                         "Attempt to write a struct name");
        }
        
        if (type.isStructType()) {
            ErrMsg.fatal(myExp.lineNum(), myExp.charNum(),
                         "Attempt to write a struct variable");
        }
        
        if (type.isVoidType()) {
            ErrMsg.fatal(myExp.lineNum(), myExp.charNum(),
                         "Attempt to write void");
        }
    }

    public int partialEvaluate() {
        valNode = myExp.partialEvaluate();
        //if myExp is of type IdNode, it should work fine
        if(valNode != -1) {
            myExp.storeVal(valNode);
            myExp.setStatic(1);
        } else {
            myExp.setStatic(0);
        }
        return 1; //since cout is not changing anything, assuming its always partially-evaluatable
    }

    public void setDynamic() {
        myExp.setStatic(0);
    }

    public void codeGen() {
        if (myExp.getStatic() != 1) { //EDIT
            myExp.codeGen();
            Codegen.genPop(Codegen.A0); 
        } else {
            Codegen.generate("li", Codegen.A0, valNode);
        }
        if (myExp instanceof StringLitNode) {
            Codegen.generate("li", Codegen.V0, 4);
        } else {
            Codegen.generate("li", Codegen.V0, 1);
        }
        Codegen.generate("syscall");
    }

    public void unparse(PrintWriter p, int indent) {
        doIndent(p, indent);
        p.print("cout << ");
        myExp.unparse(p, 0);
        p.println(";");
    }

    // 1 kid
    private ExpNode myExp;
    private Type myType;
    //EDIT
    private int valNode;
}

class IfStmtNode extends StmtNode {
    public IfStmtNode(ExpNode exp, DeclListNode dlist, StmtListNode slist) {
        myDeclList = dlist;
        myExp = exp;
        myStmtList = slist;
    }
    
    /**
     * nameAnalysis
     * Given a symbol table symTab, do:
     * - process the condition
     * - enter a new scope
     * - process the decls and stmts
     * - exit the scope
     */
    public void nameAnalysis(SymTable symTab) {
        myExp.nameAnalysis(symTab);
        symTab.addScope();
        myDeclList.nameAnalysis(symTab);
        myStmtList.nameAnalysis(symTab);
        try {
            symTab.removeScope();
        } catch (EmptySymTableException ex) {
            System.err.println("Unexpected EmptySymTableException " +
                               " in IfStmtNode.nameAnalysis");
            System.exit(-1);        
        }
    }
    
     /**
     * typeCheck
     */
    public int partialEvaluate() {
        valNode = myExp.partialEvaluate();
        if(valNode == 1) {
            //myExp.storeVal(valNode);
            myStmtList.partialEvaluate();
            myExp.setStatic(1);
        } else if (valNode == 0) {
            myExp.setStatic(1);
        } else {
            myExp.setStatic(0);
        }
        return 1;
        //TODO - have to perform PE on all children first (to be able to say entire loop is evaluatable
        //similar to while loop
    }

    public void setDynamic() {
        myExp.setStatic(0);
        myStmtList.setDynamic();
    }

    public void typeCheck(Type retType) {
        Type type = myExp.typeCheck();
        
        if (!type.isErrorType() && !type.isBoolType()) {
            ErrMsg.fatal(myExp.lineNum(), myExp.charNum(),
                         "Non-bool expression used as an if condition");        
        }
        
        myStmtList.typeCheck(retType);
    }
       
    public void codeGen() {
        String label = Codegen.nextLabel();

        if (myExp.getStatic() != 1) { //EDIT
            myExp.codeGen();
        } else {
            Codegen.generate("li", Codegen.T0, valNode);
        }
        //Some nodes just assume (rightly so) that the value is in T0, so dont explicitly pop
        Codegen.generate("beq", Codegen.T0, Codegen.FALSE, label);

        myStmtList.codeGen();

        Codegen.genLabel(label);
    }

    public void unparse(PrintWriter p, int indent) {
        doIndent(p, indent);
        p.print("if (");
        myExp.unparse(p, 0);
        p.println(") {");
        myDeclList.unparse(p, indent+4);
        myStmtList.unparse(p, indent+4);
        doIndent(p, indent);
        p.println("}");
    }

    // e kids
    private ExpNode myExp;
    private DeclListNode myDeclList;
    private StmtListNode myStmtList;
    //EDIT
    private int valNode;
}

class IfElseStmtNode extends StmtNode {
    public IfElseStmtNode(ExpNode exp, DeclListNode dlist1,
                          StmtListNode slist1, DeclListNode dlist2,
                          StmtListNode slist2) {
        myExp = exp;
        myThenDeclList = dlist1;
        myThenStmtList = slist1;
        myElseDeclList = dlist2;
        myElseStmtList = slist2;
    }
    
    /**
     * nameAnalysis
     * Given a symbol table symTab, do:
     * - process the condition
     * - enter a new scope
     * - process the decls and stmts of then
     * - exit the scope
     * - enter a new scope
     * - process the decls and stmts of else
     * - exit the scope
     */
    public void nameAnalysis(SymTable symTab) {
        myExp.nameAnalysis(symTab);
        symTab.addScope();
        myThenDeclList.nameAnalysis(symTab);
        myThenStmtList.nameAnalysis(symTab);
        try {
            symTab.removeScope();
        } catch (EmptySymTableException ex) {
            System.err.println("Unexpected EmptySymTableException " +
                               " in IfStmtNode.nameAnalysis");
            System.exit(-1);        
        }
        symTab.addScope();
        myElseDeclList.nameAnalysis(symTab);
        myElseStmtList.nameAnalysis(symTab);
        try {
            symTab.removeScope();
        } catch (EmptySymTableException ex) {
            System.err.println("Unexpected EmptySymTableException " +
                               " in IfStmtNode.nameAnalysis");
            System.exit(-1);        
        }
    }
    
    /**
     * typeCheck
     */
    public void typeCheck(Type retType) {
        Type type = myExp.typeCheck();
        
        if (!type.isErrorType() && !type.isBoolType()) {
            ErrMsg.fatal(myExp.lineNum(), myExp.charNum(),
                         "Non-bool expression used as an if condition");        
        }
        
        myThenStmtList.typeCheck(retType);
        myElseStmtList.typeCheck(retType);
    }
        
    public int partialEvaluate() {
        valNode = myExp.partialEvaluate();
        if(valNode != -1) {
            myExp.storeVal(valNode);
            myExp.setStatic(1);
        } else {
            myExp.setStatic(0);
        }
        return 1;
        //TODO again
    }

    public void setDynamic() {
        myExp.setStatic(0);
        myThenStmtList.setDynamic();
        myElseStmtList.setDynamic();
    }

    public void codeGen() {
        String elseLabel = Codegen.nextLabel();
        String doneLabel = Codegen.nextLabel();

        if (myExp.getStatic() != 1) { //EDIT
            myExp.codeGen();
            Codegen.genPop(Codegen.T0);
        } else {
            Codegen.generate("li", Codegen.T0, valNode);
        }
        Codegen.generate("beq", Codegen.T0, Codegen.FALSE, elseLabel);

        myThenStmtList.codeGen();
        Codegen.generate("b", doneLabel);

        Codegen.genLabel(elseLabel);
        myElseStmtList.codeGen();

        Codegen.genLabel(doneLabel);
    }

    public void unparse(PrintWriter p, int indent) {
        doIndent(p, indent);
        p.print("if (");
        myExp.unparse(p, 0);
        p.println(") {");
        myThenDeclList.unparse(p, indent+4);
        myThenStmtList.unparse(p, indent+4);
        doIndent(p, indent);
        p.println("}");
        doIndent(p, indent);
        p.println("else {");
        myElseDeclList.unparse(p, indent+4);
        myElseStmtList.unparse(p, indent+4);
        doIndent(p, indent);
        p.println("}");        
    }

    // 5 kids
    private ExpNode myExp;
    private DeclListNode myThenDeclList;
    private StmtListNode myThenStmtList;
    private StmtListNode myElseStmtList;
    private DeclListNode myElseDeclList;
    //EDIT
    private int valNode;
}

class WhileStmtNode extends StmtNode {
    public WhileStmtNode(ExpNode exp, DeclListNode dlist, StmtListNode slist) {
        myExp = exp;
        myDeclList = dlist;
        myStmtList = slist;
    }
    
    /**
     * nameAnalysis
     * Given a symbol table symTab, do:
     * - process the condition
     * - enter a new scope
     * - process the decls and stmts
     * - exit the scope
     */
    public void nameAnalysis(SymTable symTab) {
        myExp.nameAnalysis(symTab);
        symTab.addScope();
        myDeclList.nameAnalysis(symTab);
        myStmtList.nameAnalysis(symTab);
        try {
            symTab.removeScope();
        } catch (EmptySymTableException ex) {
            System.err.println("Unexpected EmptySymTableException " +
                               " in IfStmtNode.nameAnalysis");
            System.exit(-1);        
        }
    }
    
    /**
     * typeCheck
     */
    public void typeCheck(Type retType) {
        Type type = myExp.typeCheck();
        
        if (!type.isErrorType() && !type.isBoolType()) {
            ErrMsg.fatal(myExp.lineNum(), myExp.charNum(),
                         "Non-bool expression used as a while condition");        
        }
        
        myStmtList.typeCheck(retType);}
        
    public int partialEvaluate() {
        valNode = myExp.partialEvaluate();
        initialVal = valNode;
        while (valNode == 1) {
            if(myStmtList.partialEvaluate() == -1)
                loopStatic = -1;
            else
                loopStatic = 1;
            valNode = myExp.partialEvaluate();
        }
        if(loopStatic == -1) { //set all LHS IDs as dynamic so that codegen doesn't use PEd values
            myExp.setDynamic();
            myStmtList.setDynamic();
        }
        return 1;
    }

    public void setDynamic() {
        myExp.setStatic(0);
        myStmtList.setDynamic();
    }

    public void codeGen() {
        if (loopStatic == 1) { //eliminate the loop and just write everything only once
            if (initialVal == 1)
                myStmtList.codeGen();
        } else {
            //if (myExp.getStatic() != 1) { //EDIT
                myExp.codeGen();
                Codegen.genPop(Codegen.T0);
            //} else {
            //    Codegen.generate("li", Codegen.T0, valNode);
            //}
            String startlabel = Codegen.nextLabel();
            String endlabel = Codegen.nextLabel();
            Codegen.generate("beq", Codegen.T0, Codegen.FALSE, endlabel);
            //iteration start
            Codegen.genLabel(startlabel);
            myStmtList.codeGen();
            //one iteration done, check again
            myExp.codeGen();
            Codegen.genPop(Codegen.T0);
            Codegen.generate("bne", Codegen.T0, Codegen.FALSE, startlabel);
            //end
            Codegen.genLabel(endlabel);
        }
    }

    public void unparse(PrintWriter p, int indent) {
        doIndent(p, indent);
        p.print("while (");
        myExp.unparse(p, 0);
        p.println(") {");
        myDeclList.unparse(p, indent+4);
        myStmtList.unparse(p, indent+4);
        doIndent(p, indent);
        p.println("}");
    }

    // 3 kids
    private ExpNode myExp;
    private DeclListNode myDeclList;
    private StmtListNode myStmtList;
    //EDIT
    private int valNode = -1;
    private int initialVal = -1;
    private int loopStatic = -1;
}

class CallStmtNode extends StmtNode {
    public CallStmtNode(CallExpNode call) {
        myCall = call;
    }
    
    /**
     * nameAnalysis
     * Given a symbol table symTab, perform name analysis on this node's child
     */
    public void nameAnalysis(SymTable symTab) {
        myCall.nameAnalysis(symTab);
    }
    
    /**
     * typeCheck
     */
    public void typeCheck(Type retType) {
        myCall.typeCheck();
    }
    
    public int partialEvaluate() {
        return -1;
        //TODO - not worrying about this for now
        //TODO - perhaps the most important
    }

    public void codeGen() {
        myCall.codeGen(); //callexpnode leaves return value (which may be garbage) on stack
        Codegen.genPop(Codegen.T0); //pop it and leave it?
    }

    public void unparse(PrintWriter p, int indent) {
        doIndent(p, indent);
        myCall.unparse(p, indent);
        p.println(";");
    }

    // 1 kid
    private CallExpNode myCall;
}

class ReturnStmtNode extends StmtNode {
    public ReturnStmtNode(ExpNode exp) {
        myExp = exp;
    }
    
    /**
     * nameAnalysis
     * Given a symbol table symTab, perform name analysis on this node's child,
     * if it has one
     */
    public void nameAnalysis(SymTable symTab) {
        if (myExp != null) {
            myExp.nameAnalysis(symTab);
        }
    }

    /**
     * typeCheck
     */
    public void typeCheck(Type retType) {
        if (myExp != null) {  // return value given
            Type type = myExp.typeCheck();
            
            if (retType.isVoidType()) {
                ErrMsg.fatal(myExp.lineNum(), myExp.charNum(),
                             "Return with a value in a void function");                
            }
            
            else if (!retType.isErrorType() && !type.isErrorType() && !retType.equals(type)){
                ErrMsg.fatal(myExp.lineNum(), myExp.charNum(),
                             "Bad return value");
            }
        }
        
        else {  // no return value given -- ok if this is a void function
            if (!retType.isVoidType()) {
                ErrMsg.fatal(0, 0, "Missing return value");                
            }
        }
        
    }
    
    public int partialEvaluate() {
        return -1;
        //TODO
    }

    public void codeGen() {
        myExp.codeGen();
        Codegen.genPop(Codegen.V0);
    }

    public void unparse(PrintWriter p, int indent) {
        doIndent(p, indent);
        p.print("return");
        if (myExp != null) {
            p.print(" ");
            myExp.unparse(p, 0);
        }
        p.println(";");
    }

    // 1 kid
    private ExpNode myExp; // possibly null
}

// **********************************************************************
// ExpNode and its subclasses
// **********************************************************************

abstract class ExpNode extends ASTnode {
    /**
     * Default version for nodes with no names
     */
    public void nameAnalysis(SymTable symTab) { }

    public void genAddr() {}

    //EDIT
    public void storeVal(int val) {}
    public int getStatic() {return 0;}
    public void setStatic(int val) {}
    public void setDynamic() {}
    
    abstract public Type typeCheck();
    //EDIT - what type should it have?
    abstract public int partialEvaluate();
    abstract public void codeGen();
    abstract public int lineNum();
    abstract public int charNum();
}

class IntLitNode extends ExpNode {
    public IntLitNode(int lineNum, int charNum, int intVal) {
        myLineNum = lineNum;
        myCharNum = charNum;
        myIntVal = intVal;
    }
    
    /**
     * Return the line number for this literal.
     */
    public int lineNum() {
        return myLineNum;
    }
    
    /**
     * Return the char number for this literal.
     */
    public int charNum() {
        return myCharNum;
    }
        
    /**
     * typeCheck
     */
    public Type typeCheck() {
        return new IntType();
    }
    
    public int partialEvaluate() {
        return myIntVal;
    }

    public void codeGen() {
        Codegen.generate("li", Codegen.T0, myIntVal);
        Codegen.genPush(Codegen.T0);
    }

    public void unparse(PrintWriter p, int indent) {
        p.print(myIntVal);
    }

    private int myLineNum;
    private int myCharNum;
    private int myIntVal;
}

class StringLitNode extends ExpNode {
    public StringLitNode(int lineNum, int charNum, String strVal) {
        myLineNum = lineNum;
        myCharNum = charNum;
        myStrVal = strVal;
    }
    
    /**
     * Return the line number for this literal.
     */
    public int lineNum() {
        return myLineNum;
    }
    
    /**
     * Return the char number for this literal.
     */
    public int charNum() {
        return myCharNum;
    }
    
    /**
     * typeCheck
     */
    public Type typeCheck() {
        return new StringType();
    }
        
    public int partialEvaluate() {
        return -1;
    }

    public void codeGen() {
        Codegen.generate(".data");
        String label = Codegen.nextLabel();
        Codegen.generateLabeled(label, ".asciiz ", "", myStrVal);
        Codegen.generate(".text");
        Codegen.generate("la", Codegen.T0, label); //if I store add here, when will I get the value?
        Codegen.genPush(Codegen.T0);
    }

    public void unparse(PrintWriter p, int indent) {
        p.print(myStrVal);
    }

    private int myLineNum;
    private int myCharNum;
    private String myStrVal;
}

class TrueNode extends ExpNode {
    public TrueNode(int lineNum, int charNum) {
        myLineNum = lineNum;
        myCharNum = charNum;
    }

    /**
     * Return the line number for this literal.
     */
    public int lineNum() {
        return myLineNum;
    }
    
    /**
     * Return the char number for this literal.
     */
    public int charNum() {
        return myCharNum;
    }
    
    /**
     * typeCheck
     */
    public Type typeCheck() {
        return new BoolType();
    }
        
    public int partialEvaluate() {
        return 1;
    }

    public void codeGen() {
        Codegen.generate("li", Codegen.T0, Codegen.TRUE);
        Codegen.genPush(Codegen.T0);
    }

    public void unparse(PrintWriter p, int indent) {
        p.print("true");
    }

    private int myLineNum;
    private int myCharNum;
}

class FalseNode extends ExpNode {
    public FalseNode(int lineNum, int charNum) {
        myLineNum = lineNum;
        myCharNum = charNum;
    }

    /**
     * Return the line number for this literal.
     */
    public int lineNum() {
        return myLineNum;
    }
    
    /**
     * Return the char number for this literal.
     */
    public int charNum() {
        return myCharNum;
    }

    /**
     * typeCheck
     */
    public Type typeCheck() {
        return new BoolType();
    }
        
    public int partialEvaluate() {
        return 0;
    }

    public void codeGen() {
        Codegen.generate("li", Codegen.T0, Codegen.FALSE);
        Codegen.genPush(Codegen.T0);
    }

    public void unparse(PrintWriter p, int indent) {
        p.print("false");
    }

    private int myLineNum;
    private int myCharNum;
}

class IdNode extends ExpNode {
    public IdNode(int lineNum, int charNum, String strVal) {
        myLineNum = lineNum;
        myCharNum = charNum;
        myStrVal = strVal;
    }

    /**
     * Link the given symbol to this ID.
     */
    public void link(SemSym sym) {
        mySym = sym;
    }
    
    /**
     * Return the name of this ID.
     */
    public String name() {
        return myStrVal;
    }
    
    /**
     * Return the symbol associated with this ID.
     */
    public SemSym sym() {
        return mySym;
    }
    
    /**
     * Return the line number for this ID.
     */
    public int lineNum() {
        return myLineNum;
    }
    
    /**
     * Return the char number for this ID.
     */
    public int charNum() {
        return myCharNum;
    }    
    
    /**
     * nameAnalysis
     * Given a symbol table symTab, do:
     * - check for use of undeclared name
     * - if ok, link to symbol table entry
     */
    public void nameAnalysis(SymTable symTab) {
        SemSym sym = symTab.lookupGlobal(myStrVal);
        if (sym == null) {
            ErrMsg.fatal(myLineNum, myCharNum, "Undeclared identifier");
        } else {
            link(sym);
        }
    }
 
    /**
     * typeCheck
     */
    public Type typeCheck() {
        if (mySym != null) {
            return mySym.getType();
        } 
        else {
            System.err.println("ID with null sym field in IdNode.typeCheck");
            System.exit(-1);
        }
        return null;
    }
           
    public int partialEvaluate() {
        if (mySym.getStatic() == 1) {
            return mySym.loadVal();
        } else {
            return -1;
        }
    }

    public void storeVal(int val) {
        mySym.storeVal(val);
    }

    public void setStatic(int val) {
        mySym.setStatic(val);
    }

    public int getStatic() {
        return mySym.getStatic();
    }

    public void codeGen() {
        if(mySym.isGlobal == 1) {
            Codegen.generate("lw", Codegen.T0, "_" + name());
            Codegen.genPush(Codegen.T0);
        } else {
            Codegen.generateIndexed("lw", Codegen.T0, Codegen.FP, -1*mySym.returnOffset()); 
            Codegen.genPush(Codegen.T0);
        }
    }

    public void genJumpAndLink() {
        if(myStrVal.matches("main")) {
            Codegen.generate("jal", myStrVal); //does this just execute the next func? Its params will already be in the stack right?
        } else {
            //Code reaches here
            Codegen.generate("jal", "_" + myStrVal);
        }
    }

    public void genAddr() {
        if(mySym.isGlobal == 1) {
            Codegen.generateWithComment("la", "copy address into register", Codegen.T0, "_" + name());
            Codegen.genPush(Codegen.T0);
        } else {
            Codegen.generateIndexed("la", Codegen.T0, Codegen.FP, -1*mySym.returnOffset());
            Codegen.genPush(Codegen.T0);
        }
    }

    public void setGlobal() {
        myGlobal = 1;
    }

    public void unparse(PrintWriter p, int indent) {
        p.print(myStrVal);
        if (mySym != null) {
            p.print("(" + mySym + ")");
        }
    }

    private int myLineNum;
    private int myCharNum;
    private String myStrVal;
    private SemSym mySym;
    private int myGlobal = 0;
}

class DotAccessExpNode extends ExpNode {
    public DotAccessExpNode(ExpNode loc, IdNode id) {
        myLoc = loc;    
        myId = id;
        mySym = null;
    }

    /**
     * Return the symbol associated with this dot-access node.
     */
    public SemSym sym() {
        return mySym;
    }    
    
    /**
     * Return the line number for this dot-access node. 
     * The line number is the one corresponding to the RHS of the dot-access.
     */
    public int lineNum() {
        return myId.lineNum();
    }
    
    /**
     * Return the char number for this dot-access node.
     * The char number is the one corresponding to the RHS of the dot-access.
     */
    public int charNum() {
        return myId.charNum();
    }
    
    /**
     * nameAnalysis
     * Given a symbol table symTab, do:
     * - process the LHS of the dot-access
     * - process the RHS of the dot-access
     * - if the RHS is of a struct type, set the sym for this node so that
     *   a dot-access "higher up" in the AST can get access to the symbol
     *   table for the appropriate struct definition
     */
    public void nameAnalysis(SymTable symTab) {
        badAccess = false;
        SymTable structSymTab = null; // to lookup RHS of dot-access
        SemSym sym = null;
        
        myLoc.nameAnalysis(symTab);  // do name analysis on LHS
        
        // if myLoc is really an ID, then sym will be a link to the ID's symbol
        if (myLoc instanceof IdNode) {
            IdNode id = (IdNode)myLoc;
            sym = id.sym();
            
            // check ID has been declared to be of a struct type
            
            if (sym == null) { // ID was undeclared
                badAccess = true;
            }
            else if (sym instanceof StructSym) { 
                // get symbol table for struct type
                SemSym tempSym = ((StructSym)sym).getStructType().sym();
                structSymTab = ((StructDefSym)tempSym).getSymTable();
            } 
            else {  // LHS is not a struct type
                ErrMsg.fatal(id.lineNum(), id.charNum(), 
                             "Dot-access of non-struct type");
                badAccess = true;
            }
        }
        
        // if myLoc is really a dot-access (i.e., myLoc was of the form
        // LHSloc.RHSid), then sym will either be
        // null - indicating RHSid is not of a struct type, or
        // a link to the Sym for the struct type RHSid was declared to be
        else if (myLoc instanceof DotAccessExpNode) {
            DotAccessExpNode loc = (DotAccessExpNode)myLoc;
            
            if (loc.badAccess) {  // if errors in processing myLoc
                badAccess = true; // don't continue proccessing this dot-access
            }
            else { //  no errors in processing myLoc
                sym = loc.sym();

                if (sym == null) {  // no struct in which to look up RHS
                    ErrMsg.fatal(loc.lineNum(), loc.charNum(), 
                                 "Dot-access of non-struct type");
                    badAccess = true;
                }
                else {  // get the struct's symbol table in which to lookup RHS
                    if (sym instanceof StructDefSym) {
                        structSymTab = ((StructDefSym)sym).getSymTable();
                    }
                    else {
                        System.err.println("Unexpected Sym type in DotAccessExpNode");
                        System.exit(-1);
                    }
                }
            }

        }
        
        else { // don't know what kind of thing myLoc is
            System.err.println("Unexpected node type in LHS of dot-access");
            System.exit(-1);
        }
        
        // do name analysis on RHS of dot-access in the struct's symbol table
        if (!badAccess) {
        
            sym = structSymTab.lookupGlobal(myId.name()); // lookup
            if (sym == null) { // not found - RHS is not a valid field name
                ErrMsg.fatal(myId.lineNum(), myId.charNum(), 
                             "Invalid struct field name");
                badAccess = true;
            }
            
            else {
                myId.link(sym);  // link the symbol
                // if RHS is itself as struct type, link the symbol for its struct 
                // type to this dot-access node (to allow chained dot-access)
                if (sym instanceof StructSym) {
                    mySym = ((StructSym)sym).getStructType().sym();
                }
            }
        }
    }    
 
    /**
     * typeCheck
     */
    public int partialEvaluate() {
        return -1;
    }

    public Type typeCheck() {
        return myId.typeCheck();
    }
    
    public void codeGen() {
        //TODO - extra credit
    }

    public void unparse(PrintWriter p, int indent) {
        myLoc.unparse(p, 0);
        p.print(".");
        myId.unparse(p, 0);
    }

    // 2 kids
    private ExpNode myLoc;    
    private IdNode myId;
    private SemSym mySym;          // link to Sym for struct type
    private boolean badAccess;  // to prevent multiple, cascading errors
}

class AssignNode extends ExpNode {
    public AssignNode(ExpNode lhs, ExpNode exp) {
        myLhs = lhs;
        myExp = exp;
    }
    
    /**
     * Return the line number for this assignment node. 
     * The line number is the one corresponding to the left operand.
     */
    public int lineNum() {
        return myLhs.lineNum();
    }
    
    /**
     * Return the char number for this assignment node.
     * The char number is the one corresponding to the left operand.
     */
    public int charNum() {
        return myLhs.charNum();
    }
    
    /**
     * nameAnalysis
     * Given a symbol table symTab, perform name analysis on this node's 
     * two children
     */
    public void nameAnalysis(SymTable symTab) {
        myLhs.nameAnalysis(symTab);
        myExp.nameAnalysis(symTab);
    }
 
    /**
     * typeCheck
     */
    public Type typeCheck() {
        Type typeLhs = myLhs.typeCheck();
        Type typeExp = myExp.typeCheck();
        Type retType = typeLhs;
        
        if (typeLhs.isFnType() && typeExp.isFnType()) {
            ErrMsg.fatal(lineNum(), charNum(), "Function assignment");
            retType = new ErrorType();
        }
        
        if (typeLhs.isStructDefType() && typeExp.isStructDefType()) {
            ErrMsg.fatal(lineNum(), charNum(), "Struct name assignment");
            retType = new ErrorType();
        }
        
        if (typeLhs.isStructType() && typeExp.isStructType()) {
            ErrMsg.fatal(lineNum(), charNum(), "Struct variable assignment");
            retType = new ErrorType();
        }        
        
        if (!typeLhs.equals(typeExp) && !typeLhs.isErrorType() && !typeExp.isErrorType()) {
            ErrMsg.fatal(lineNum(), charNum(), "Type mismatch");
            retType = new ErrorType();
        }
        
        if (typeLhs.isErrorType() || typeExp.isErrorType()) {
            retType = new ErrorType();
        }
        
        return retType;
    }
    
    public int partialEvaluate() {
        valRhs = myExp.partialEvaluate();
        if(valRhs != -1) {//TODO - unary operator return of -1
            myLhs.storeVal(valRhs);
            myLhs.setStatic(1);
            return 1;
        } else {
            myLhs.setStatic(0);
            return -1;
        }
    }

    public void setDynamic() {
        System.out.format("setDynamic called for AssignNode\n");
        myLhs.setStatic(0);
        myExp.setDynamic();
    }

    public void codeGen() {
        //System.out.format("Static for lhs is %d\n", myLhs.getStatic());
        if (myLhs.getStatic() != 1) { //EDIT
            myExp.codeGen();
        } else {
            Codegen.generate("li", Codegen.T0, valRhs);
            Codegen.genPush(Codegen.T0);
        }
        myLhs.genAddr(); //At this point rhs value is on stack, lhs address is on top of it
        Codegen.genPop(Codegen.T1); //Popping addr to T1
        Codegen.genPop(Codegen.T0); //Popping rhs to T0
        Codegen.generateIndexed("sw", Codegen.T0, Codegen.T1, 0);
        Codegen.genPush(Codegen.T0);
    }

    public void unparse(PrintWriter p, int indent) {
        if (indent != -1)  p.print("(");
        myLhs.unparse(p, 0);
        p.print(" = ");
        myExp.unparse(p, 0);
        if (indent != -1)  p.print(")");
    }

    // 2 kids
    private ExpNode myLhs;
    private ExpNode myExp;
    //EDIT
    private int valRhs;
}

class CallExpNode extends ExpNode {
    public CallExpNode(IdNode name, ExpListNode elist) {
        myId = name;
        myExpList = elist;
    }

    public CallExpNode(IdNode name) {
        myId = name;
        myExpList = new ExpListNode(new LinkedList<ExpNode>());
    }

    /**
     * Return the line number for this call node. 
     * The line number is the one corresponding to the function name.
     */
    public int lineNum() {
        return myId.lineNum();
    }
    
    /**
     * Return the char number for this call node.
     * The char number is the one corresponding to the function name.
     */
    public int charNum() {
        return myId.charNum();
    }
    
    /**
     * nameAnalysis
     * Given a symbol table symTab, perform name analysis on this node's 
     * two children
     */
    public void nameAnalysis(SymTable symTab) {
        myId.nameAnalysis(symTab);
        myExpList.nameAnalysis(symTab);
    }  
      
    /**
     * typeCheck
     */
    public Type typeCheck() {
        if (!myId.typeCheck().isFnType()) {  
            ErrMsg.fatal(myId.lineNum(), myId.charNum(), 
                         "Attempt to call a non-function");
            return new ErrorType();
        }
        
        FnSym fnSym = (FnSym)(myId.sym());
        
        if (fnSym == null) {
            System.err.println("null sym for Id in CallExpNode.typeCheck");
            System.exit(-1);
        }
        
        if (myExpList.size() != fnSym.getNumParams()) {
            ErrMsg.fatal(myId.lineNum(), myId.charNum(), 
                         "Function call with wrong number of args");
            return fnSym.getReturnType();
        }
        
        myExpList.typeCheck(fnSym.getParamTypes());
        return fnSym.getReturnType();
    }
        
    public int partialEvaluate() {
        return -1;
    }

    public void codeGen() {
        //TODO
        myExpList.codeGen();
        myId.genJumpAndLink(); //does RA have to be done manually? I don't think so
        Codegen.genPush(Codegen.V0); //When jal returns, return value will be in v0
    }

    // ** unparse **
    public void unparse(PrintWriter p, int indent) {
        myId.unparse(p, 0);
        p.print("(");
        if (myExpList != null) {
            myExpList.unparse(p, 0);
        }
        p.print(")");
    }

    // 2 kids
    private IdNode myId;
    private ExpListNode myExpList;  // possibly null
}

abstract class UnaryExpNode extends ExpNode {
    public UnaryExpNode(ExpNode exp) {
        myExp = exp;
    }
    
    /**
     * Return the line number for this unary expression node. 
     * The line number is the one corresponding to the  operand.
     */
    public int lineNum() {
        return myExp.lineNum();
    }
    
    /**
     * Return the char number for this unary expression node.
     * The char number is the one corresponding to the  operand.
     */
    public int charNum() {
        return myExp.charNum();
    }
    
    /**
     * nameAnalysis
     * Given a symbol table symTab, perform name analysis on this node's child
     */
    public void nameAnalysis(SymTable symTab) {
        myExp.nameAnalysis(symTab);
    }
    
    // one child
    protected ExpNode myExp;
    //EDIT
    protected int valExp = -1;
}

abstract class BinaryExpNode extends ExpNode {
    public BinaryExpNode(ExpNode exp1, ExpNode exp2) {
        myExp1 = exp1;
        myExp2 = exp2;
    }
    
    /**
     * Return the line number for this binary expression node. 
     * The line number is the one corresponding to the left operand.
     */
    public int lineNum() {
        return myExp1.lineNum();
    }
    
    /**
     * Return the char number for this binary expression node.
     * The char number is the one corresponding to the left operand.
     */
    public int charNum() {
        return myExp1.charNum();
    }
    
    /**
     * nameAnalysis
     * Given a symbol table symTab, perform name analysis on this node's 
     * two children
     */
    public void nameAnalysis(SymTable symTab) {
        myExp1.nameAnalysis(symTab);
        myExp2.nameAnalysis(symTab);
    }
    
    // two kids
    protected ExpNode myExp1;
    protected ExpNode myExp2;
    //EDIT
    protected int valExp1 = -1;
    protected int valExp2 = -1;
}

// **********************************************************************
// Subclasses of UnaryExpNode
// **********************************************************************

class UnaryMinusNode extends UnaryExpNode {
    public UnaryMinusNode(ExpNode exp) {
        super(exp);
    }

    /**
     * typeCheck
     */
    public Type typeCheck() {
        Type type = myExp.typeCheck();
        Type retType = new IntType();
        
        if (!type.isErrorType() && !type.isIntType()) {
            ErrMsg.fatal(lineNum(), charNum(),
                         "Arithmetic operator applied to non-numeric operand");
            retType = new ErrorType();
        }
        
        if (type.isErrorType()) {
            retType = new ErrorType();
        }
        
        return retType;
    }

    public int partialEvaluate() {
        valExp = myExp.partialEvaluate(); //TODO - Need to write functions for doing all this
        if(valExp != -1) {//if the partial evaluation can be done
            return -1*valExp;
        } else {
            return -1;
        }
    }

    public void setDynamic() {
        valExp = -1;
    }

    public void codeGen() {
        //No change is needed for PE here, because the partially evaluated value will already be propagated upwards
        //and this codegen will never be called if PE was successful
        myExp.codeGen();
        Codegen.genPop(Codegen.T0);
        Codegen.generate("li", Codegen.T1, -1);
        Codegen.generate("mult", Codegen.T0, Codegen.T1);
        Codegen.generate("mflo", Codegen.T1); //Since its just a mult by -1, the result should fit inside one register 
        Codegen.genPush(Codegen.T1);
    }

    public void unparse(PrintWriter p, int indent) {
        p.print("(-");
        myExp.unparse(p, 0);
        p.print(")");
    }
}

class NotNode extends UnaryExpNode {
    public NotNode(ExpNode exp) {
        super(exp);
    }

    /**
     * typeCheck
     */
    public Type typeCheck() {
        Type type = myExp.typeCheck();
        Type retType = new BoolType();
        
        if (!type.isErrorType() && !type.isBoolType()) {
            ErrMsg.fatal(lineNum(), charNum(),
                         "Logical operator applied to non-bool operand");
            retType = new ErrorType();
        }
        
        if (type.isErrorType()) {
            retType = new ErrorType();
        }
        
        return retType;
    }

    public int partialEvaluate() {
        valExp = myExp.partialEvaluate();
        if (valExp != -1){
            if (valExp > 0) return 1;
            else return 0;
        } else {
            return -1;
        }
    }

    public void setDynamic() {
        valExp = -1;
    }

    public void codeGen() {
        String zeroLabel = Codegen.nextLabel();
        String oneLabel = Codegen.nextLabel();
        String doneLabel = Codegen.nextLabel();

        myExp.codeGen();
        Codegen.genPop(Codegen.T0); //is this the best way of inverting?
        Codegen.generate("li", Codegen.T1, 0);
        Codegen.generate("beq", Codegen.T0, Codegen.FALSE, oneLabel); //If exp is 0, jump to oneLabel and store a 1 

        Codegen.genLabel(zeroLabel); 
        Codegen.generate("b", doneLabel); //T1 has 0, go and store it

        Codegen.genLabel(oneLabel);
        Codegen.generate("li", Codegen.T1, 1); //Make T1 1, go and store it

        Codegen.genLabel(doneLabel);
        Codegen.genPush(Codegen.T1);

    }

    public void unparse(PrintWriter p, int indent) {
        p.print("(!");
        myExp.unparse(p, 0);
        p.print(")");
    }
}

// **********************************************************************
// Subclasses of BinaryExpNode
// **********************************************************************

abstract class ArithmeticExpNode extends BinaryExpNode {
    public ArithmeticExpNode(ExpNode exp1, ExpNode exp2) {
        super(exp1, exp2);
    }
    
    /**
     * typeCheck
     */
    public Type typeCheck() {
        Type type1 = myExp1.typeCheck();
        Type type2 = myExp2.typeCheck();
        Type retType = new IntType();
        
        if (!type1.isErrorType() && !type1.isIntType()) {
            ErrMsg.fatal(myExp1.lineNum(), myExp1.charNum(),
                         "Arithmetic operator applied to non-numeric operand");
            retType = new ErrorType();
        }
        
        if (!type2.isErrorType() && !type2.isIntType()) {
            ErrMsg.fatal(myExp2.lineNum(), myExp2.charNum(),
                         "Arithmetic operator applied to non-numeric operand");
            retType = new ErrorType();
        }
        
        if (type1.isErrorType() || type2.isErrorType()) {
            retType = new ErrorType();
        }
        
        return retType;
    }

    public void codeGen() {
        myExp1.codeGen();
        myExp2.codeGen();
        Codegen.genPop(Codegen.T1);
        Codegen.genPop(Codegen.T0); //now T0 has exp1 and t1 has exp2
    }

}

abstract class LogicalExpNode extends BinaryExpNode {
    public LogicalExpNode(ExpNode exp1, ExpNode exp2) {
        super(exp1, exp2);
    }
    
    /**
     * typeCheck
     */
    public Type typeCheck() {
        Type type1 = myExp1.typeCheck();
        Type type2 = myExp2.typeCheck();
        Type retType = new BoolType();
        
        if (!type1.isErrorType() && !type1.isBoolType()) {
            ErrMsg.fatal(myExp1.lineNum(), myExp1.charNum(),
                         "Logical operator applied to non-bool operand");
            retType = new ErrorType();
        }
        
        if (!type2.isErrorType() && !type2.isBoolType()) {
            ErrMsg.fatal(myExp2.lineNum(), myExp2.charNum(),
                         "Logical operator applied to non-bool operand");
            retType = new ErrorType();
        }
        
        if (type1.isErrorType() || type2.isErrorType()) {
            retType = new ErrorType();
        }
        
        return retType;
    }

    public void codeGen() {
    }

}

abstract class EqualityExpNode extends BinaryExpNode {
    public EqualityExpNode(ExpNode exp1, ExpNode exp2) {
        super(exp1, exp2);
    }
    
    /**
     * typeCheck
     */
    public Type typeCheck() {
        Type type1 = myExp1.typeCheck();
        Type type2 = myExp2.typeCheck();
        Type retType = new BoolType();
        
        if (type1.isVoidType() && type2.isVoidType()) {
            ErrMsg.fatal(lineNum(), charNum(),
                         "Equality operator applied to void functions");
            retType = new ErrorType();
        }
        
        if (type1.isFnType() && type2.isFnType()) {
            ErrMsg.fatal(lineNum(), charNum(),
                         "Equality operator applied to functions");
            retType = new ErrorType();
        }
        
        if (type1.isStructDefType() && type2.isStructDefType()) {
            ErrMsg.fatal(lineNum(), charNum(),
                         "Equality operator applied to struct names");
            retType = new ErrorType();
        }
        
        if (type1.isStructType() && type2.isStructType()) {
            ErrMsg.fatal(lineNum(), charNum(),
                         "Equality operator applied to struct variables");
            retType = new ErrorType();
        }        
        
        if (!type1.equals(type2) && !type1.isErrorType() && !type2.isErrorType()) {
            ErrMsg.fatal(lineNum(), charNum(),
                         "Type mismatch");
            retType = new ErrorType();
        }
        
        if (type1.isErrorType() || type2.isErrorType()) {
            retType = new ErrorType();
        }
        
        return retType;
    }

    public void codeGen() {
    }
}

abstract class RelationalExpNode extends BinaryExpNode {
    public RelationalExpNode(ExpNode exp1, ExpNode exp2) {
        super(exp1, exp2);
    }
    
    /**
     * typeCheck
     */
    public Type typeCheck() {
        Type type1 = myExp1.typeCheck();
        Type type2 = myExp2.typeCheck();
        Type retType = new BoolType();
        
        if (!type1.isErrorType() && !type1.isIntType()) {
            ErrMsg.fatal(myExp1.lineNum(), myExp1.charNum(),
                         "Relational operator applied to non-numeric operand");
            retType = new ErrorType();
        }
        
        if (!type2.isErrorType() && !type2.isIntType()) {
            ErrMsg.fatal(myExp2.lineNum(), myExp2.charNum(),
                         "Relational operator applied to non-numeric operand");
            retType = new ErrorType();
        }
        
        if (type1.isErrorType() || type2.isErrorType()) {
            retType = new ErrorType();
        }
        
        return retType;
    }

    public void codeGen() {
    }
}

class PlusNode extends ArithmeticExpNode {
    public PlusNode(ExpNode exp1, ExpNode exp2) {
        super(exp1, exp2);
    }
    
    public void unparse(PrintWriter p, int indent) {
        p.print("(");
        myExp1.unparse(p, 0);
        p.print(" + ");
        myExp2.unparse(p, 0);
        p.print(")");
    }
    
    public int partialEvaluate() {
        valExp1 = myExp1.partialEvaluate();
        valExp2 = myExp2.partialEvaluate();
        if ((valExp1 != -1) && (valExp2 != -1)) {
            return valExp1 + valExp2;
        } else {
            //what if only 1 can -> handled in codegen
            return -1;
        }
    }

    public void setDynamic() {
        valExp1 = -1;
        valExp2 = -1;
    }

    public void codeGen() {
        if ((valExp1 == -1) && (valExp2 == -1)) {
            super.codeGen();
            Codegen.generate("add", Codegen.T0, Codegen.T0, Codegen.T1);
            Codegen.genPush(Codegen.T0);
        } else if ((valExp1 != -1) && (valExp2 == -1)) {
            myExp2.codeGen();
            Codegen.genPop(Codegen.T1); //T1 has exp2
            Codegen.generate("li", Codegen.T0, valExp1); //directly use this val
            Codegen.generate("add", Codegen.T0, Codegen.T0, Codegen.T1);
            Codegen.genPush(Codegen.T0);
        } else if ((valExp1 == -1) && (valExp2 != -1)) {
            myExp1.codeGen();
            Codegen.genPop(Codegen.T0); //T0 has exp1
            Codegen.generate("li", Codegen.T1, valExp2); //directly use this val
            Codegen.generate("add", Codegen.T0, Codegen.T0, Codegen.T1);
            Codegen.genPush(Codegen.T0);
        }
    }
}

class MinusNode extends ArithmeticExpNode {
    public MinusNode(ExpNode exp1, ExpNode exp2) {
        super(exp1, exp2);
    }
    
    public void unparse(PrintWriter p, int indent) {
        p.print("(");
        myExp1.unparse(p, 0);
        p.print(" - ");
        myExp2.unparse(p, 0);
        p.print(")");
    }
    
    public int partialEvaluate() {
        valExp1 = myExp1.partialEvaluate();
        valExp2 = myExp2.partialEvaluate();
        if ((valExp1 != -1) && (valExp2 != -1)) {
            return valExp1 - valExp2;
        } else {
            return -1;
        }
    }

    public void setDynamic() {
        valExp1 = -1;
        valExp2 = -1;
    }

    public void codeGen() {
        if ((valExp1 == -1) && (valExp2 == -1)) {
            super.codeGen();
            Codegen.generate("sub", Codegen.T0, Codegen.T0, Codegen.T1);
            Codegen.genPush(Codegen.T0);
        } else if ((valExp1 != -1) && (valExp2 == -1)) {
            myExp2.codeGen();
            Codegen.genPop(Codegen.T1); //T1 has exp2
            Codegen.generate("li", Codegen.T0, valExp1); //directly use this val
            Codegen.generate("sub", Codegen.T0, Codegen.T0, Codegen.T1);
            Codegen.genPush(Codegen.T0);
        } else if ((valExp1 == -1) && (valExp2 != -1)) {
            myExp1.codeGen();
            Codegen.genPop(Codegen.T0); //T0 has exp1
            Codegen.generate("li", Codegen.T1, valExp2); //directly use this val
            Codegen.generate("sub", Codegen.T0, Codegen.T0, Codegen.T1);
            Codegen.genPush(Codegen.T0);
        }
    }
}

class TimesNode extends ArithmeticExpNode {
    public TimesNode(ExpNode exp1, ExpNode exp2) {
        super(exp1, exp2);
    }

    
    public void unparse(PrintWriter p, int indent) {
        p.print("(");
        myExp1.unparse(p, 0);
        p.print(" * ");
        myExp2.unparse(p, 0);
        p.print(")");
    }
    
    public int partialEvaluate() {
        valExp1 = myExp1.partialEvaluate();
        valExp2 = myExp2.partialEvaluate();
        if ((valExp1 != -1) && (valExp2 != -1)) {
            return valExp1*valExp2;
        } else {
            return -1;
        }
    }

    public void setDynamic() {
        valExp1 = -1;
        valExp2 = -1;
    }

    public void codeGen() {
        if ((valExp1 == -1) && (valExp2 == -1)) {
            super.codeGen();
            Codegen.generate("mult", Codegen.T0, Codegen.T1);
            Codegen.generate("mflo", Codegen.T0); //take care of mfhi
            Codegen.genPush(Codegen.T0);
        } else if ((valExp1 != -1) && (valExp2 == -1)) {
            myExp2.codeGen();
            Codegen.genPop(Codegen.T1); //T1 has exp2
            Codegen.generate("li", Codegen.T0, valExp1); //directly use this val
            Codegen.generate("mult", Codegen.T0, Codegen.T1);
            Codegen.generate("mflo", Codegen.T0); 
            Codegen.genPush(Codegen.T0);
        } else if ((valExp1 == -1) && (valExp2 != -1)) {
            myExp1.codeGen();
            Codegen.genPop(Codegen.T0); //T0 has exp1
            Codegen.generate("li", Codegen.T1, valExp2); //directly use this val
            Codegen.generate("mult", Codegen.T0, Codegen.T1);
            Codegen.generate("mflo", Codegen.T0);
            Codegen.genPush(Codegen.T0);
        }
    }
}

class DivideNode extends ArithmeticExpNode {
    public DivideNode(ExpNode exp1, ExpNode exp2) {
        super(exp1, exp2);
    }
    
    public void unparse(PrintWriter p, int indent) {
        p.print("(");
        myExp1.unparse(p, 0);
        p.print(" / ");
        myExp2.unparse(p, 0);
        p.print(")");
    }
    
    public int partialEvaluate() {
        valExp1 = myExp1.partialEvaluate();
        valExp2 = myExp2.partialEvaluate();
        if ((valExp1 != -1) && (valExp2 != -1)) {
            return valExp1/valExp2;
        } else {
            return -1;
        }
    }

    public void setDynamic() {
        valExp1 = -1;
        valExp2 = -1;
    }

    public void codeGen() {
        if ((valExp1 == -1) && (valExp2 == -1)) {
            super.codeGen();
            Codegen.generate("div", Codegen.T0, Codegen.T1);
            Codegen.generate("mflo", Codegen.T0); //take care of mfhi
            Codegen.genPush(Codegen.T0);
        } else if ((valExp1 != -1) && (valExp2 == -1)) {
            myExp2.codeGen();
            Codegen.genPop(Codegen.T1); //T1 has exp2
            Codegen.generate("li", Codegen.T0, valExp1); //directly use this val
            Codegen.generate("div", Codegen.T0, Codegen.T1);
            Codegen.generate("mflo", Codegen.T0); 
            Codegen.genPush(Codegen.T0);
        } else if ((valExp1 == -1) && (valExp2 != -1)) {
            myExp1.codeGen();
            Codegen.genPop(Codegen.T0); //T0 has exp1
            Codegen.generate("li", Codegen.T1, valExp2); //directly use this val
            Codegen.generate("div", Codegen.T0, Codegen.T1);
            Codegen.generate("mflo", Codegen.T0);
            Codegen.genPush(Codegen.T0);
        }
    }
}

class AndNode extends LogicalExpNode {
    public AndNode(ExpNode exp1, ExpNode exp2) {
        super(exp1, exp2);
    }
    
    public void unparse(PrintWriter p, int indent) {
        p.print("(");
        myExp1.unparse(p, 0);
        p.print(" && ");
        myExp2.unparse(p, 0);
        p.print(")");
    }

    public int partialEvaluate() {
        valExp1 = myExp1.partialEvaluate();
        valExp2 = myExp2.partialEvaluate();
        if ((valExp1 != -1) && (valExp2 != -1)) {
            return valExp1 & valExp2;
        } else {
            return -1;
        }
    }

    public void setDynamic() {
        valExp1 = -1;
        valExp2 = -1;
    }

    public void codeGen() {
        if ((valExp1 == -1) && (valExp2 == -1)) {
            String doneLabel = Codegen.nextLabel();

            myExp1.codeGen();
            Codegen.genPop(Codegen.T0); 
            Codegen.generate("li", Codegen.T1, 0);
            Codegen.generate("beq", Codegen.T0, Codegen.T1, doneLabel); //if T0 is 0, jump to doneLabel and store value in T0 (which is 0) into stack

            myExp2.codeGen();
            Codegen.genPop(Codegen.T0); //This value basically decides everything now, just store it into stack

            Codegen.genLabel(doneLabel);
            Codegen.genPush(Codegen.T0);

        } else if ((valExp1 != -1) && (valExp2 == -1)) {
            String doneLabel = Codegen.nextLabel();

            Codegen.generate("li", Codegen.T0, valExp1); //This line serves the purpose of the below two
            //myExp1.codeGen();
            //Codegen.genPop(Codegen.T0); 
            Codegen.generate("li", Codegen.T1, 0);
            Codegen.generate("beq", Codegen.T0, Codegen.T1, doneLabel);

            myExp2.codeGen();
            Codegen.genPop(Codegen.T0);

            Codegen.genLabel(doneLabel);
            Codegen.genPush(Codegen.T0);

        } else if ((valExp1 == -1) && (valExp2 != -1)) {
            String doneLabel = Codegen.nextLabel();

            myExp1.codeGen();
            Codegen.genPop(Codegen.T0); 
            Codegen.generate("li", Codegen.T1, 0);
            Codegen.generate("beq", Codegen.T0, Codegen.T1, doneLabel);

            Codegen.generate("li", Codegen.T0, valExp2); //This line serves the purpose of the below two
            //myExp2.codeGen();
            //Codegen.genPop(Codegen.T0);

            Codegen.genLabel(doneLabel);
            Codegen.genPush(Codegen.T0);

        }
    }

}

class OrNode extends LogicalExpNode {
    public OrNode(ExpNode exp1, ExpNode exp2) {
        super(exp1, exp2);
    }
    
    public void unparse(PrintWriter p, int indent) {
        p.print("(");
        myExp1.unparse(p, 0);
        p.print(" || ");
        myExp2.unparse(p, 0);
        p.print(")");
    }

    public int partialEvaluate() {
        valExp1 = myExp1.partialEvaluate();
        valExp2 = myExp2.partialEvaluate();
        if ((valExp1 != -1) && (valExp2 != -1)) {
            return valExp1 | valExp2;
        } else {
            return -1;
        }
    }

    public void setDynamic() {
        valExp1 = -1;
        valExp2 = -1;
    }

    public void codeGen() {
        if ((valExp1 == -1) && (valExp2 == -1)) {
            String doneLabel = Codegen.nextLabel();

            myExp1.codeGen();
            Codegen.genPop(Codegen.T0); 
            Codegen.generate("li", Codegen.T1, 1);
            Codegen.generate("beq", Codegen.T0, Codegen.T1, doneLabel); //if T0 is 1, jump to doneLabel and store value in T0 (which is 1) into stack

            myExp2.codeGen();
            Codegen.genPop(Codegen.T0); //This value basically decides everything now, just store it into stack

            Codegen.genLabel(doneLabel);
            Codegen.genPush(Codegen.T0);
        } else if ((valExp1 != -1) && (valExp2 == -1)) {
            String doneLabel = Codegen.nextLabel();

            Codegen.generate("li", Codegen.T0, valExp1); //This line serves the purpose of the below two
            //myExp1.codeGen();
            //Codegen.genPop(Codegen.T0); 
            Codegen.generate("li", Codegen.T1, 1);
            Codegen.generate("beq", Codegen.T0, Codegen.T1, doneLabel); 

            myExp2.codeGen();
            Codegen.genPop(Codegen.T0);

            Codegen.genLabel(doneLabel);
            Codegen.genPush(Codegen.T0);
        } else if ((valExp1 == -1) && (valExp2 != -1)) {
            String doneLabel = Codegen.nextLabel();

            myExp1.codeGen();
            Codegen.genPop(Codegen.T0); 
            Codegen.generate("li", Codegen.T1, 1);
            Codegen.generate("beq", Codegen.T0, Codegen.T1, doneLabel);

            Codegen.generate("li", Codegen.T0, valExp2); //This line serves the purpose of the below two
            //myExp2.codeGen();
            //Codegen.genPop(Codegen.T0);

            Codegen.genLabel(doneLabel);
            Codegen.genPush(Codegen.T0);
        }
    }

}

class EqualsNode extends EqualityExpNode {
    public EqualsNode(ExpNode exp1, ExpNode exp2) {
        super(exp1, exp2);
    }
    
    public void unparse(PrintWriter p, int indent) {
        p.print("(");
        myExp1.unparse(p, 0);
        p.print(" == ");
        myExp2.unparse(p, 0);
        p.print(")");
    }

    public int partialEvaluate() {
        valExp1 = myExp1.partialEvaluate();
        valExp2 = myExp2.partialEvaluate();
        if ((valExp1 != -1) && (valExp2 != -1)) {
            if (valExp1 == valExp2)
                return 1;
            else
                return 0;
        } else {
            return -1;
        }
    }

    public void setDynamic() {
        valExp1 = -1;
        valExp2 = -1;
    }

    public void codeGen() {
        if ((valExp1 == -1) && (valExp2 == -1)) {
            String storeZeroLabel = Codegen.nextLabel();
            String storeOneLabel = Codegen.nextLabel();
            String doneLabel = Codegen.nextLabel();

            myExp1.codeGen();
            myExp2.codeGen();
            Codegen.genPop(Codegen.T1);
            Codegen.genPop(Codegen.T0);
            Codegen.generate("beq", Codegen.T0, Codegen.T1, storeOneLabel);

            Codegen.genLabel(storeZeroLabel);
            Codegen.generate("li", Codegen.T0, 0);
            Codegen.generate("b", doneLabel);

            Codegen.genLabel(storeOneLabel);
            Codegen.generate("li", Codegen.T0, 1);

            Codegen.genLabel(doneLabel);
            Codegen.genPush(Codegen.T0);
        } else if ((valExp1 != -1) && (valExp2 == -1)) {
            String storeZeroLabel = Codegen.nextLabel();
            String storeOneLabel = Codegen.nextLabel();
            String doneLabel = Codegen.nextLabel();

            
            Codegen.generate("li", Codegen.T0, valExp1);
            //myExp1.codeGen();
            myExp2.codeGen();
            Codegen.genPop(Codegen.T1);
            //Codegen.genPop(Codegen.T0);
            Codegen.generate("beq", Codegen.T0, Codegen.T1, storeOneLabel);

            Codegen.genLabel(storeZeroLabel);
            Codegen.generate("li", Codegen.T0, 0);
            Codegen.generate("b", doneLabel);

            Codegen.genLabel(storeOneLabel);
            Codegen.generate("li", Codegen.T0, 1);

            Codegen.genLabel(doneLabel);
            Codegen.genPush(Codegen.T0);
        } else if ((valExp1 == -1) && (valExp2 != -1)) {
            String storeZeroLabel = Codegen.nextLabel();
            String storeOneLabel = Codegen.nextLabel();
            String doneLabel = Codegen.nextLabel();

            myExp1.codeGen();
            Codegen.generate("li", Codegen.T1, valExp2);
            //myExp2.codeGen();
            //Codegen.genPop(Codegen.T1);
            Codegen.genPop(Codegen.T0);
            Codegen.generate("beq", Codegen.T0, Codegen.T1, storeOneLabel);

            Codegen.genLabel(storeZeroLabel);
            Codegen.generate("li", Codegen.T0, 0);
            Codegen.generate("b", doneLabel);

            Codegen.genLabel(storeOneLabel);
            Codegen.generate("li", Codegen.T0, 1);

            Codegen.genLabel(doneLabel);
            Codegen.genPush(Codegen.T0);
        }
    }

}

class NotEqualsNode extends EqualityExpNode {
    public NotEqualsNode(ExpNode exp1, ExpNode exp2) {
        super(exp1, exp2);
    }
    
    public void unparse(PrintWriter p, int indent) {
        p.print("(");
        myExp1.unparse(p, 0);
        p.print(" != ");
        myExp2.unparse(p, 0);
        p.print(")");
    }

    public int partialEvaluate() {
        valExp1 = myExp1.partialEvaluate();
        valExp2 = myExp2.partialEvaluate();
        if ((valExp1 != -1) && (valExp2 != -1)) {
            if (valExp1 != valExp2)
                return 1;
            else
                return 0;
        } else {
            return -1;
        }
    }

    public void setDynamic() {
        valExp1 = -1;
        valExp2 = -1;
    }

    public void codeGen() {
        if ((valExp1 == -1) && (valExp2 == -1)) {
            String storeZeroLabel = Codegen.nextLabel();
            String storeOneLabel = Codegen.nextLabel();
            String doneLabel = Codegen.nextLabel();

            myExp1.codeGen();
            myExp2.codeGen();
            Codegen.genPop(Codegen.T1);
            Codegen.genPop(Codegen.T0);
            Codegen.generate("bne", Codegen.T0, Codegen.T1, storeOneLabel);

            Codegen.genLabel(storeZeroLabel);
            Codegen.generate("li", Codegen.T0, 0);
            Codegen.generate("b", doneLabel);

            Codegen.genLabel(storeOneLabel);
            Codegen.generate("li", Codegen.T0, 1);

            Codegen.genLabel(doneLabel);
            Codegen.genPush(Codegen.T0);
        } else if ((valExp1 != -1) && (valExp2 == -1)) {
            String storeZeroLabel = Codegen.nextLabel();
            String storeOneLabel = Codegen.nextLabel();
            String doneLabel = Codegen.nextLabel();

            Codegen.generate("li", Codegen.T0, valExp1);
            //myExp1.codeGen();
            myExp2.codeGen();
            Codegen.genPop(Codegen.T1);
            //Codegen.genPop(Codegen.T0);
            Codegen.generate("bne", Codegen.T0, Codegen.T1, storeOneLabel);

            Codegen.genLabel(storeZeroLabel);
            Codegen.generate("li", Codegen.T0, 0);
            Codegen.generate("b", doneLabel);

            Codegen.genLabel(storeOneLabel);
            Codegen.generate("li", Codegen.T0, 1);

            Codegen.genLabel(doneLabel);
            Codegen.genPush(Codegen.T0);
        } else if ((valExp1 == -1) && (valExp2 != -1)) {
            String storeZeroLabel = Codegen.nextLabel();
            String storeOneLabel = Codegen.nextLabel();
            String doneLabel = Codegen.nextLabel();

            myExp1.codeGen();
            Codegen.generate("li", Codegen.T1, valExp2);
            //myExp2.codeGen();
            //Codegen.genPop(Codegen.T1);
            Codegen.genPop(Codegen.T0);
            Codegen.generate("bne", Codegen.T0, Codegen.T1, storeOneLabel);

            Codegen.genLabel(storeZeroLabel);
            Codegen.generate("li", Codegen.T0, 0);
            Codegen.generate("b", doneLabel);

            Codegen.genLabel(storeOneLabel);
            Codegen.generate("li", Codegen.T0, 1);

            Codegen.genLabel(doneLabel);
            Codegen.genPush(Codegen.T0);
        }
    }
}

class LessNode extends RelationalExpNode {
    public LessNode(ExpNode exp1, ExpNode exp2) {
        super(exp1, exp2);
    }
    
    public void unparse(PrintWriter p, int indent) {
        p.print("(");
        myExp1.unparse(p, 0);
        p.print(" < ");
        myExp2.unparse(p, 0);
        p.print(")");
    }

    public int partialEvaluate() {
        valExp1 = myExp1.partialEvaluate();
        valExp2 = myExp2.partialEvaluate();
        if ((valExp1 != -1) && (valExp2 != -1)) {
            if (valExp1 < valExp2)
                return 1;
            else
                return 0;
        } else {
            return -1;
        }
    }

    public void setDynamic() {
        valExp1 = -1;
        valExp2 = -1;
    }

     public void codeGen() {
       if ((valExp1 == -1) && (valExp2 == -1)) {
            String storeZeroLabel = Codegen.nextLabel();
            String storeOneLabel = Codegen.nextLabel();
            String doneLabel = Codegen.nextLabel();

            myExp1.codeGen();
            myExp2.codeGen();
            Codegen.genPop(Codegen.T1);
            Codegen.genPop(Codegen.T0);
            Codegen.generate("blt", Codegen.T0, Codegen.T1, storeOneLabel);

            Codegen.genLabel(storeZeroLabel);
            Codegen.generate("li", Codegen.T0, 0);
            Codegen.generate("b", doneLabel);

            Codegen.genLabel(storeOneLabel);
            Codegen.generate("li", Codegen.T0, 1);

            Codegen.genLabel(doneLabel);
            Codegen.genPush(Codegen.T0);
        } else if ((valExp1 != -1) && (valExp2 == -1)) {
            String storeZeroLabel = Codegen.nextLabel();
            String storeOneLabel = Codegen.nextLabel();
            String doneLabel = Codegen.nextLabel();

            Codegen.generate("li", Codegen.T0, valExp1);
            //myExp1.codeGen();
            myExp2.codeGen();
            Codegen.genPop(Codegen.T1);
            //Codegen.genPop(Codegen.T0);
            Codegen.generate("blt", Codegen.T0, Codegen.T1, storeOneLabel);

            Codegen.genLabel(storeZeroLabel);
            Codegen.generate("li", Codegen.T0, 0);
            Codegen.generate("b", doneLabel);

            Codegen.genLabel(storeOneLabel);
            Codegen.generate("li", Codegen.T0, 1);

            Codegen.genLabel(doneLabel);
            Codegen.genPush(Codegen.T0);
        } else if ((valExp1 == -1) && (valExp2 != -1)) {
            String storeZeroLabel = Codegen.nextLabel();
            String storeOneLabel = Codegen.nextLabel();
            String doneLabel = Codegen.nextLabel();

            myExp1.codeGen();
            Codegen.generate("li", Codegen.T1, valExp2);
            //myExp2.codeGen();
            //Codegen.genPop(Codegen.T1);
            Codegen.genPop(Codegen.T0);
            Codegen.generate("blt", Codegen.T0, Codegen.T1, storeOneLabel);

            Codegen.genLabel(storeZeroLabel);
            Codegen.generate("li", Codegen.T0, 0);
            Codegen.generate("b", doneLabel);

            Codegen.genLabel(storeOneLabel);
            Codegen.generate("li", Codegen.T0, 1);

            Codegen.genLabel(doneLabel);
            Codegen.genPush(Codegen.T0);
        }
    }
}

class GreaterNode extends RelationalExpNode {
    public GreaterNode(ExpNode exp1, ExpNode exp2) {
        super(exp1, exp2);
    }

    public void unparse(PrintWriter p, int indent) {
        p.print("(");
        myExp1.unparse(p, 0);
        p.print(" > ");
        myExp2.unparse(p, 0);
        p.print(")");
    }

    public int partialEvaluate() {
        valExp1 = myExp1.partialEvaluate();
        valExp2 = myExp2.partialEvaluate();
        if ((valExp1 != -1) && (valExp2 != -1)) {
            if (valExp1 > valExp2)
                return 1;
            else
                return 0;
        } else {
            return -1;
        }
    }

    public void setDynamic() {
        valExp1 = -1;
        valExp2 = -1;
    }

    public void codeGen() {
       if ((valExp1 == -1) && (valExp2 == -1)) {
            String storeZeroLabel = Codegen.nextLabel();
            String storeOneLabel = Codegen.nextLabel();
            String doneLabel = Codegen.nextLabel();

            myExp1.codeGen();
            myExp2.codeGen();
            Codegen.genPop(Codegen.T1);
            Codegen.genPop(Codegen.T0);
            Codegen.generate("bgt", Codegen.T0, Codegen.T1, storeOneLabel);

            Codegen.genLabel(storeZeroLabel);
            Codegen.generate("li", Codegen.T0, 0);
            Codegen.generate("b", doneLabel);

            Codegen.genLabel(storeOneLabel);
            Codegen.generate("li", Codegen.T0, 1);

            Codegen.genLabel(doneLabel);
            Codegen.genPush(Codegen.T0);
        } else if ((valExp1 != -1) && (valExp2 == -1)) {
            String storeZeroLabel = Codegen.nextLabel();
            String storeOneLabel = Codegen.nextLabel();
            String doneLabel = Codegen.nextLabel();

            Codegen.generate("li", Codegen.T0, valExp1);
            //myExp1.codeGen();
            myExp2.codeGen();
            Codegen.genPop(Codegen.T1);
            //Codegen.genPop(Codegen.T0);
            Codegen.generate("bgt", Codegen.T0, Codegen.T1, storeOneLabel);

            Codegen.genLabel(storeZeroLabel);
            Codegen.generate("li", Codegen.T0, 0);
            Codegen.generate("b", doneLabel);

            Codegen.genLabel(storeOneLabel);
            Codegen.generate("li", Codegen.T0, 1);

            Codegen.genLabel(doneLabel);
            Codegen.genPush(Codegen.T0);
        } else if ((valExp1 == -1) && (valExp2 != -1)) {
            String storeZeroLabel = Codegen.nextLabel();
            String storeOneLabel = Codegen.nextLabel();
            String doneLabel = Codegen.nextLabel();

            myExp1.codeGen();
            Codegen.generate("li", Codegen.T1, valExp2);
            //myExp2.codeGen();
            //Codegen.genPop(Codegen.T1);
            Codegen.genPop(Codegen.T0);
            Codegen.generate("bgt", Codegen.T0, Codegen.T1, storeOneLabel);

            Codegen.genLabel(storeZeroLabel);
            Codegen.generate("li", Codegen.T0, 0);
            Codegen.generate("b", doneLabel);

            Codegen.genLabel(storeOneLabel);
            Codegen.generate("li", Codegen.T0, 1);

            Codegen.genLabel(doneLabel);
            Codegen.genPush(Codegen.T0);
        }
    }
}

class LessEqNode extends RelationalExpNode {
    public LessEqNode(ExpNode exp1, ExpNode exp2) {
        super(exp1, exp2);
    }

    public void unparse(PrintWriter p, int indent) {
        p.print("(");
        myExp1.unparse(p, 0);
        p.print(" <= ");
        myExp2.unparse(p, 0);
        p.print(")");
    }

    public int partialEvaluate() {
        valExp1 = myExp1.partialEvaluate();
        valExp2 = myExp2.partialEvaluate();
        if ((valExp1 != -1) && (valExp2 != -1)) {
            if (valExp1 <= valExp2)
                return 1;
            else
                return 0;
        } else {
            return -1;
        }
    }

    public void setDynamic() {
        valExp1 = -1;
        valExp2 = -1;
    }

    public void codeGen() {
        if ((valExp1 == -1) && (valExp2 == -1)) {
            String storeZeroLabel = Codegen.nextLabel();
            String storeOneLabel = Codegen.nextLabel();
            String doneLabel = Codegen.nextLabel();

            myExp1.codeGen();
            myExp2.codeGen();
            Codegen.genPop(Codegen.T1);
            Codegen.genPop(Codegen.T0);
            Codegen.generate("ble", Codegen.T0, Codegen.T1, storeOneLabel);

            Codegen.genLabel(storeZeroLabel);
            Codegen.generate("li", Codegen.T0, 0);
            Codegen.generate("b", doneLabel);

            Codegen.genLabel(storeOneLabel);
            Codegen.generate("li", Codegen.T0, 1);

            Codegen.genLabel(doneLabel);
            Codegen.genPush(Codegen.T0);
        } else if ((valExp1 != -1) && (valExp2 == -1)) {
            String storeZeroLabel = Codegen.nextLabel();
            String storeOneLabel = Codegen.nextLabel();
            String doneLabel = Codegen.nextLabel();

            Codegen.generate("li", Codegen.T0, valExp1);
            //myExp1.codeGen();
            myExp2.codeGen();
            Codegen.genPop(Codegen.T1);
            //Codegen.genPop(Codegen.T0);
            Codegen.generate("ble", Codegen.T0, Codegen.T1, storeOneLabel);

            Codegen.genLabel(storeZeroLabel);
            Codegen.generate("li", Codegen.T0, 0);
            Codegen.generate("b", doneLabel);

            Codegen.genLabel(storeOneLabel);
            Codegen.generate("li", Codegen.T0, 1);

            Codegen.genLabel(doneLabel);
            Codegen.genPush(Codegen.T0);
        } else if ((valExp1 == -1) && (valExp2 != -1)) {
            String storeZeroLabel = Codegen.nextLabel();
            String storeOneLabel = Codegen.nextLabel();
            String doneLabel = Codegen.nextLabel();

            Codegen.generate("li", Codegen.T1, valExp2);
            myExp1.codeGen();
            //myExp2.codeGen();
            //Codegen.genPop(Codegen.T1);
            Codegen.genPop(Codegen.T0);
            Codegen.generate("ble", Codegen.T0, Codegen.T1, storeOneLabel);

            Codegen.genLabel(storeZeroLabel);
            Codegen.generate("li", Codegen.T0, 0);
            Codegen.generate("b", doneLabel);

            Codegen.genLabel(storeOneLabel);
            Codegen.generate("li", Codegen.T0, 1);

            Codegen.genLabel(doneLabel);
            Codegen.genPush(Codegen.T0);
        }
    }
}

class GreaterEqNode extends RelationalExpNode {
    public GreaterEqNode(ExpNode exp1, ExpNode exp2) {
        super(exp1, exp2);
    }

    public void unparse(PrintWriter p, int indent) {
        p.print("(");
        myExp1.unparse(p, 0);
        p.print(" >= ");
        myExp2.unparse(p, 0);
        p.print(")");
    }

    public int partialEvaluate() {
        valExp1 = myExp1.partialEvaluate();
        valExp2 = myExp2.partialEvaluate();
        if ((valExp1 != -1) && (valExp2 != -1)) {
            if (valExp1 >= valExp2)
                return 1;
            else
                return 0;
        } else {
            return -1;
        }
    }

    public void setDynamic() {
        valExp1 = -1;
        valExp2 = -1;
    }

    public void codeGen() {
        if ((valExp1 == -1) && (valExp2 == -1)) {
            String storeZeroLabel = Codegen.nextLabel();
            String storeOneLabel = Codegen.nextLabel();
            String doneLabel = Codegen.nextLabel();

            myExp1.codeGen();
            myExp2.codeGen();
            Codegen.genPop(Codegen.T1);
            Codegen.genPop(Codegen.T0);
            Codegen.generate("bge", Codegen.T0, Codegen.T1, storeOneLabel);

            Codegen.genLabel(storeZeroLabel);
            Codegen.generate("li", Codegen.T0, 0);
            Codegen.generate("b", doneLabel);

            Codegen.genLabel(storeOneLabel);
            Codegen.generate("li", Codegen.T0, 1);

            Codegen.genLabel(doneLabel);
            Codegen.genPush(Codegen.T0);
        } else if ((valExp1 != -1) && (valExp2 == -1)) {
            String storeZeroLabel = Codegen.nextLabel();
            String storeOneLabel = Codegen.nextLabel();
            String doneLabel = Codegen.nextLabel();

            Codegen.generate("li", Codegen.T0, valExp1);
            //myExp1.codeGen();
            myExp2.codeGen();
            Codegen.genPop(Codegen.T1);
            //Codegen.genPop(Codegen.T0);
            Codegen.generate("bge", Codegen.T0, Codegen.T1, storeOneLabel);

            Codegen.genLabel(storeZeroLabel);
            Codegen.generate("li", Codegen.T0, 0);
            Codegen.generate("b", doneLabel);

            Codegen.genLabel(storeOneLabel);
            Codegen.generate("li", Codegen.T0, 1);

            Codegen.genLabel(doneLabel);
            Codegen.genPush(Codegen.T0);
        } else if ((valExp1 == -1) && (valExp2 != -1)) {
            String storeZeroLabel = Codegen.nextLabel();
            String storeOneLabel = Codegen.nextLabel();
            String doneLabel = Codegen.nextLabel();

            Codegen.generate("li", Codegen.T1, valExp2);
            myExp1.codeGen();
            //myExp2.codeGen();
            //Codegen.genPop(Codegen.T1);
            Codegen.genPop(Codegen.T0);
            Codegen.generate("bge", Codegen.T0, Codegen.T1, storeOneLabel);

            Codegen.genLabel(storeZeroLabel);
            Codegen.generate("li", Codegen.T0, 0);
            Codegen.generate("b", doneLabel);

            Codegen.genLabel(storeOneLabel);
            Codegen.generate("li", Codegen.T0, 1);

            Codegen.genLabel(doneLabel);
            Codegen.genPush(Codegen.T0);
        }
    }
}

