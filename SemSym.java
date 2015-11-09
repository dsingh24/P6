import java.util.*;

/**
 * The Sym class defines a symbol-table entry. 
 * Each Sym contains a type (a Type).
 */
public class SemSym {
    private Type type;
    public int isGlobal = 0;
    private int offset = 0;
    private int isStatic = 0;
    private int localVal;
    
    public SemSym(Type type) {
        this.type = type;
    }

    public SemSym(Type type, int isGlobal) {
        this.type = type;
        this.isGlobal = isGlobal; //isGlobal == 1 would be mean global
    }
    
    public Type getType() {
        return type;
    }
    
    public String toString() {
        return type.toString();
    }

    public void setOffset(int offset) {
        this.offset = offset;
    }

    public int returnOffset() {
        return offset;
    }

    //EDIT
    public void storeVal(int val) {
        this.localVal = val;
    }

    public int loadVal() {
        return localVal;
    }

    public void setStatic(int val) {
        this.isStatic = val;
    }

    public int getStatic() {
        return isStatic;
    }
}

/**
 * The FnSym class is a subclass of the Sym class just for functions.
 * The returnType field holds the return type and there are fields to hold
 * information about the parameters.
 */
class FnSym extends SemSym {
    // new fields
    private Type returnType;
    private int numParams;
    private List<Type> paramTypes;
    private int sizeFormals;
    private int sizeLocals;
    
    public FnSym(Type type, int numparams) {
        super(new FnType());
        returnType = type;
        numParams = numparams;    
    }

    public void addFormals(List<Type> L) {
        paramTypes = L;
    }
    
    public Type getReturnType() {
        return returnType;
    }

    public int getNumParams() {
        return numParams;
    }

    public int getSizeFormals() {
        return sizeFormals;
    }

    public int getSizeLocals() {
        return sizeLocals;
    }

    public List<Type> getParamTypes() {
        return paramTypes;
    }

    public String toString() {
        // make list of formals
        String str = "";
        boolean notfirst = false;
        for (Type type : paramTypes) {
            if (notfirst)
                str += ",";
            else
                notfirst = true;
            str += type.toString();
        }

        str += "->" + returnType.toString();
        return str;
    }
}

/**
 * The StructSym class is a subclass of the Sym class just for variables 
 * declared to be a struct type. 
 * Each StructSym contains a symbol table to hold information about its 
 * fields.
 */
class StructSym extends SemSym {
    // new fields
    private IdNode structType;  // name of the struct type
    
    public StructSym(IdNode id) {
        super(new StructType(id));
        structType = id;
    }

    public IdNode getStructType() {
        return structType;
    }    
}

/**
 * The StructDefSym class is a subclass of the Sym class just for the 
 * definition of a struct type. 
 * Each StructDefSym contains a symbol table to hold information about its 
 * fields.
 */
class StructDefSym extends SemSym {
    // new fields
    private SymTable symTab;
    
    public StructDefSym(SymTable table) {
        super(new StructDefType());
        symTab = table;
    }

    public SymTable getSymTable() {
        return symTab;
    }
}
