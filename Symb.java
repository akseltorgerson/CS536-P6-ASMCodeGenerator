import java.util.*;

/**
 * The Symb class defines a symbol-table entry. 
 * Each Symb contains a type (a Type).
 */
public class Symb {
    private Type type;
    private int localOffset; //Local offset the instance of Symb
    private static boolean global; //In global or local scope
    private static int currentOffset; //The current offset being used
    
    public Symb(Type type) {
        this.type = type;
    }
    
    public Type getType() {
        return type;
    }
    
    public String toString() {
        return type.toString();
    }

    public void setLocOffset(int offset){
        localOffset = offset;
    }

    public int getLocOffset(){
        return localOffset;
    }

    public static void setCurrOffset(int offset){
        currentOffset = offset;
    }

    public static int getCurrOffset(){
        return currentOffset;
    }

    public static void setGlobal(boolean isGlobal){
        global = isGlobal;
    }

    public static boolean isGlobal(){
        return global;
    }
}

/**
 * The FnSymb class is a subclass of the Symb class just for functions.
 * The returnType field holds the return type and there are fields to hold
 * information about the parameters.
 */
class FnSymb extends Symb {
    // new fields
    private Type returnType;
    private int numParams;
    private List<Type> paramTypes;
    
    public FnSymb(Type type, int numparams) {
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
 * The StructSymb class is a subclass of the Symb class just for variables 
 * declared to be a struct type. 
 * Each StructSymb contains a symbol table to hold information about its 
 * fields.
 */
class StructSymb extends Symb {
    // new fields
    private IdNode structType;  // name of the struct type
    
    public StructSymb(IdNode id) {
        super(new StructType(id));
        structType = id;
    }

    public IdNode getStructType() {
        return structType;
    }    
}

/**
 * The StructDefSymb class is a subclass of the Symb class just for the 
 * definition of a struct type. 
 * Each StructDefSymb contains a symbol table to hold information about its 
 * fields.
 */
class StructDefSymb extends Symb {
    // new fields
    private SymTable symTab;
    
    public StructDefSymb(SymTable table) {
        super(new StructDefType());
        symTab = table;
    }

    public SymTable getSymTable() {
        return symTab;
    }
}
