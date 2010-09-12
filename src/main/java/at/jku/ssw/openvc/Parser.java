package at.jku.ssw.openvc;

import at.jku.ssw.openvc.ast.*;
import at.jku.ssw.openvc.ast.concurrentStatements.*;
import at.jku.ssw.openvc.ast.declarations.*;
import at.jku.ssw.openvc.ast.expressions.*;
import at.jku.ssw.openvc.ast.sequentialStatements.*;
import scala.Either;
import scala.Left;
import scala.Right;
import scala.Tuple2;
import scala.Option;
import scala.collection.Seq;
import scala.collection.mutable.ListBuffer;



public class Parser {
	public static final int _EOF = 0;
	public static final int _BASIC_IDENTIFIER = 1;
	public static final int _EXTENDED_IDENTIFIER = 2;
	public static final int _BASED_LITERAL = 3;
	public static final int _INTEGER_LITERAL = 4;
	public static final int _REAL_LITERAL = 5;
	public static final int _STRING_LITERAL = 6;
	public static final int _BIT_STRING_LITERAL = 7;
	public static final int _CHARACTER_LITERAL = 8;
	public static final int _APOSTROPHE = 9;
	public static final int _ABS = 10;
	public static final int _ACCESS = 11;
	public static final int _AFTER = 12;
	public static final int _ALIAS = 13;
	public static final int _ALL = 14;
	public static final int _AND = 15;
	public static final int _ARCHITECTURE = 16;
	public static final int _ARRAY = 17;
	public static final int _ASSERT = 18;
	public static final int _ATTRIBUTE = 19;
	public static final int _BEGIN = 20;
	public static final int _BLOCK = 21;
	public static final int _BODY = 22;
	public static final int _BUFFER = 23;
	public static final int _BUS = 24;
	public static final int _CASE = 25;
	public static final int _COMPONENT = 26;
	public static final int _CONFIGURATION = 27;
	public static final int _CONSTANT = 28;
	public static final int _DISCONNECT = 29;
	public static final int _DOWNTO = 30;
	public static final int _ELSE = 31;
	public static final int _ELSIF = 32;
	public static final int _END_TOKEN = 33;
	public static final int _ENTITY = 34;
	public static final int _EXIT = 35;
	public static final int _FILE = 36;
	public static final int _FOR = 37;
	public static final int _FUNCTION = 38;
	public static final int _GENERATE = 39;
	public static final int _GENERIC = 40;
	public static final int _GROUP = 41;
	public static final int _GUARDED = 42;
	public static final int _IF_TOKEN = 43;
	public static final int _IMPURE = 44;
	public static final int _IN = 45;
	public static final int _INERTIAL = 46;
	public static final int _INOUT = 47;
	public static final int _IS = 48;
	public static final int _LABEL = 49;
	public static final int _LIBRARY = 50;
	public static final int _LINKAGE = 51;
	public static final int _LITERAL = 52;
	public static final int _LOOP = 53;
	public static final int _MAP = 54;
	public static final int _MOD = 55;
	public static final int _NAND = 56;
	public static final int _NEW = 57;
	public static final int _NEXT = 58;
	public static final int _NOR = 59;
	public static final int _NOT = 60;
	public static final int _NULL = 61;
	public static final int _OF = 62;
	public static final int _ON = 63;
	public static final int _OPEN = 64;
	public static final int _OR = 65;
	public static final int _OTHERS = 66;
	public static final int _OUT = 67;
	public static final int _PACKAGE = 68;
	public static final int _PORT = 69;
	public static final int _POSTPONED = 70;
	public static final int _PROCEDURE = 71;
	public static final int _PROCESS = 72;
	public static final int _PROTECTED = 73;
	public static final int _PURE = 74;
	public static final int _RANGE = 75;
	public static final int _RECORD = 76;
	public static final int _REGISTER = 77;
	public static final int _REJECT = 78;
	public static final int _REM = 79;
	public static final int _REPORT = 80;
	public static final int _RETURN = 81;
	public static final int _ROL = 82;
	public static final int _ROR = 83;
	public static final int _SELECT = 84;
	public static final int _SEVERITY = 85;
	public static final int _SHARED = 86;
	public static final int _SIGNAL = 87;
	public static final int _SLA = 88;
	public static final int _SLL = 89;
	public static final int _SRA = 90;
	public static final int _SRL = 91;
	public static final int _SUBTYPE = 92;
	public static final int _THEN = 93;
	public static final int _TO_TOKEN = 94;
	public static final int _TRANSPORT = 95;
	public static final int _TYPE = 96;
	public static final int _UNAFFECTED = 97;
	public static final int _UNITS = 98;
	public static final int _UNTIL = 99;
	public static final int _USE = 100;
	public static final int _VARIABLE = 101;
	public static final int _WAIT = 102;
	public static final int _WHEN = 103;
	public static final int _WHILE = 104;
	public static final int _WITH = 105;
	public static final int _XNOR = 106;
	public static final int _XOR = 107;
	public static final int _DOUBLESTAR = 108;
	public static final int _LEQ = 109;
	public static final int _GEQ = 110;
	public static final int _ARROW = 111;
	public static final int _NEQ = 112;
	public static final int _VAR_ASSIGN = 113;
	public static final int _BOX = 114;
	public static final int _DBLQUOTE = 115;
	public static final int _SEMICOLON = 116;
	public static final int _COMMA = 117;
	public static final int _AMPERSAND = 118;
	public static final int _LPAREN = 119;
	public static final int _RPAREN = 120;
	public static final int _LBRACKET = 121;
	public static final int _RBRACKET = 122;
	public static final int _COLON = 123;
	public static final int _MUL = 124;
	public static final int _DIV = 125;
	public static final int _PLUS = 126;
	public static final int _MINUS = 127;
	public static final int _LT = 128;
	public static final int _GT = 129;
	public static final int _EQ = 130;
	public static final int _BAR = 131;
	public static final int _DOT = 132;
	public static final int maxT = 134;

	static final boolean T = true;
	static final boolean x = false;
	static final int minErrDist = 2;

	public Token t;    // last recognized token
	public Token la;   // lookahead token
	int errDist = minErrDist;
	
	public Scanner scanner;
	public Errors errors;

	public static class MyListBuffer<T> {
		private ListBuffer<T> impl=new ListBuffer<T>();
		public void append(T x){
			if (x!=null) impl.$plus$eq(x);
		}
		public void appendAll(Seq<T> x){
			if (x!=null) impl.appendAll(x);
		}
		public void prepend(T x){
			if (x!=null) impl.$plus$eq$colon(x);
		}
		public Seq<T> toList(){
			return impl.toList();
		}
		public boolean isEmpty(){
			return impl.isEmpty();
		}
	}
	
	private static <T> Option<T> toOption(T x){return Option.apply(x);}
	
	//group_template:GROUP identifier IS LPAREN ...
	//group_declaration:GROUP identifier COLON ...
	//la == GROUP
	private boolean IsGroupTemplate() {
		if (la.kind!=_GROUP) return false;
		scanner.ResetPeek();
		Token next;
		do {
			next = scanner.Peek();
		}while (next.kind!=_IS && next.kind!=_COLON && next.kind!=_SEMICOLON); 
		return next.kind==_IS;
	}

	//attribute_declaration: ATTRIBUTE identifier COLON type_mark SEMICOLON 
	//attribute_specification: ATTRIBUTE identifier OF entity_name_list COLON entity_class IS expression SEMICOLON 
	//la == ATTRIBUTE
	private boolean IsAttributeDeclaration() {
		if (la.kind!=_ATTRIBUTE) return false;
		scanner.ResetPeek();
		Token next;
		do {
			next = scanner.Peek();
		}while (next.kind!=_COLON && next.kind!=_OF && next.kind!=_SEMICOLON);
		return next.kind==_COLON;
	}

	//physical_type_definition: RANGE range UNITS ...
	//integer_or_floating_point_type_definition: RANGE range SEMICOLON
	//la == RANGE
	private boolean IsPhysicalType() {
		if (la.kind!=_RANGE) return false;
		scanner.ResetPeek();
		Token next;
		do {
			next = scanner.Peek();
		}while (next.kind!=_UNITS && next.kind!=_SEMICOLON);
		return next.kind==_UNITS;
	}

	//constrained array: LPAREN index_subtype_definition (COMMA index_subtype_definition)* RPAREN OF subtype_indication
	//index_subtype_definition: type_mark RANGE BOX
	//index_constraint: LPAREN discrete_range (COMMA  discrete_range)* RPAREN
	//la==LPAREN
	private boolean IsIndexSubtypeDefinition() {
		scanner.ResetPeek();
		Token next;
		do {
			next = scanner.Peek();
		}while (next.kind!=_BOX && next.kind!=_SEMICOLON && next.kind!=_RPAREN && next.kind!=_LPAREN);
		return next.kind==_BOX;
	}
	
	//procedure_call_statement: selected_name [LPAREN association_list RPAREN] SEMICOLON
	//signal_or_variable_assignment_statement: target (VAR_ASSIGN|LEQ) ....
	//target: name | aggregate
	private boolean IsAssignmentStatement() {
		scanner.ResetPeek();
		Token next;
		do {
			next = scanner.Peek();
		}while (next.kind!=_VAR_ASSIGN && next.kind!=_LEQ && next.kind!=_SEMICOLON);
		return next.kind==_VAR_ASSIGN || next.kind==_LEQ;
	}
	
	//concurrent_signal_assignment_statement: (target LEQ | WITH) ....
	//concurrent_procedure_call_statement: selected_name [LPAREN association_list RPAREN] SEMICOLON
	private boolean IsConcurrentSignalSssignmentStatement() {
		scanner.ResetPeek();
		Token next;
		do {
			next = scanner.Peek();
		}while (next.kind!=_LEQ && next.kind!=_WITH && next.kind!=_SEMICOLON);
		return next.kind==_LEQ || next.kind==_WITH;
	}
	//
	//For those parameters with modes, the only modes that are allowed for formal parameters of a procedure are
	//in, inout, and out. If the mode is in and no object class is explicitly specified, constant is assumed. If the
	//mode is inout or out, and no object class is explicitly specified, variable is assumed.
	//
	//interface_constant_declaration: [CONSTANT] identifier_list COLON [IN] subtype_indication [VAR_ASSIGN expression] 
	private boolean IsInterfaceConstantDeclaration() {
		if (la.kind==_CONSTANT) return true;
		else if (la.kind==_VARIABLE || la.kind==_SIGNAL || la.kind==_FILE) return false;
		scanner.ResetPeek();
		Token next;
		do {
			next = scanner.Peek();
		}while (next.kind!=_VAR_ASSIGN && next.kind!=_SEMICOLON && next.kind!=_IN);
		return next.kind==_IN;
	}
	//interface_variable_declaration: [VARIABLE] identifier_list COLON [interface_mode] subtype_indication [VAR_ASSIGN expression]
	private boolean IsInterfaceVariableDeclaration() {
		if (la.kind==_VARIABLE) return true;
		else if (la.kind==_CONSTANT || la.kind==_SIGNAL || la.kind==_FILE) return false;
		scanner.ResetPeek();
		Token next;
		do {
			next = scanner.Peek();
		}while (next.kind!=_VAR_ASSIGN && next.kind!=_SEMICOLON && next.kind!=_IN && next.kind!=_INOUT && next.kind!=_OUT);
		return next.kind==_INOUT || next.kind==_OUT;
	}
	private Position toPosition(Token token){
		return new Position(token.line,token.col);
	}    
	
	private Identifier toIdentifier(Token token){
		return toIdentifier(token,true);
	}

	private Identifier toIdentifier(Token token,boolean toLowerCase){
    	if (token.kind!=_STRING_LITERAL && token.kind!=_CHARACTER_LITERAL){
    		return new Identifier(toPosition(token),toLowerCase?token.val.toLowerCase():token.val.replace("\\\\","\\"));   
    	}else{
    		return new Identifier(toPosition(token),token.val);
    	}
	}      



	public Parser(Scanner scanner) {
		this.scanner = scanner;
		errors = new Errors();
	}

	void SynErr (int n) {
		if (errDist >= minErrDist) errors.SynErr(la.line, la.col, n);
		errDist = 0;
	}

	public void SemErr (String msg) {
		if (errDist >= minErrDist) errors.SemErr(t.line, t.col, msg);
		errDist = 0;
	}
	
	void Get () {
		for (;;) {
			t = la;
			la = scanner.Scan();
			if (la.kind <= maxT) {
				++errDist;
				break;
			}

			la = t;
		}
	}
	
	void Expect (int n) {
		if (la.kind==n) Get(); else { SynErr(n); }
	}
	
	boolean StartOf (int s) {
		return set[s][la.kind];
	}
	
	void ExpectWeak (int n, int follow) {
		if (la.kind == n) Get();
		else {
			SynErr(n);
			while (!StartOf(follow)) Get();
		}
	}
	
	boolean WeakSeparator (int n, int syFol, int repFol) {
		int kind = la.kind;
		if (kind == n) { Get(); return true; }
		else if (StartOf(repFol)) return false;
		else {
			SynErr(n);
			while (!(set[syFol][kind] || set[repFol][kind] || set[0][kind])) {
				Get();
				kind = la.kind;
			}
			return StartOf(syFol);
		}
	}
	
	DesignFile  VHDL() {
		DesignFile  designFile;
		MyListBuffer<DesignUnit> units=new MyListBuffer<DesignUnit>(); 		
		la = new Token();
		la.val = "";		
		Get();
		
		DesignUnit designUnit = design_unit();
		units.append(designUnit); 
		while (StartOf(1)) {
			designUnit = design_unit();
			units.append(designUnit); 
		}
		designFile=new DesignFile(units.toList());
		Expect(0);
		
		return designFile;
	}

	DesignUnit  design_unit() {
		DesignUnit  designUnit;
		MyListBuffer<Identifier> libraries=new MyListBuffer<Identifier>();
		MyListBuffer<UseClause> useClauses=new MyListBuffer<UseClause>();
		
		while (la.kind == 50 || la.kind == 100) {
			if (la.kind == 50) {
				Seq<Identifier> identifierList = library_clause();
				libraries.appendAll(identifierList);
			} else {
				UseClause useClause = use_clause();
				useClauses.append(useClause);
			}
		}
		Position pos=toPosition(la);
		LibraryUnit libraryUnit = library_unit();
		designUnit=new DesignUnit(pos,libraries.toList(),useClauses.toList(),toOption(libraryUnit));
		return designUnit;
	}

	Seq<Identifier>  library_clause() {
		Seq<Identifier>  identifierList;
		Expect(50);
		identifierList = identifier_list();
		Expect(116);
		return identifierList;
	}

	UseClause  use_clause() {
		UseClause  useClause;
		Position pos=toPosition(la);
		Expect(100);
		Seq<SelectedName> list = selected_name_list();
		Expect(116);
		useClause=new UseClause(pos,list);
		return useClause;
	}

	LibraryUnit  library_unit() {
		LibraryUnit  libraryUnit;
		libraryUnit=null;
		if (la.kind == 34) {
			libraryUnit = entity_declaration();
		} else if (la.kind == 16) {
			libraryUnit = architecture_body();
		} else if (scanner.Peek().kind==_BODY) {
			libraryUnit = package_body();
		} else if (la.kind == 68) {
			libraryUnit = package_declaration();
		} else if (la.kind == 27) {
			libraryUnit = configuration_declaration();
		} else SynErr(135);
		return libraryUnit;
	}

	EntityDeclaration  entity_declaration() {
		EntityDeclaration  entityDecl;
		MyListBuffer<DeclarativeItem> declarativeItems=new MyListBuffer<DeclarativeItem>();
		MyListBuffer<ConcurrentStatement> concurrentStmts=new MyListBuffer<ConcurrentStatement>();
		InterfaceList genericClause=null,portClause=null;
		ConcurrentStatement stmt=null;
		Identifier label=null;
		boolean postponed=false;
		
		Expect(34);
		Identifier start_identifier = identifier();
		Expect(48);
		if (la.kind == 40) {
			genericClause = generic_clause();
		}
		if (la.kind == 69) {
			portClause = port_clause();
		}
		while (StartOf(2)) {
			DeclarativeItem item = entity_declarative_item();
			declarativeItems.append(item); 
		}
		if (la.kind == 20) {
			Get();
			while (StartOf(3)) {
				if (scanner.Peek().kind==_COLON) {
					label = label_colon();
				}
				if (la.kind == 70) {
					Get();
					postponed=true;
				}
				if (la.kind == 18) {
					stmt = concurrent_assertion_statement(label,postponed);
				} else if (la.kind == 1 || la.kind == 2 || la.kind == 6) {
					stmt = concurrent_procedure_call_statement(label,postponed);
				} else if (la.kind == 72) {
					stmt = process_statement(label,postponed);
				} else SynErr(136);
				concurrentStmts.append(stmt);
			}
		}
		Expect(33);
		if (la.kind == 34) {
			Get();
		}
		if (la.kind == 1 || la.kind == 2) {
			unused_identifier();
		}
		Expect(116);
		entityDecl=new EntityDeclaration(start_identifier,toOption(genericClause),toOption(portClause),declarativeItems.toList(),concurrentStmts.toList());
		return entityDecl;
	}

	ArchitectureDeclaration  architecture_body() {
		ArchitectureDeclaration  archDecl;
		MyListBuffer<DeclarativeItem> declarativeItems=new MyListBuffer<DeclarativeItem>();
		Expect(16);
		Identifier start_identifier = identifier();
		Expect(62);
		SelectedName entityName = selected_name();
		Expect(48);
		while (StartOf(4)) {
			DeclarativeItem item = block_declarative_item();
			declarativeItems.append(item); 
		}
		Expect(20);
		Seq<ConcurrentStatement> statementList = architecture_statement_list();
		Expect(33);
		if (la.kind == 16) {
			Get();
		}
		if (la.kind == 1 || la.kind == 2) {
			unused_identifier();
		}
		Expect(116);
		archDecl=new ArchitectureDeclaration(start_identifier,declarativeItems.toList(),entityName,statementList); 
		return archDecl;
	}

	PackageBodyDeclaration  package_body() {
		PackageBodyDeclaration  packageBody;
		MyListBuffer<DeclarativeItem> declarativeItems=new MyListBuffer<DeclarativeItem>();
		Expect(68);
		Expect(22);
		Identifier start_identifier = identifier();
		Expect(48);
		while (StartOf(5)) {
			DeclarativeItem item = package_body_declarative_item();
			declarativeItems.append(item);
		}
		Expect(33);
		if (la.kind == 68) {
			Get();
			Expect(22);
		}
		if (la.kind == 1 || la.kind == 2) {
			unused_identifier();
		}
		Expect(116);
		packageBody = new PackageBodyDeclaration(start_identifier,declarativeItems.toList());
		return packageBody;
	}

	PackageDeclaration  package_declaration() {
		PackageDeclaration  packageDecl;
		MyListBuffer<DeclarativeItem> declarativeItems=new MyListBuffer<DeclarativeItem>(); 
		Expect(68);
		Identifier start_identifier = identifier();
		Expect(48);
		while (StartOf(6)) {
			DeclarativeItem item = package_declarative_item();
			declarativeItems.append(item);
		}
		Expect(33);
		if (la.kind == 68) {
			Get();
		}
		if (la.kind == 1 || la.kind == 2) {
			unused_identifier();
		}
		Expect(116);
		packageDecl=new PackageDeclaration(start_identifier,declarativeItems.toList());
		return packageDecl;
	}

	ConfigurationDeclaration  configuration_declaration() {
		ConfigurationDeclaration  configDecl;
		MyListBuffer<DeclarativeItem> declarativeItems=new MyListBuffer<DeclarativeItem>();
		Expect(27);
		Identifier start_identifier = identifier();
		Expect(62);
		SelectedName entityName = selected_name();
		Expect(48);
		while (la.kind == 19 || la.kind == 41 || la.kind == 100) {
			DeclarativeItem item = configuration_declarative_item();
			declarativeItems.append(item);
		}
		BlockConfiguration blockConfig = block_configuration();
		Expect(33);
		if (la.kind == 27) {
			Get();
		}
		if (la.kind == 1 || la.kind == 2) {
			unused_identifier();
		}
		Expect(116);
		configDecl=new ConfigurationDeclaration(start_identifier,declarativeItems.toList(),entityName,blockConfig);
		return configDecl;
	}

	Seq<Identifier>  identifier_list() {
		Seq<Identifier>  list;
		MyListBuffer<Identifier> tmpList=new MyListBuffer<Identifier>();
		Identifier identifier=null;
		
		identifier = identifier();
		tmpList.append(identifier);
		while (la.kind == 117) {
			Get();
			identifier = identifier();
			tmpList.append(identifier);
		}
		list=tmpList.toList();
		return list;
	}

	InterfaceList  generic_clause() {
		InterfaceList  genericList;
		Expect(40);
		Expect(119);
		genericList = generic_interface_list();
		Expect(120);
		Expect(116);
		return genericList;
	}

	InterfaceList  generic_interface_list() {
		InterfaceList  list;
		MyListBuffer<InterfaceList.AbstractInterfaceElement> elements=new MyListBuffer<InterfaceList.AbstractInterfaceElement>(); 
		InterfaceList.InterfaceConstantDeclaration declaration = interface_constant_declaration();
		elements.append(declaration);
		while (la.kind == 116) {
			Get();
			declaration = interface_constant_declaration();
			elements.append(declaration); 
		}
		list=new InterfaceList(elements.toList());
		return list;
	}

	InterfaceList.InterfaceConstantDeclaration  interface_constant_declaration() {
		InterfaceList.InterfaceConstantDeclaration  constElement;
		Expression expr=null;
		if (la.kind == 28) {
			Get();
		}
		Seq<Identifier> list = identifier_list();
		Expect(123);
		if (la.kind == 45) {
			Get();
		}
		SubTypeIndication subType = subtype_indication();
		if (la.kind == 113) {
			Get();
			expr = expression();
		}
		constElement=new InterfaceList.InterfaceConstantDeclaration(list,subType,toOption(expr));
		return constElement;
	}

	InterfaceList  port_clause() {
		InterfaceList  portList;
		Expect(69);
		Expect(119);
		portList = port_interface_list();
		Expect(120);
		Expect(116);
		return portList;
	}

	InterfaceList  port_interface_list() {
		InterfaceList  list;
		MyListBuffer<InterfaceList.AbstractInterfaceElement> elements=new MyListBuffer<InterfaceList.AbstractInterfaceElement>(); 
		InterfaceList.InterfaceSignalDeclaration declaration = interface_signal_declaration_procedure();
		elements.append(declaration); 
		while (la.kind == 116) {
			Get();
			declaration = interface_signal_declaration_procedure();
			elements.append(declaration);
		}
		list=new InterfaceList(elements.toList());
		return list;
	}

	InterfaceList.InterfaceSignalDeclaration  interface_signal_declaration_procedure() {
		InterfaceList.InterfaceSignalDeclaration  signalElement;
		Expression expr=null;boolean bus=false;InterfaceMode mode=null;
		if (la.kind == 87) {
			Get();
		}
		Seq<Identifier> list = identifier_list();
		Expect(123);
		if (StartOf(7)) {
			mode = interface_mode();
		}
		SubTypeIndication subType = subtype_indication();
		if (la.kind == 24) {
			Get();
			bus=true;
		}
		if (la.kind == 113) {
			Get();
			expr = expression();
		}
		signalElement=new InterfaceList.InterfaceSignalDeclaration(list,toOption(mode),subType,bus,toOption(expr));
		return signalElement;
	}

	Identifier  identifier() {
		Identifier  id;
		id=null;
		if (la.kind == 1) {
			Get();
			id=toIdentifier(t);
		} else if (la.kind == 2) {
			Get();
			id=toIdentifier(t,false);
		} else SynErr(137);
		return id;
	}

	DeclarativeItem  entity_declarative_item() {
		DeclarativeItem  item;
		item=null;
		while (!(StartOf(8))) {SynErr(138); Get();}
		if (StartOf(9)) {
			item = subprogram_declaration_or_body();
		} else if (la.kind == 96) {
			item = type_declaration();
		} else if (la.kind == 92) {
			item = subtype_declaration();
		} else if (la.kind == 28) {
			item = constant_declaration();
		} else if (la.kind == 87) {
			item = signal_declaration();
		} else if (la.kind == 86 || la.kind == 101) {
			item = variable_declaration();
		} else if (la.kind == 36) {
			item = file_declaration();
		} else if (la.kind == 13) {
			item = alias_declaration();
		} else if (IsAttributeDeclaration()) {
			item = attribute_declaration();
		} else if (la.kind == 19) {
			item = attribute_specification();
		} else if (la.kind == 29) {
			item = disconnection_specification();
		} else if (la.kind == 100) {
			item = use_clause();
		} else if (IsGroupTemplate()) {
			item = group_template_declaration();
		} else if (la.kind == 41) {
			item = group_declaration();
		} else SynErr(139);
		return item;
	}

	Identifier  label_colon() {
		Identifier  label;
		label = identifier();
		Expect(123);
		return label;
	}

	ConcurrentAssertionStatement  concurrent_assertion_statement(Identifier label,boolean postponed) {
		ConcurrentAssertionStatement  assertStmt;
		Position pos=toPosition(la);Expression report_expression=null,severity_expression=null;
		Expect(18);
		Expression expr = condition();
		if (la.kind == 80) {
			Get();
			report_expression = expression();
		}
		if (la.kind == 85) {
			Get();
			severity_expression = expression();
		}
		Expect(116);
		assertStmt=new ConcurrentAssertionStatement(pos,toOption(label),postponed,expr,toOption(report_expression),toOption(severity_expression));
		return assertStmt;
	}

	ConcurrentProcedureCallStatement  concurrent_procedure_call_statement(Identifier label,boolean postponed) {
		ConcurrentProcedureCallStatement  procedureCallStmt;
		AssociationList paramterList=null;
		SelectedName procedure_name = selected_name();
		if (la.kind == 119) {
			Get();
			paramterList = association_list();
			Expect(120);
		}
		Expect(116);
		procedureCallStmt=new ConcurrentProcedureCallStatement(toOption(label),postponed,procedure_name,toOption(paramterList));
		return procedureCallStmt;
	}

	ProcessStatement  process_statement(Identifier label,boolean postponed) {
		ProcessStatement  processStmt;
		Position pos=toPosition(la);
		MyListBuffer<DeclarativeItem> declarativeItems=new MyListBuffer<DeclarativeItem>();
		Seq<Name> name_list=null;
		
		Expect(72);
		if (la.kind == 119) {
			Get();
			name_list = name_list();
			Expect(120);
		}
		if (la.kind == 48) {
			Get();
		}
		while (StartOf(10)) {
			DeclarativeItem item = process_declarative_item();
			declarativeItems.append(item);
		}
		Expect(20);
		Seq<SequentialStatement> sequentialStatements = sequential_statement_list();
		Expect(33);
		if (la.kind == 70) {
			Get();
		}
		Expect(72);
		if (la.kind == 1 || la.kind == 2) {
			unused_identifier();
		}
		Expect(116);
		processStmt=new ProcessStatement(pos,toOption(label),postponed,toOption(name_list),declarativeItems.toList(),sequentialStatements);
		return processStmt;
	}

	void unused_identifier() {
		Identifier unused = identifier();
	}

	DeclarativeItem  subprogram_declaration_or_body() {
		DeclarativeItem  declOrBody;
		SubProgramDefinition subProgramDef=null;
		SubProgramDeclaration decl = subprogram_specification();
		if (la.kind == 48) {
			subProgramDef = subprogram_body(decl);
		}
		Expect(116);
		if (subProgramDef!=null) declOrBody=subProgramDef; else declOrBody=decl;
		return declOrBody;
	}

	AbstractTypeDeclaration  type_declaration() {
		AbstractTypeDeclaration  typeDecl;
		Position pos=toPosition(la);typeDecl=null;
		Expect(96);
		Identifier id = identifier();
		if (la.kind == 48) {
			Get();
			typeDecl = type_definition(id,pos);
			Expect(116);
		} else if (la.kind == 116) {
			Get();
			typeDecl=new IncompleteTypeDeclaration(pos,id);
		} else SynErr(140);
		return typeDecl;
	}

	SubTypeDeclaration  subtype_declaration() {
		SubTypeDeclaration  subTypeDecl;
		Position pos=toPosition(la);
		Expect(92);
		Identifier identifier = identifier();
		Expect(48);
		SubTypeIndication subType = subtype_indication();
		Expect(116);
		subTypeDecl=new SubTypeDeclaration(pos,identifier,subType);
		return subTypeDecl;
	}

	ConstantDeclaration  constant_declaration() {
		ConstantDeclaration  constantDecl;
		Position pos=toPosition(la);Expression expr=null;
		Expect(28);
		Seq<Identifier> list = identifier_list();
		Expect(123);
		SubTypeIndication subType = subtype_indication();
		if (la.kind == 113) {
			Get();
			expr = expression();
		}
		Expect(116);
		constantDecl=new ConstantDeclaration(pos,list,subType,toOption(expr));
		return constantDecl;
	}

	SignalDeclaration  signal_declaration() {
		SignalDeclaration  signalDecl;
		Position pos=toPosition(la);Expression expr=null;boolean reg=false,bus=false;
		Expect(87);
		Seq<Identifier> list = identifier_list();
		Expect(123);
		SubTypeIndication subType = subtype_indication();
		if (la.kind == 24 || la.kind == 77) {
			if (la.kind == 77) {
				Get();
				reg=true;
			} else {
				Get();
				bus=true;
			}
		}
		if (la.kind == 113) {
			Get();
			expr = expression();
		}
		Expect(116);
		SignalType signalType=null;
		if(reg) signalType=SignalType.REGISTER;
		else if (bus) signalType=SignalType.BUS;
		signalDecl=new SignalDeclaration(pos,list,subType,toOption(signalType),toOption(expr));
		
		return signalDecl;
	}

	VariableDeclaration  variable_declaration() {
		VariableDeclaration  varDecl;
		Position pos=toPosition(la);Expression expr=null;boolean shared=false;
		if (la.kind == 86) {
			Get();
			shared=true;
		}
		Expect(101);
		Seq<Identifier> list = identifier_list();
		Expect(123);
		SubTypeIndication subType = subtype_indication();
		if (la.kind == 113) {
			Get();
			expr = expression();
		}
		Expect(116);
		varDecl=new VariableDeclaration(pos,shared,list,subType,toOption(expr));
		return varDecl;
	}

	FileDeclaration  file_declaration() {
		FileDeclaration  fileDecl;
		Position pos=toPosition(la);Expression file_open_kind_expression=null,file_logical_name=null;
		Expect(36);
		Seq<Identifier> list = identifier_list();
		Expect(123);
		SubTypeIndication subType = subtype_indication();
		if (la.kind == 48 || la.kind == 64) {
			if (la.kind == 64) {
				Get();
				file_open_kind_expression = expression();
			}
			Expect(48);
			file_logical_name = expression();
		}
		Expect(116);
		fileDecl=new FileDeclaration(pos,list,subType,toOption(file_open_kind_expression),toOption(file_logical_name));
		return fileDecl;
	}

	AliasDeclaration  alias_declaration() {
		AliasDeclaration  aliasDecl;
		Position pos=toPosition(la);Signature signature=null;SubTypeIndication subType=null;
		Expect(13);
		Identifier designator = alias_designator();
		if (la.kind == 123) {
			Get();
			subType = subtype_indication();
		}
		Expect(48);
		Name name = name();
		if (la.kind == 121) {
			signature = signature();
		}
		Expect(116);
		aliasDecl=new AliasDeclaration(pos,designator,toOption(subType),name,toOption(signature));
		return aliasDecl;
	}

	AttributeDeclaration  attribute_declaration() {
		AttributeDeclaration  attributeDecl;
		Position pos=toPosition(la);
		Expect(19);
		Identifier identifier = identifier();
		Expect(123);
		SelectedName type = type_mark();
		Expect(116);
		attributeDecl=new AttributeDeclaration(pos,identifier,type);
		return attributeDecl;
	}

	AttributeSpecification  attribute_specification() {
		AttributeSpecification  node;
		Position pos=toPosition(la);Either<Seq<Tuple2<Identifier,Option<Signature>>>,Identifier> nameList=null;
		Expect(19);
		Identifier identifier = identifier();
		Expect(62);
		nameList = entity_name_list();
		Expect(123);
		EntityClass entityClass = entity_class();
		Expect(48);
		Expression expr = expression();
		Expect(116);
		node=new AttributeSpecification(pos,identifier,nameList,entityClass,expr);
		return node;
	}

	DisconnectionSpecification  disconnection_specification() {
		DisconnectionSpecification  disconnectSpec;
		Position pos=toPosition(la);Identifier id=null;Seq<SelectedName> list=null;
		Expect(29);
		if (la.kind == 1 || la.kind == 2 || la.kind == 6) {
			list = selected_name_list();
		} else if (la.kind == 66) {
			Get();
			id=toIdentifier(t);
		} else if (la.kind == 14) {
			Get();
			id=toIdentifier(t);
		} else SynErr(141);
		Expect(123);
		SelectedName type = type_mark();
		Expect(12);
		Expression expr = expression();
		Expect(116);
		disconnectSpec= new DisconnectionSpecification(pos,id==null?new Left<Seq<SelectedName>,Identifier>(list):new Right<Seq<SelectedName>,Identifier>(id),type,expr);
		return disconnectSpec;
	}

	GroupTemplateDeclaration  group_template_declaration() {
		GroupTemplateDeclaration  groupTemplateDecl;
		Position pos=toPosition(la);
		MyListBuffer<GroupTemplateDeclaration.Element> elements=new MyListBuffer<GroupTemplateDeclaration.Element>(); 
		 
		Expect(41);
		Identifier identifier = identifier();
		Expect(48);
		Expect(119);
		GroupTemplateDeclaration.Element entry = entity_class_entry();
		elements.append(entry);
		while (la.kind == 117) {
			Get();
			entry = entity_class_entry();
			elements.append(entry);
		}
		Expect(120);
		Expect(116);
		groupTemplateDecl=new GroupTemplateDeclaration(pos,identifier,elements.toList());
		return groupTemplateDecl;
	}

	GroupDeclaration  group_declaration() {
		GroupDeclaration  groupDecl;
		Position pos=toPosition(la);Seq<Either<Name,Identifier>> list=null;
		Expect(41);
		Identifier identifier = identifier();
		Expect(123);
		SelectedName selectedName = selected_name();
		Expect(119);
		list = group_constituent_list();
		Expect(120);
		Expect(116);
		groupDecl=new GroupDeclaration(pos,identifier,selectedName,list);
		return groupDecl;
	}

	SelectedName  selected_name() {
		SelectedName  name;
		MyListBuffer<Identifier> parts=new MyListBuffer<Identifier>();
		Identifier prefix = name_prefix();
		while (la.kind == 132) {
			Name.SelectedPart selectedPart = name_selected_part();
			parts.append(selectedPart.identifier());
		}
		parts.prepend(prefix); name =new SelectedName(parts.toList());
		return name;
	}

	DeclarativeItem  block_declarative_item() {
		DeclarativeItem  item;
		item=null;
		while (!(StartOf(11))) {SynErr(142); Get();}
		if (StartOf(9)) {
			item = subprogram_declaration_or_body();
		} else if (la.kind == 96) {
			item = type_declaration();
		} else if (la.kind == 92) {
			item = subtype_declaration();
		} else if (la.kind == 28) {
			item = constant_declaration();
		} else if (la.kind == 87) {
			item = signal_declaration();
		} else if (la.kind == 86 || la.kind == 101) {
			item = variable_declaration();
		} else if (la.kind == 36) {
			item = file_declaration();
		} else if (la.kind == 13) {
			item = alias_declaration();
		} else if (la.kind == 26) {
			item = component_declaration();
		} else if (IsAttributeDeclaration()) {
			item = attribute_declaration();
		} else if (la.kind == 19) {
			item = attribute_specification();
		} else if (la.kind == 37) {
			item = configuration_specification();
		} else if (la.kind == 29) {
			item = disconnection_specification();
		} else if (la.kind == 100) {
			item = use_clause();
		} else if (IsGroupTemplate()) {
			item = group_template_declaration();
		} else if (la.kind == 41) {
			item = group_declaration();
		} else SynErr(143);
		return item;
	}

	Seq<ConcurrentStatement>  architecture_statement_list() {
		Seq<ConcurrentStatement>  list;
		MyListBuffer<ConcurrentStatement> statementList=new MyListBuffer<ConcurrentStatement>();
		while (StartOf(12)) {
			ConcurrentStatement stmt = architecture_statement();
			statementList.append(stmt);
		}
		list=statementList.toList();
		return list;
	}

	DeclarativeItem  configuration_declarative_item() {
		DeclarativeItem  item;
		item=null;
		while (!(StartOf(13))) {SynErr(144); Get();}
		if (la.kind == 100) {
			item = use_clause();
		} else if (la.kind == 41) {
			item = group_declaration();
		} else if (la.kind == 19) {
			item = attribute_specification();
		} else SynErr(145);
		return item;
	}

	BlockConfiguration  block_configuration() {
		BlockConfiguration  blockConfig;
		MyListBuffer<UseClause> useClauses=new MyListBuffer<UseClause>();
		MyListBuffer<Object> configurations=new MyListBuffer<Object>();
		
		Expect(37);
		BlockConfigurationSpecification blockSpec = block_specification();
		while (la.kind == 100) {
			UseClause useClause = use_clause();
			useClauses.append(useClause);
		}
		while (la.kind == 37) {
			if (la.kind == 37) {
				BlockConfiguration blockConfiguration = block_configuration();
				configurations.append(blockConfiguration);
			} else {
				ComponentConfiguration componentConfiguration = component_configuration();
				configurations.append(componentConfiguration);
			}
		}
		Expect(33);
		Expect(37);
		Expect(116);
		blockConfig=new BlockConfiguration(blockSpec,useClauses.toList(),configurations.toList());
		return blockConfig;
	}

	Either<DiscreteRange,Expression>  block_configuration_index() {
		Either<DiscreteRange,Expression>  index;
		Expression expr = expression();
		index=new Right<DiscreteRange,Expression>(expr);
		return index;
	}

	Expression  expression() {
		Expression  expr;
		Tuple2<Position,Operators.Logical> op=null;
		expr = relation();
		while (StartOf(14)) {
			op = logical_operator();
			Expression right = relation();
			expr=new LogicalExpression(op._1,expr,op._2,right);
		}
		return expr;
	}

	BlockConfigurationSpecification  block_specification() {
		BlockConfigurationSpecification  blockSpec;
		blockSpec=null;Either<DiscreteRange,Expression> blockIndex=null;
		if (scanner.Peek().kind==_LPAREN) {
			Identifier identifier = identifier();
			if (la.kind == 119) {
				Get();
				blockIndex = block_configuration_index();
				Expect(120);
			}
			blockSpec=new BlockConfigurationSpecification(new Right<SelectedName, Tuple2<Identifier, Option<Either<DiscreteRange, Expression>>>>(new Tuple2<Identifier, Option<Either<DiscreteRange, Expression>>>(identifier,toOption(blockIndex))));
		} else if (la.kind == 1 || la.kind == 2 || la.kind == 6) {
			SelectedName selectedName = selected_name();
			blockSpec=new BlockConfigurationSpecification(new Left<SelectedName, Tuple2<Identifier, Option<Either<DiscreteRange, Expression>>>>(selectedName));
		} else SynErr(146);
		return blockSpec;
	}

	ComponentConfiguration  component_configuration() {
		ComponentConfiguration  componentConfig;
		BlockConfiguration blockConfiguration=null;Object indication=null;
		Expect(37);
		Object componentSpec = component_specification();
		if (StartOf(15)) {
			indication = binding_indication();
			Expect(116);
		}
		if (la.kind == 37) {
			blockConfiguration = block_configuration();
		}
		Expect(33);
		Expect(37);
		Expect(116);
		componentConfig=new ComponentConfiguration(componentSpec,toOption(indication),toOption(blockConfiguration));
		return componentConfig;
	}

	Object  component_specification() {
		Object  spec;
		Object list = instantiation_list();
		Expect(123);
		SelectedName name = selected_name();
		spec=null;
		return spec;
	}

	Object  binding_indication() {
		Object  indication;
		if (la.kind == 100) {
			Get();
			entity_aspect();
		}
		if (la.kind == 40) {
			AssociationList genericMap = generic_map_aspect();
		}
		if (la.kind == 69) {
			AssociationList portMap = port_map_aspect();
		}
		indication=null;
		return indication;
	}

	DeclarativeItem  package_declarative_item() {
		DeclarativeItem  item;
		item=null;
		while (!(StartOf(16))) {SynErr(147); Get();}
		if (StartOf(9)) {
			item = subprogram_declaration();
		} else if (la.kind == 96) {
			item = type_declaration();
		} else if (la.kind == 92) {
			item = subtype_declaration();
		} else if (la.kind == 28) {
			item = constant_declaration();
		} else if (la.kind == 87) {
			item = signal_declaration();
		} else if (la.kind == 86 || la.kind == 101) {
			item = variable_declaration();
		} else if (la.kind == 36) {
			item = file_declaration();
		} else if (la.kind == 13) {
			item = alias_declaration();
		} else if (la.kind == 26) {
			item = component_declaration();
		} else if (IsAttributeDeclaration()) {
			item = attribute_declaration();
		} else if (la.kind == 19) {
			item = attribute_specification();
		} else if (la.kind == 29) {
			item = disconnection_specification();
		} else if (la.kind == 100) {
			item = use_clause();
		} else if (IsGroupTemplate()) {
			item = group_template_declaration();
		} else if (la.kind == 41) {
			item = group_declaration();
		} else SynErr(148);
		return item;
	}

	DeclarativeItem  subprogram_declaration() {
		DeclarativeItem  subprogramDecl;
		subprogramDecl = subprogram_specification();
		Expect(116);
		return subprogramDecl;
	}

	ComponentDeclaration  component_declaration() {
		ComponentDeclaration  componentDecl;
		Position pos=toPosition(la);InterfaceList genericClause=null,portClause=null;
		Expect(26);
		Identifier start_identifier = identifier();
		if (la.kind == 48) {
			Get();
		}
		if (la.kind == 40) {
			genericClause = generic_clause();
		}
		if (la.kind == 69) {
			portClause = port_clause();
		}
		Expect(33);
		Expect(26);
		if (la.kind == 1 || la.kind == 2) {
			unused_identifier();
		}
		Expect(116);
		componentDecl=new ComponentDeclaration(pos,start_identifier,toOption(genericClause),toOption(portClause));
		return componentDecl;
	}

	DeclarativeItem  package_body_declarative_item() {
		DeclarativeItem  item;
		item=null;
		while (!(StartOf(17))) {SynErr(149); Get();}
		if (StartOf(9)) {
			item = subprogram_declaration_or_body();
		} else if (la.kind == 96) {
			item = type_declaration();
		} else if (la.kind == 92) {
			item = subtype_declaration();
		} else if (la.kind == 28) {
			item = constant_declaration();
		} else if (la.kind == 87) {
			item = signal_declaration();
		} else if (la.kind == 86 || la.kind == 101) {
			item = variable_declaration();
		} else if (la.kind == 36) {
			item = file_declaration();
		} else if (la.kind == 13) {
			item = alias_declaration();
		} else if (la.kind == 100) {
			item = use_clause();
		} else if (la.kind == 19) {
			item = attribute_specification();
		} else if (IsGroupTemplate()) {
			item = group_template_declaration();
		} else if (la.kind == 41) {
			item = group_declaration();
		} else SynErr(150);
		return item;
	}

	Identifier  designator() {
		Identifier  identifier;
		identifier = null;
		if (la.kind == 1 || la.kind == 2) {
			identifier = identifier();
		} else if (la.kind == 6) {
			Get();
			identifier=toIdentifier(t);
		} else SynErr(151);
		return identifier;
	}

	SubProgramDeclaration  subprogram_specification() {
		SubProgramDeclaration  decl;
		Position pos=toPosition(la);Identifier designator=null;InterfaceList list=null;decl=null;
		if (la.kind == 71) {
			Get();
			designator = designator();
			if (la.kind == 119) {
				Get();
				list = parameter_interface_list_procedure();
				Expect(120);
			}
			decl=new ProcedureDeclaration(pos,designator,toOption(list));
		} else if (la.kind == 38 || la.kind == 44 || la.kind == 74) {
			boolean pure=true;
			if (la.kind == 44 || la.kind == 74) {
				if (la.kind == 74) {
					Get();
				} else {
					Get();
					pure=false;
				}
			}
			Expect(38);
			designator = designator();
			if (la.kind == 119) {
				Get();
				list = parameter_interface_list_function();
				Expect(120);
			}
			Expect(81);
			SelectedName returnType = type_mark();
			decl=new FunctionDeclaration(pos,pure,designator,toOption(list),returnType);
		} else SynErr(152);
		return decl;
	}

	InterfaceList  parameter_interface_list_procedure() {
		InterfaceList  list;
		MyListBuffer<InterfaceList.AbstractInterfaceElement> elements=new MyListBuffer<InterfaceList.AbstractInterfaceElement>();
		InterfaceList.AbstractInterfaceElement element=null;
		
		element = interface_element_procedure();
		elements.append(element);
		while (la.kind == 116) {
			Get();
			element = interface_element_procedure();
			elements.append(element);
		}
		list=new InterfaceList(elements.toList());
		return list;
	}

	InterfaceList  parameter_interface_list_function() {
		InterfaceList  list;
		MyListBuffer<InterfaceList.AbstractInterfaceElement> elements=new MyListBuffer<InterfaceList.AbstractInterfaceElement>();
		InterfaceList.AbstractInterfaceElement element=null;
		
		element = interface_element_function();
		elements.append(element);
		while (la.kind == 116) {
			Get();
			element = interface_element_function();
			elements.append(element);
		}
		list=new InterfaceList(elements.toList());
		return list;
	}

	SelectedName  type_mark() {
		SelectedName  typeName;
		typeName = selected_name();
		return typeName;
	}

	SubProgramDefinition  subprogram_body(SubProgramDeclaration subprogramDecl) {
		SubProgramDefinition  subProgramDef;
		MyListBuffer<DeclarativeItem> declarativeItems=new MyListBuffer<DeclarativeItem>(); 
		Expect(48);
		while (StartOf(10)) {
			DeclarativeItem item = subprogram_declarative_item();
			declarativeItems.append(item); 
		}
		Expect(20);
		Seq<SequentialStatement> sequentialStatements = sequential_statement_list();
		Expect(33);
		if (la.kind == 38 || la.kind == 71) {
			if (la.kind == 71) {
				Get();
			} else {
				Get();
			}
		}
		if (la.kind == 1 || la.kind == 2 || la.kind == 6) {
			Identifier unused = designator();
		}
		if (subprogramDecl instanceof ProcedureDeclaration){
		ProcedureDeclaration procDecl = (ProcedureDeclaration)subprogramDecl;
		subProgramDef=new ProcedureDefinition(subprogramDecl.position(),procDecl.identifier(),procDecl.parameterInterfaceList(),declarativeItems.toList(),sequentialStatements);
		}else {
			FunctionDeclaration funcDecl=(FunctionDeclaration)subprogramDecl;
			subProgramDef=new FunctionDefinition(subprogramDecl.position(),funcDecl.pure(),funcDecl.identifier(),funcDecl.parameterInterfaceList(),funcDecl.returnType(),declarativeItems.toList(),sequentialStatements);
		}
		
		return subProgramDef;
	}

	DeclarativeItem  subprogram_declarative_item() {
		DeclarativeItem  item;
		item=null;
		while (!(StartOf(18))) {SynErr(153); Get();}
		if (StartOf(9)) {
			item = subprogram_declaration_or_body();
		} else if (la.kind == 96) {
			item = type_declaration();
		} else if (la.kind == 92) {
			item = subtype_declaration();
		} else if (la.kind == 28) {
			item = constant_declaration();
		} else if (la.kind == 86 || la.kind == 101) {
			item = variable_declaration();
		} else if (la.kind == 36) {
			item = file_declaration();
		} else if (la.kind == 13) {
			item = alias_declaration();
		} else if (IsAttributeDeclaration()) {
			item = attribute_declaration();
		} else if (la.kind == 19) {
			item = attribute_specification();
		} else if (la.kind == 100) {
			item = use_clause();
		} else if (IsGroupTemplate()) {
			item = group_template_declaration();
		} else if (la.kind == 41) {
			item = group_declaration();
		} else SynErr(154);
		return item;
	}

	Seq<SequentialStatement>  sequential_statement_list() {
		Seq<SequentialStatement>  list;
		MyListBuffer<SequentialStatement> tmpList=new MyListBuffer<SequentialStatement>();
		while (StartOf(19)) {
			SequentialStatement stmt = sequential_statement();
			tmpList.append(stmt);
		}
		list=tmpList.toList();
		return list;
	}

	AbstractTypeDeclaration  type_definition(Identifier id,Position pos) {
		AbstractTypeDeclaration  typeDef;
		typeDef=null;
		if (la.kind == 119) {
			typeDef = enumeration_type_definition(id,pos);
		} else if (IsPhysicalType()) {
			typeDef = physical_type_definition(id,pos);
		} else if (la.kind == 75) {
			typeDef = integer_or_floating_point_type_definition(id,pos);
		} else if (la.kind == 17) {
			typeDef = array_type_definition(id,pos);
		} else if (la.kind == 76) {
			typeDef = record_type_definition(id,pos);
		} else if (la.kind == 11) {
			typeDef = access_type_definition(id,pos);
		} else if (la.kind == 36) {
			typeDef = file_type_definition(id,pos);
		} else if (la.kind==_PROTECTED && scanner.Peek().kind==_BODY) {
			typeDef = protected_type_body(id,pos);
		} else if (la.kind == 73) {
			typeDef = protected_type_declaration(id,pos);
		} else SynErr(155);
		return typeDef;
	}

	EnumerationTypeDefinition  enumeration_type_definition(Identifier id,Position pos) {
		EnumerationTypeDefinition  enumTypeDef;
		MyListBuffer<Identifier> elements=new MyListBuffer<Identifier>(); 
		Identifier element=null;
		
		Expect(119);
		element = enumeration_literal();
		elements.append(element);
		while (la.kind == 117) {
			Get();
			element = enumeration_literal();
			elements.append(element);
		}
		Expect(120);
		enumTypeDef=new EnumerationTypeDefinition(pos,id,elements.toList());
		return enumTypeDef;
	}

	PhysicalTypeDefinition  physical_type_definition(Identifier id,Position pos) {
		PhysicalTypeDefinition  physicalTypeDef;
		MyListBuffer<PhysicalTypeDefinition.Element> elements=new MyListBuffer<PhysicalTypeDefinition.Element>(); 
		Expect(75);
		Range range = range();
		Expect(98);
		Identifier baseIdentifier = identifier();
		Expect(116);
		while (la.kind == 1 || la.kind == 2) {
			Identifier identifier = identifier();
			Expect(130);
			PhysicalLiteral literal = physical_literal();
			Expect(116);
			elements.append(new PhysicalTypeDefinition.Element(identifier,literal));
		}
		Expect(33);
		Expect(98);
		if (la.kind == 1 || la.kind == 2) {
			unused_identifier();
		}
		physicalTypeDef=new PhysicalTypeDefinition(pos,id,range,baseIdentifier,elements.toList());
		return physicalTypeDef;
	}

	IntegerOrFloatingPointTypeDefinition  integer_or_floating_point_type_definition(Identifier id,Position pos) {
		IntegerOrFloatingPointTypeDefinition  intOrFloat;
		Expect(75);
		Range range = range();
		intOrFloat=new IntegerOrFloatingPointTypeDefinition(pos,id,range);
		return intOrFloat;
	}

	AbstractArrayTypeDefinition  array_type_definition(Identifier id,Position pos) {
		AbstractArrayTypeDefinition  arrayTypeDef;
		MyListBuffer<SelectedName> unConstraintList=new MyListBuffer<SelectedName>();
		SubTypeIndication subType=null;
		Seq<DiscreteRange> ranges =null;
		SelectedName type=null;
		
		Expect(17);
		if (IsIndexSubtypeDefinition()) {
			Expect(119);
			type = index_subtype_definition();
			unConstraintList.append(type);
			while (la.kind == 117) {
				Get();
				type = index_subtype_definition();
				unConstraintList.append(type);
			}
			Expect(120);
			Expect(62);
			subType = subtype_indication();
		} else if (la.kind == 119) {
			ranges = index_constraint();
			Expect(62);
			subType = subtype_indication();
		} else SynErr(156);
		if (unConstraintList.isEmpty()) arrayTypeDef=new ConstrainedArrayTypeDefinition(pos,id,ranges,subType);
		else arrayTypeDef=new UnconstrainedArrayTypeDefinition(pos,id,unConstraintList.toList(),subType);
		
		return arrayTypeDef;
	}

	RecordTypeDefinition  record_type_definition(Identifier id,Position pos) {
		RecordTypeDefinition  recordTypeDef;
		MyListBuffer<RecordTypeDefinition.Element> elements=new MyListBuffer<RecordTypeDefinition.Element>(); 
		Expect(76);
		Seq<Identifier> list = identifier_list();
		Expect(123);
		SubTypeIndication subType = subtype_indication();
		Expect(116);
		elements.append(new RecordTypeDefinition.Element(list, subType));
		while (la.kind == 1 || la.kind == 2) {
			list = identifier_list();
			Expect(123);
			subType = subtype_indication();
			Expect(116);
			elements.append(new RecordTypeDefinition.Element(list, subType));
		}
		Expect(33);
		Expect(76);
		if (la.kind == 1 || la.kind == 2) {
			unused_identifier();
		}
		recordTypeDef=new RecordTypeDefinition(pos,id,elements.toList());
		return recordTypeDef;
	}

	AccessTypeDefinition  access_type_definition(Identifier id,Position pos) {
		AccessTypeDefinition  accessTypeDef;
		Expect(11);
		SubTypeIndication subType = subtype_indication();
		accessTypeDef=new AccessTypeDefinition(pos,id,subType);
		return accessTypeDef;
	}

	FileTypeDefinition  file_type_definition(Identifier id,Position pos) {
		FileTypeDefinition  fileTypeDef;
		Expect(36);
		Expect(62);
		SelectedName type = type_mark();
		fileTypeDef=new FileTypeDefinition(pos,id,type);
		return fileTypeDef;
	}

	ProtectedTypeBodyDeclaration  protected_type_body(Identifier id,Position pos) {
		ProtectedTypeBodyDeclaration  protectedTypeBody;
		MyListBuffer<DeclarativeItem> items=new MyListBuffer<DeclarativeItem>(); 
		Expect(73);
		Expect(22);
		while (StartOf(10)) {
			DeclarativeItem item = protected_type_body_declarative_item();
			items.append(item);
		}
		Expect(33);
		Expect(73);
		Expect(22);
		if (la.kind == 1 || la.kind == 2) {
			unused_identifier();
		}
		protectedTypeBody=new ProtectedTypeBodyDeclaration(pos,id,items.toList());
		return protectedTypeBody;
	}

	ProtectedTypeDeclaration  protected_type_declaration(Identifier id,Position pos) {
		ProtectedTypeDeclaration  protectedTypeDecl;
		MyListBuffer<DeclarativeItem> items=new MyListBuffer<DeclarativeItem>(); 
		Expect(73);
		while (StartOf(20)) {
			DeclarativeItem item = protected_type_declarative_item();
			items.append(item);
		}
		Expect(33);
		Expect(73);
		if (la.kind == 1 || la.kind == 2) {
			unused_identifier();
		}
		protectedTypeDecl=new ProtectedTypeDeclaration(pos,id,items.toList());
		return protectedTypeDecl;
	}

	SubTypeIndication  subtype_indication() {
		SubTypeIndication  subType;
		Either<Range,Seq<DiscreteRange>> constraint=null;SelectedName n2=null;
		SelectedName n1 = selected_name();
		if (la.kind == 1 || la.kind == 2 || la.kind == 6) {
			n2 = selected_name();
		}
		if (la.kind == 75 || la.kind == 119) {
			constraint = constraint();
		}
		if (n2!=null) subType=new SubTypeIndication(toOption(n1),n2,toOption(constraint));
		else subType=new SubTypeIndication(n1,toOption(constraint));
		
		return subType;
	}

	Identifier  alias_designator() {
		Identifier  identifier;
		identifier=null;
		if (la.kind == 1 || la.kind == 2) {
			identifier = identifier();
		} else if (la.kind == 8) {
			Get();
			identifier=toIdentifier(t);
		} else if (la.kind == 6) {
			Get();
			identifier=toIdentifier(t);
		} else SynErr(157);
		return identifier;
	}

	Name  name() {
		Name  name;
		MyListBuffer<Name.Part> parts=new MyListBuffer<Name.Part>();
		Identifier prefix = name_prefix();
		name =new Name(prefix,parts.toList());
		return name;
	}

	Signature  signature() {
		Signature  signature;
		Seq<SelectedName> list=null;SelectedName returnType=null;
		Expect(121);
		if (la.kind == 1 || la.kind == 2 || la.kind == 6) {
			list = selected_name_list();
		}
		if (la.kind == 81) {
			Get();
			returnType = type_mark();
		}
		Expect(122);
		signature =new Signature(toOption(list),toOption(returnType));
		return signature;
	}

	Either<Seq<Tuple2<Identifier,Option<Signature>>>,Identifier>  entity_name_list() {
		Either<Seq<Tuple2<Identifier,Option<Signature>>>,Identifier>  list;
		MyListBuffer<Tuple2<Identifier,Option<Signature>>> elements=new MyListBuffer<Tuple2<Identifier,Option<Signature>>>();
		Tuple2<Identifier,Option<Signature>> designator=null;
		list=null;
		
		if (StartOf(21)) {
			designator = entity_designator();
			elements.append(designator); 
			while (la.kind == 117) {
				Get();
				designator = entity_designator();
				elements.append(designator);
			}
			list=new Left<Seq<Tuple2<Identifier,Option<Signature>>>,Identifier>(elements.toList());
		} else if (la.kind == 66) {
			Get();
			list=new Right<Seq<Tuple2<Identifier,Option<Signature>>>,Identifier>(toIdentifier(t));
		} else if (la.kind == 14) {
			Get();
			list=new Right<Seq<Tuple2<Identifier,Option<Signature>>>,Identifier>(toIdentifier(t));
		} else SynErr(158);
		return list;
	}

	EntityClass  entity_class() {
		EntityClass  entityClass;
		entityClass=null;
		switch (la.kind) {
		case 34: {
			Get();
			entityClass=EntityClass.ENTITY;
			break;
		}
		case 16: {
			Get();
			entityClass=EntityClass.ARCHITECTURE;
			break;
		}
		case 27: {
			Get();
			entityClass=EntityClass.CONFIGURATION;
			break;
		}
		case 68: {
			Get();
			entityClass=EntityClass.PACKAGE;
			break;
		}
		case 71: {
			Get();
			entityClass=EntityClass.PROCEDURE;
			break;
		}
		case 38: {
			Get();
			entityClass=EntityClass.FUNCTION;
			break;
		}
		case 96: {
			Get();
			entityClass=EntityClass.TYPE;
			break;
		}
		case 92: {
			Get();
			entityClass=EntityClass.SUBTYPE;
			break;
		}
		case 28: {
			Get();
			entityClass=EntityClass.CONSTANT;
			break;
		}
		case 87: {
			Get();
			entityClass=EntityClass.SIGNAL;
			break;
		}
		case 101: {
			Get();
			entityClass=EntityClass.VARIABLE;
			break;
		}
		case 36: {
			Get();
			entityClass=EntityClass.FILE;
			break;
		}
		case 26: {
			Get();
			entityClass=EntityClass.COMPONENT;
			break;
		}
		case 49: {
			Get();
			entityClass=EntityClass.LABEL;
			break;
		}
		case 52: {
			Get();
			entityClass=EntityClass.LITERAL;
			break;
		}
		case 98: {
			Get();
			entityClass=EntityClass.UNITS;
			break;
		}
		case 41: {
			Get();
			entityClass=EntityClass.GROUP;
			break;
		}
		default: SynErr(159); break;
		}
		return entityClass;
	}

	Tuple2<Identifier,Option<Signature>>  entity_designator() {
		Tuple2<Identifier,Option<Signature>>  designator;
		Identifier identifier=null; Signature signature=null;
		if (la.kind == 1 || la.kind == 2) {
			identifier = identifier();
		} else if (la.kind == 8) {
			Get();
			identifier=toIdentifier(t);
		} else if (la.kind == 6) {
			Get();
			identifier=toIdentifier(t);
		} else SynErr(160);
		if (la.kind == 121) {
			signature = signature();
		}
		designator=new Tuple2<Identifier,Option<Signature>>(identifier,toOption(signature)); 
		return designator;
	}

	ConfigurationSpecification  configuration_specification() {
		ConfigurationSpecification  configSpec;
		Position pos=toPosition(la);
		Expect(37);
		Object componentSpec = component_specification();
		Object indication = binding_indication();
		Expect(116);
		configSpec= new ConfigurationSpecification(pos);
		return configSpec;
	}

	Either<Seq<Identifier>,Identifier>  instantiation_list() {
		Either<Seq<Identifier>,Identifier>  list;
		list=null;
		if (la.kind == 1 || la.kind == 2) {
			Seq<Identifier> identifierList = identifier_list();
			list=new Left<Seq<Identifier>,Identifier>(identifierList);
		} else if (la.kind == 66) {
			Get();
			list=new Right<Seq<Identifier>,Identifier>(toIdentifier(t));
		} else if (la.kind == 14) {
			Get();
			list=new Right<Seq<Identifier>,Identifier>(toIdentifier(t));
		} else SynErr(161);
		return list;
	}

	void entity_aspect() {
		if (la.kind == 34) {
			Get();
			SelectedName entity_name = selected_name();
			if (la.kind == 119) {
				Get();
				Identifier architecture_identifier = identifier();
				Expect(120);
			}
		} else if (la.kind == 27) {
			Get();
			SelectedName configuration_name = selected_name();
		} else if (la.kind == 64) {
			Get();
		} else SynErr(162);
	}

	AssociationList  generic_map_aspect() {
		AssociationList  associationList;
		Expect(40);
		Expect(54);
		Expect(119);
		associationList = association_list();
		Expect(120);
		return associationList;
	}

	AssociationList  port_map_aspect() {
		AssociationList  associationList;
		Expect(69);
		Expect(54);
		Expect(119);
		associationList = association_list();
		Expect(120);
		return associationList;
	}

	Seq<SelectedName>  selected_name_list() {
		Seq<SelectedName>  list;
		MyListBuffer<SelectedName> tmpList=new MyListBuffer<SelectedName>();
		SelectedName name;
		
		name = selected_name();
		tmpList.append(name);
		while (la.kind == 117) {
			Get();
			name = selected_name();
			tmpList.append(name);
		}
		list=tmpList.toList();
		return list;
	}

	GroupTemplateDeclaration.Element  entity_class_entry() {
		GroupTemplateDeclaration.Element  entry;
		boolean box=false;
		EntityClass  entityClass = entity_class();
		if (la.kind == 114) {
			Get();
			box=true;
		}
		entry = new GroupTemplateDeclaration.Element(entityClass,box);
		return entry;
	}

	Seq<Either<Name,Identifier>>  group_constituent_list() {
		Seq<Either<Name,Identifier>>  list;
		MyListBuffer<Either<Name,Identifier>> elements=new MyListBuffer<Either<Name,Identifier>>(); 
		Either<Name,Identifier> element=null;
		
		element = group_constituent();
		elements.append(element); 
		while (la.kind == 117) {
			Get();
			element = group_constituent();
			elements.append(element);
		}
		list=elements.toList();
		return list;
	}

	Either<Name,Identifier>  group_constituent() {
		Either<Name,Identifier>  constituent;
		constituent=null;
		if (la.kind == 1 || la.kind == 2 || la.kind == 6) {
			Name name = name();
			constituent=new Left<Name,Identifier>(name);
		} else if (la.kind == 8) {
			Get();
			constituent=new Right<Name,Identifier>(toIdentifier(t));
		} else SynErr(163);
		return constituent;
	}

	Identifier  enumeration_literal() {
		Identifier  identifier;
		identifier=null;
		if (la.kind == 1 || la.kind == 2) {
			identifier = identifier();
		} else if (la.kind == 8) {
			Get();
			identifier=toIdentifier(t);
		} else SynErr(164);
		return identifier;
	}

	Range  range() {
		Range  range;
		Expression from = simple_expression();
		Direction rangeDirection = direction();
		Expression to = simple_expression();
		range =new Range(from,rangeDirection,to);
		return range;
	}

	PhysicalLiteral  physical_literal() {
		PhysicalLiteral  literal;
		String text=la.val;
		LiteralType literalType=null;
		Position pos=toPosition(la);
		
		if (la.kind == 4) {
			Get();
			literalType=LiteralType.INTEGER_LITERAL;
		} else if (la.kind == 5) {
			Get();
			literalType=LiteralType.REAL_LITERAL;
		} else SynErr(165);
		Identifier unit_name = identifier();
		literal =new PhysicalLiteral(pos,text,unit_name,literalType);
		return literal;
	}

	SelectedName  index_subtype_definition() {
		SelectedName  typeMark;
		typeMark = type_mark();
		Expect(75);
		Expect(114);
		return typeMark;
	}

	Seq<DiscreteRange>  index_constraint() {
		Seq<DiscreteRange>  ranges;
		MyListBuffer<DiscreteRange> list=new MyListBuffer<DiscreteRange>();
		DiscreteRange discreteRange=null;
		
		Expect(119);
		discreteRange = discrete_range();
		list.append(discreteRange);
		while (la.kind == 117) {
			Get();
			discreteRange = discrete_range();
			list.append(discreteRange);
		}
		Expect(120);
		ranges = list.toList();
		return ranges;
	}

	DeclarativeItem  protected_type_declarative_item() {
		DeclarativeItem  item;
		item=null;
		while (!(StartOf(22))) {SynErr(166); Get();}
		if (StartOf(9)) {
			item = subprogram_declaration();
		} else if (la.kind == 19) {
			item = attribute_specification();
		} else if (la.kind == 100) {
			item = use_clause();
		} else SynErr(167);
		return item;
	}

	DeclarativeItem  protected_type_body_declarative_item() {
		DeclarativeItem  item;
		item=null;
		while (!(StartOf(18))) {SynErr(168); Get();}
		if (StartOf(9)) {
			item = subprogram_declaration_or_body();
		} else if (la.kind == 96) {
			item = type_declaration();
		} else if (la.kind == 92) {
			item = subtype_declaration();
		} else if (la.kind == 28) {
			item = constant_declaration();
		} else if (la.kind == 86 || la.kind == 101) {
			item = variable_declaration();
		} else if (la.kind == 36) {
			item = file_declaration();
		} else if (la.kind == 13) {
			item = alias_declaration();
		} else if (IsAttributeDeclaration()) {
			item = attribute_declaration();
		} else if (la.kind == 19) {
			item = attribute_specification();
		} else if (la.kind == 100) {
			item = use_clause();
		} else if (IsGroupTemplate()) {
			item = group_template_declaration();
		} else if (la.kind == 41) {
			item = group_declaration();
		} else SynErr(169);
		return item;
	}

	Either<Range,Seq<DiscreteRange>>  constraint() {
		Either<Range,Seq<DiscreteRange>>  constraint;
		constraint=null;
		if (la.kind == 75) {
			Range rangeContraint = range_constraint();
			constraint = new Left<Range,Seq<DiscreteRange>>(rangeContraint);
		} else if (la.kind == 119) {
			Seq<DiscreteRange> ranges = index_constraint();
			constraint = new Right<Range,Seq<DiscreteRange>>(ranges);
		} else SynErr(170);
		return constraint;
	}

	Direction  direction() {
		Direction  rangeDirection;
		rangeDirection=null;
		if (la.kind == 94) {
			Get();
			rangeDirection=Direction.To;
		} else if (la.kind == 30) {
			Get();
			rangeDirection=Direction.Downto;
		} else SynErr(171);
		return rangeDirection;
	}

	Range  range_constraint() {
		Range  rangeContraint;
		Expect(75);
		rangeContraint = range();
		return rangeContraint;
	}

	DiscreteRange  discrete_range() {
		DiscreteRange  discreteRange;
		Range range = range();
		discreteRange=new DiscreteRange(new Left<Range, SubTypeIndication>(range));
		return discreteRange;
	}

	Expression  simple_expression() {
		Expression  simpleExpr;
		Tuple2<Position,Operators.Sign> sign=null;Tuple2<Position,Operators.Add> op=null;
		if (la.kind == 126 || la.kind == 127) {
			sign = sign();
		}
		simpleExpr = term();
		if (sign!=null) simpleExpr=new SimpleExpression(sign._1,sign._2,simpleExpr);
		while (la.kind == 118 || la.kind == 126 || la.kind == 127) {
			op = adding_operator();
			Expression right = term();
			simpleExpr=new SimpleExpression(op._1,simpleExpr,op._2,right);
		}
		return simpleExpr;
	}

	ConcurrentStatement  architecture_statement() {
		ConcurrentStatement  concurrentStmt;
		concurrentStmt=null; Identifier label=null;
		if (scanner.Peek().kind==_COLON) {
			label = label_colon();
			concurrentStmt = architecture_statement_with_label(label);
			concurrentStmt = architecture_statement_optional_label(label);
		} else if (StartOf(12)) {
			concurrentStmt = architecture_statement_optional_label(label);
		} else SynErr(172);
		return concurrentStmt;
	}

	ConcurrentStatement  architecture_statement_with_label(Identifier label) {
		ConcurrentStatement  concurrentStmt;
		concurrentStmt=null;
		if (la.kind == 26 || la.kind == 27 || la.kind == 34) {
			concurrentStmt = component_instantiation_statement(label);
		} else if (la.kind == 21) {
			concurrentStmt = block_statement(label);
		} else if (la.kind == 37 || la.kind == 43) {
			concurrentStmt = generate_statement(label);
		} else SynErr(173);
		return concurrentStmt;
	}

	ConcurrentStatement  architecture_statement_optional_label(Identifier label) {
		ConcurrentStatement  concurrentStmt;
		concurrentStmt=null;boolean postponed=false;
		if (la.kind == 70) {
			Get();
			postponed=true;
		}
		if (la.kind == 72) {
			concurrentStmt = process_statement(label,postponed);
		} else if (la.kind == 18) {
			concurrentStmt = concurrent_assertion_statement(label,postponed);
		} else if (IsConcurrentSignalSssignmentStatement()) {
			concurrentStmt = concurrent_signal_assignment_statement(label,postponed);
		} else if (la.kind == 1 || la.kind == 2 || la.kind == 6) {
			concurrentStmt = concurrent_procedure_call_statement(label,postponed);
		} else SynErr(174);
		return concurrentStmt;
	}

	ComponentInstantiationStatement  component_instantiation_statement(Identifier label) {
		ComponentInstantiationStatement  stmt;
		ComponentType componentType =null;
		AssociationList genericMap=null,portMap=null;
		Position pos=toPosition(la);
		SelectedName name=null;
		Identifier architecture=null;
		
		if (la.kind == 26) {
			Get();
			name = selected_name();
			componentType=ComponentType.COMPONENT;
		} else if (la.kind == 34) {
			Get();
			name = selected_name();
			if (la.kind == 119) {
				Get();
				architecture = identifier();
				Expect(120);
			}
			componentType=ComponentType.ENTITY;
		} else if (la.kind == 27) {
			Get();
			name = selected_name();
			componentType=ComponentType.CONFIGURATION;
		} else SynErr(175);
		if (la.kind == 40) {
			genericMap = generic_map_aspect();
		}
		if (la.kind == 69) {
			portMap = port_map_aspect();
		}
		Expect(116);
		stmt=new ComponentInstantiationStatement(pos,label,componentType,name,toOption(architecture),toOption(genericMap),toOption(portMap));
		return stmt;
	}

	BlockStatement  block_statement(Identifier label) {
		BlockStatement  blockStmt;
		Position pos=toPosition(la);
		MyListBuffer<DeclarativeItem> declarativeItems=new MyListBuffer<DeclarativeItem>(); 
		Expression guard_expression=null;
		InterfaceList genericClause=null,portClause=null;
		AssociationList genericMap=null,portMap=null;
		
		Expect(21);
		if (la.kind == 119) {
			Get();
			guard_expression = expression();
			Expect(120);
		}
		if (la.kind == 48) {
			Get();
		}
		if (la.kind == 40) {
			genericClause = generic_clause();
			if (la.kind == 40) {
				genericMap = generic_map_aspect();
				Expect(116);
			}
		}
		if (la.kind == 69) {
			portClause = port_clause();
			if (la.kind == 69) {
				portMap = port_map_aspect();
				Expect(116);
			}
		}
		while (StartOf(4)) {
			DeclarativeItem item = block_declarative_item();
			declarativeItems.append(item);
		}
		Expect(20);
		Seq<ConcurrentStatement> statementList = architecture_statement_list();
		Expect(33);
		Expect(21);
		if (la.kind == 1 || la.kind == 2) {
			unused_identifier();
		}
		Expect(116);
		blockStmt=new BlockStatement(pos,toOption(label),toOption(guard_expression),toOption(genericClause),toOption(genericMap),toOption(portClause),toOption(portMap),declarativeItems.toList(),statementList);
		return blockStmt;
	}

	ConcurrentStatement  generate_statement(Identifier label) {
		ConcurrentStatement  generateStmt;
		generateStmt=null;
		if (la.kind == 37) {
			generateStmt = for_generate_statement(label);
		} else if (la.kind == 43) {
			generateStmt = if_generate_statement(label);
		} else SynErr(176);
		return generateStmt;
	}

	ConcurrentSignalAssignmentStatement  concurrent_signal_assignment_statement(Identifier label,boolean postponed) {
		ConcurrentSignalAssignmentStatement  signalAssignmentStatement;
		signalAssignmentStatement=null;
		if (StartOf(23)) {
			signalAssignmentStatement = conditional_signal_assignment(label,postponed);
		} else if (la.kind == 105) {
			signalAssignmentStatement = selected_signal_assignment(label,postponed);
		} else SynErr(177);
		return signalAssignmentStatement;
	}

	AssociationList  association_list() {
		AssociationList  list;
		MyListBuffer<AssociationList.Element> elements=new MyListBuffer<AssociationList.Element>();
		AssociationList.Element element=null;
		
		element = association_element();
		elements.append(element);
		while (la.kind == 117) {
			Get();
			element = association_element();
			elements.append(element);
		}
		list=new AssociationList(elements.toList());
		return list;
	}

	Seq<Name>  name_list() {
		Seq<Name>  list;
		MyListBuffer<Name> tmpList=new MyListBuffer<Name>();
		Name name = name();
		tmpList.append(name);
		while (la.kind == 117) {
			Get();
			name = name();
			tmpList.append(name);
		}
		list=tmpList.toList();
		return list;
	}

	DeclarativeItem  process_declarative_item() {
		DeclarativeItem  item;
		item=null;
		while (!(StartOf(18))) {SynErr(178); Get();}
		if (StartOf(9)) {
			item = subprogram_declaration_or_body();
		} else if (la.kind == 96) {
			item = type_declaration();
		} else if (la.kind == 92) {
			item = subtype_declaration();
		} else if (la.kind == 28) {
			item = constant_declaration();
		} else if (la.kind == 86 || la.kind == 101) {
			item = variable_declaration();
		} else if (la.kind == 36) {
			item = file_declaration();
		} else if (la.kind == 13) {
			item = alias_declaration();
		} else if (la.kind == 100) {
			item = use_clause();
		} else if (IsAttributeDeclaration()) {
			item = attribute_declaration();
		} else if (la.kind == 19) {
			item = attribute_specification();
		} else if (IsGroupTemplate()) {
			item = group_template_declaration();
		} else if (la.kind == 41) {
			item = group_declaration();
		} else SynErr(179);
		return item;
	}

	Expression  condition() {
		Expression  expr;
		expr = expression();
		return expr;
	}

	ConcurrentConditionalSignalAssignment  conditional_signal_assignment(Identifier label,boolean postponed) {
		ConcurrentConditionalSignalAssignment  signalAssignment;
		MyListBuffer<ConcurrentConditionalSignalAssignment.When> elements=new MyListBuffer<ConcurrentConditionalSignalAssignment.When>();
		boolean guarded=false;
		DelayMechanism delay=null;
		
		Target target = target();
		Expect(109);
		Position pos=toPosition(t);
		if (la.kind == 42) {
			Get();
			guarded=true;
		}
		if (la.kind == 46 || la.kind == 78 || la.kind == 95) {
			delay = delay_mechanism();
		}
		conditional_waveforms(elements);
		Expect(116);
		signalAssignment=new ConcurrentConditionalSignalAssignment(pos,toOption(label),postponed,target,guarded,toOption(delay),elements.toList());
		return signalAssignment;
	}

	ConcurrentSelectedSignalAssignment  selected_signal_assignment(Identifier label,boolean postponed) {
		ConcurrentSelectedSignalAssignment  signalAssignment;
		Position pos=toPosition(la);
		MyListBuffer<ConcurrentSelectedSignalAssignment.When> elements=new MyListBuffer<ConcurrentSelectedSignalAssignment.When>(); 
		boolean guarded=false;
		ConcurrentSelectedSignalAssignment.When when=null;
		DelayMechanism delay=null;
		
		Expect(105);
		Expression expr = expression();
		Expect(84);
		Target target = target();
		Expect(109);
		if (la.kind == 42) {
			Get();
			guarded=true;
		}
		if (la.kind == 46 || la.kind == 78 || la.kind == 95) {
			delay = delay_mechanism();
		}
		when = selected_waveform();
		elements.append(when);
		while (la.kind == 117) {
			Get();
			when = selected_waveform();
			elements.append(when);
		}
		Expect(116);
		signalAssignment=new ConcurrentSelectedSignalAssignment(pos,toOption(label),postponed,expr,target,guarded,toOption(delay),elements.toList());
		return signalAssignment;
	}

	Target  target() {
		Target  target;
		target=null;
		if (la.kind == 1 || la.kind == 2 || la.kind == 6) {
			Name name = name();
			target = new Target(new Left<Name, Aggregate>(name));
		} else if (la.kind == 119) {
			Aggregate aggregate = aggregate();
			target = new Target(new Right<Name, Aggregate>(aggregate));
		} else SynErr(180);
		return target;
	}

	DelayMechanism  delay_mechanism() {
		DelayMechanism  mechanism;
		Expression time_expression=null;
		if (la.kind == 95) {
			Get();
		} else if (la.kind == 46 || la.kind == 78) {
			if (la.kind == 78) {
				Get();
				time_expression = expression();
			}
			Expect(46);
		} else SynErr(181);
		if (time_expression==null) mechanism=new DelayMechanism(DelayType.TRANSPORT,toOption(time_expression));
		else mechanism=new DelayMechanism(DelayType.INERTIAL,toOption(time_expression));
		
		return mechanism;
	}

	void conditional_waveforms(MyListBuffer<ConcurrentConditionalSignalAssignment.When> elements) {
		Expression expr=null;
		Waveform waveform = waveform();
		if (la.kind == 103) {
			Get();
			expr = condition();
			if (la.kind == 31) {
				Get();
				conditional_waveforms(elements);
			}
		}
		elements.prepend(new ConcurrentConditionalSignalAssignment.When(waveform,expr));
	}

	Waveform  waveform() {
		Waveform  waveForm;
		MyListBuffer<Waveform.Element> elements=new MyListBuffer<Waveform.Element>();
		if (StartOf(24)) {
			Waveform.Element element = waveform_element();
			elements.append(element);
			while (la.kind == 117) {
				Get();
				element = waveform_element();
				elements.append(element);
			}
		} else if (la.kind == 97) {
			Get();
		} else SynErr(182);
		waveForm=new Waveform(elements.toList());
		return waveForm;
	}

	ConcurrentSelectedSignalAssignment.When  selected_waveform() {
		ConcurrentSelectedSignalAssignment.When  whenClause;
		Waveform waveform = waveform();
		Expect(103);
		Choices choices = choices();
		whenClause = new ConcurrentSelectedSignalAssignment.When(waveform,choices);
		return whenClause;
	}

	Choices  choices() {
		Choices  choices;
		MyListBuffer<Choices.Choice> elements=new MyListBuffer<Choices.Choice>(); 
		Choices.Choice choice = choice();
		elements.append(choice);
		while (la.kind == 131) {
			Get();
			choice = choice();
			elements.append(choice);
		}
		choices =new Choices(elements.toList());
		return choices;
	}

	Aggregate  aggregate() {
		Aggregate  aggregate;
		MyListBuffer<Aggregate.ElementAssociation> elements=new MyListBuffer<Aggregate.ElementAssociation>(); 
		Expect(119);
		Aggregate.ElementAssociation element = element_association();
		elements.append(element);
		while (la.kind == 117) {
			Get();
			element = element_association();
			elements.append(element);
		}
		Expect(120);
		aggregate =new Aggregate(elements.toList());
		return aggregate;
	}

	ForGenerateStatement  for_generate_statement(Identifier label) {
		ForGenerateStatement  forGenerateStmt;
		Position pos=toPosition(la);Tuple2<Seq<DeclarativeItem>,Seq<ConcurrentStatement>> body=null;
		Expect(37);
		Identifier loopIdentifier = identifier();
		Expect(45);
		DiscreteRange discreteRange = discrete_range();
		Expect(39);
		body = generate_statement_body();
		Expect(33);
		Expect(39);
		if (la.kind == 1 || la.kind == 2) {
			unused_identifier();
		}
		Expect(116);
		forGenerateStmt=new ForGenerateStatement(pos,toOption(label),loopIdentifier,discreteRange,body._1,body._2);
		return forGenerateStmt;
	}

	IfGenerateStatement  if_generate_statement(Identifier label) {
		IfGenerateStatement  ifGenerateStmt;
		Position pos=toPosition(la);Tuple2<Seq<DeclarativeItem>,Seq<ConcurrentStatement>> body=null;
		Expect(43);
		Expression expr = condition();
		Expect(39);
		body = generate_statement_body();
		Expect(33);
		Expect(39);
		if (la.kind == 1 || la.kind == 2) {
			unused_identifier();
		}
		Expect(116);
		ifGenerateStmt=new IfGenerateStatement(pos,toOption(label),expr,body._1,body._2);
		return ifGenerateStmt;
	}

	Tuple2<Seq<DeclarativeItem>,Seq<ConcurrentStatement>>  generate_statement_body() {
		Tuple2<Seq<DeclarativeItem>,Seq<ConcurrentStatement>>  statementList;
		MyListBuffer<DeclarativeItem> declarativeItems=new MyListBuffer<DeclarativeItem>();
		if (StartOf(25)) {
			while (StartOf(4)) {
				DeclarativeItem item = block_declarative_item();
				declarativeItems.append(item);
			}
			Expect(20);
		}
		Seq<ConcurrentStatement> concurrentStatements = architecture_statement_list();
		statementList=new Tuple2<Seq<DeclarativeItem>,Seq<ConcurrentStatement>>(declarativeItems.toList(),concurrentStatements);
		
		return statementList;
	}

	SequentialStatement  sequential_statement() {
		SequentialStatement  sequentialStatement;
		sequentialStatement=null;Identifier label=null;
		if (scanner.Peek().kind==_COLON) {
			label = label_colon();
		}
		while (!(StartOf(26))) {SynErr(183); Get();}
		if (la.kind == 102) {
			sequentialStatement = wait_statement(label);
		} else if (la.kind == 18) {
			sequentialStatement = assertion_statement(label);
		} else if (la.kind == 80) {
			sequentialStatement = report_statement(label);
		} else if (la.kind == 43) {
			sequentialStatement = if_statement(label);
		} else if (la.kind == 25) {
			sequentialStatement = case_statement(label);
		} else if (la.kind == 37 || la.kind == 53 || la.kind == 104) {
			sequentialStatement = loop_statement(label);
		} else if (la.kind == 58) {
			sequentialStatement = next_statement(label);
		} else if (la.kind == 35) {
			sequentialStatement = exit_statement(label);
		} else if (la.kind == 81) {
			sequentialStatement = return_statement(label);
		} else if (la.kind == 61) {
			sequentialStatement = null_statement(label);
		} else if (IsAssignmentStatement()) {
			sequentialStatement = signal_or_variable_assignment_statement(label);
		} else if (la.kind == 1 || la.kind == 2 || la.kind == 6) {
			sequentialStatement = procedure_call_statement(label);
		} else SynErr(184);
		return sequentialStatement;
	}

	WaitStatement  wait_statement(Identifier label) {
		WaitStatement  waitStmt;
		Position pos=toPosition(la);Expression untilExpr=null,forExpression=null;Seq<Name> name_list=null;
		Expect(102);
		if (la.kind == 63) {
			Get();
			name_list = name_list();
		}
		if (la.kind == 99) {
			Get();
			untilExpr = condition();
		}
		if (la.kind == 37) {
			Get();
			forExpression = expression();
		}
		Expect(116);
		waitStmt=new WaitStatement(pos,toOption(label),toOption(name_list),toOption(untilExpr),toOption(forExpression));
		return waitStmt;
	}

	AssertStatement  assertion_statement(Identifier label) {
		AssertStatement  assertStmt;
		Position pos=toPosition(la);Expression report_expression=null, severity_expression= null;
		Expect(18);
		Expression expr = condition();
		if (la.kind == 80) {
			Get();
			report_expression = expression();
		}
		if (la.kind == 85) {
			Get();
			severity_expression = expression();
		}
		Expect(116);
		assertStmt=new AssertStatement(pos,toOption(label),expr,toOption(report_expression),toOption(severity_expression));
		return assertStmt;
	}

	ReportStatement  report_statement(Identifier label) {
		ReportStatement  reportStmt;
		Position pos=toPosition(la);Expression severity_expression=null;
		Expect(80);
		Expression report_expression = expression();
		if (la.kind == 85) {
			Get();
			severity_expression = expression();
		}
		Expect(116);
		reportStmt=new ReportStatement(pos,toOption(label),report_expression,toOption(severity_expression));
		return reportStmt;
	}

	IfStatement  if_statement(Identifier label) {
		IfStatement  ifStmt;
		Position pos=toPosition(la); 
		MyListBuffer<IfStatement.IfThenPart> ifList=new MyListBuffer<IfStatement.IfThenPart>(); 
		Seq<SequentialStatement> sequentialStatements = null;
		Seq<SequentialStatement> else_sequential_statement = null;
		
		Expect(43);
		Expression if_condition = condition();
		Expect(93);
		sequentialStatements = sequential_statement_list();
		ifList.append(new IfStatement.IfThenPart(if_condition,sequentialStatements));
		while (la.kind == 32) {
			Get();
			Expression elsif_condition = condition();
			Expect(93);
			sequentialStatements = sequential_statement_list();
			ifList.append(new IfStatement.IfThenPart(elsif_condition,sequentialStatements));
		}
		if (la.kind == 31) {
			Get();
			else_sequential_statement = sequential_statement_list();
		}
		Expect(33);
		Expect(43);
		if (la.kind == 1 || la.kind == 2) {
			unused_identifier();
		}
		Expect(116);
		ifStmt=new IfStatement(pos,toOption(label),ifList.toList(),toOption(else_sequential_statement));
		return ifStmt;
	}

	CaseStatement  case_statement(Identifier label) {
		CaseStatement  caseStmt;
		Position pos=toPosition(la);
		MyListBuffer<CaseStatement.When> alternatives=new MyListBuffer<CaseStatement.When>(); 
		
		Expect(25);
		Expression expr = expression();
		Expect(48);
		Expect(103);
		Choices choices = choices();
		Expect(111);
		Seq<SequentialStatement> sequentialStatements = sequential_statement_list();
		alternatives.append(new CaseStatement.When(choices,sequentialStatements));
		while (la.kind == 103) {
			Get();
			choices = choices();
			Expect(111);
			sequentialStatements = sequential_statement_list();
			alternatives.append(new CaseStatement.When(choices,sequentialStatements));
		}
		Expect(33);
		Expect(25);
		if (la.kind == 1 || la.kind == 2) {
			unused_identifier();
		}
		Expect(116);
		caseStmt=new CaseStatement(pos,toOption(label),expr,alternatives.toList());
		return caseStmt;
	}

	SequentialStatement  loop_statement(Identifier label) {
		SequentialStatement  loopStmt;
		Position pos=toPosition(la);Either<Expression,Tuple2<Identifier,DiscreteRange>> stmtType=null;
		if (la.kind == 37 || la.kind == 104) {
			stmtType = iteration_scheme();
		}
		Expect(53);
		Seq<SequentialStatement> sequentialStatements = sequential_statement_list();
		Expect(33);
		Expect(53);
		if (la.kind == 1 || la.kind == 2) {
			unused_identifier();
		}
		Expect(116);
		if (stmtType!=null){
		if (stmtType instanceof Left) loopStmt=new WhileStatement(pos,toOption(label),((Left<Expression,Tuple2<Identifier,DiscreteRange>>)stmtType).a,sequentialStatements);
		else {
			Tuple2<Identifier,DiscreteRange> r=((Right<Expression,Tuple2<Identifier,DiscreteRange>>)stmtType).b;
			loopStmt=new ForStatement(pos,toOption(label),r._1,r._2,sequentialStatements);
		}
		}else loopStmt=new LoopStatement(pos,toOption(label),sequentialStatements);
		
		return loopStmt;
	}

	NextStatement  next_statement(Identifier label) {
		NextStatement  nextStmt;
		Position pos=toPosition(la);Identifier identifier=null;Expression expr=null;
		Expect(58);
		if (la.kind == 1 || la.kind == 2) {
			identifier = identifier();
		}
		if (la.kind == 103) {
			Get();
			expr = condition();
		}
		Expect(116);
		nextStmt=new NextStatement(pos,toOption(label),toOption(identifier),toOption(expr));
		return nextStmt;
	}

	ExitStatement  exit_statement(Identifier label) {
		ExitStatement  exitStmt;
		Position pos=toPosition(la);Expression expr=null;Identifier identifier=null;
		Expect(35);
		if (la.kind == 1 || la.kind == 2) {
			identifier = identifier();
		}
		if (la.kind == 103) {
			Get();
			expr = condition();
		}
		Expect(116);
		exitStmt=new ExitStatement(pos,toOption(label),toOption(identifier),toOption(expr));
		return exitStmt;
	}

	ReturnStatement  return_statement(Identifier label) {
		ReturnStatement  returnStmt;
		Position pos=toPosition(la);Expression expr=null;
		Expect(81);
		if (StartOf(24)) {
			expr = expression();
		}
		Expect(116);
		returnStmt=new ReturnStatement(pos,toOption(label),toOption(expr));
		return returnStmt;
	}

	NullStatement  null_statement(Identifier label) {
		NullStatement  nullStmt;
		Position pos=toPosition(la);
		Expect(61);
		Expect(116);
		nullStmt=new NullStatement(pos,toOption(label));
		return nullStmt;
	}

	SequentialStatement  signal_or_variable_assignment_statement(Identifier label) {
		SequentialStatement  statement;
		statement=null;
		Target target = target();
		if (la.kind == 109) {
			statement = signal_assignment_statement(label,target);
		} else if (la.kind == 113) {
			statement = variable_assignment_statement(label,target);
		} else SynErr(185);
		return statement;
	}

	ProcedureCallStatement  procedure_call_statement(Identifier label) {
		ProcedureCallStatement  procedureCallStmt;
		AssociationList paramterList=null;
		SelectedName procedure_name = selected_name();
		if (la.kind == 119) {
			Get();
			paramterList = association_list();
			Expect(120);
		}
		Expect(116);
		procedureCallStmt=new ProcedureCallStatement(toOption(label),procedure_name,toOption(paramterList));
		return procedureCallStmt;
	}

	SignalAssignmentStatement  signal_assignment_statement(Identifier label,Target target) {
		SignalAssignmentStatement  signalAssignStmt;
		DelayMechanism delay=null;
		Expect(109);
		Position pos=toPosition(t);
		if (la.kind == 46 || la.kind == 78 || la.kind == 95) {
			delay = delay_mechanism();
		}
		Waveform waveform = waveform();
		Expect(116);
		signalAssignStmt=new SimpleSignalAssignmentStatement(pos,toOption(label),target,toOption(delay),waveform);
		return signalAssignStmt;
	}

	VariableAssignmentStatement  variable_assignment_statement(Identifier label,Target target) {
		VariableAssignmentStatement  varAssignStmt;
		Expect(113);
		Position pos=toPosition(t);
		Expression expr = expression();
		Expect(116);
		varAssignStmt=new SimpleVariableAssignmentStatement(pos,toOption(label),target,expr);
		return varAssignStmt;
	}

	Waveform.Element  waveform_element() {
		Waveform.Element  element;
		Expression time_expression=null;
		Expression value_expression = expression();
		if (la.kind == 12) {
			Get();
			time_expression = expression();
		}
		element= new Waveform.Element(value_expression,toOption(time_expression));
		return element;
	}

	Either<Expression,Tuple2<Identifier,DiscreteRange>>  iteration_scheme() {
		Either<Expression,Tuple2<Identifier,DiscreteRange>>  scheme;
		scheme=null;
		if (la.kind == 104) {
			Get();
			Expression expr = condition();
			scheme=new Left<Expression,Tuple2<Identifier,DiscreteRange>>(expr);
		} else if (la.kind == 37) {
			Get();
			Identifier identifier = identifier();
			Expect(45);
			DiscreteRange discreteRange = discrete_range();
			scheme=new Right<Expression,Tuple2<Identifier,DiscreteRange>>(new Tuple2<Identifier,DiscreteRange>(identifier,discreteRange));
		} else SynErr(186);
		return scheme;
	}

	InterfaceList.AbstractInterfaceElement  interface_element_procedure() {
		InterfaceList.AbstractInterfaceElement  element;
		element=null;
		if (IsInterfaceConstantDeclaration()) {
			element = interface_constant_declaration();
		} else if (IsInterfaceVariableDeclaration()) {
			element = interface_variable_declaration();
		} else if (la.kind == 1 || la.kind == 2 || la.kind == 87) {
			element = interface_signal_declaration_procedure();
		} else if (la.kind == 36) {
			element = interface_file_declaration();
		} else SynErr(187);
		return element;
	}

	InterfaceList.InterfaceVariableDeclaration  interface_variable_declaration() {
		InterfaceList.InterfaceVariableDeclaration  varElement;
		Expression expr=null;InterfaceMode mode=null;
		if (la.kind == 101) {
			Get();
		}
		Seq<Identifier> list = identifier_list();
		Expect(123);
		if (StartOf(7)) {
			mode = interface_mode();
		}
		SubTypeIndication subType = subtype_indication();
		if (la.kind == 113) {
			Get();
			expr = expression();
		}
		varElement=new InterfaceList.InterfaceVariableDeclaration(list,toOption(mode),subType,toOption(expr));
		return varElement;
	}

	InterfaceList.InterfaceFileDeclaration  interface_file_declaration() {
		InterfaceList.InterfaceFileDeclaration  fileElement;
		Expect(36);
		Seq<Identifier> list = identifier_list();
		Expect(123);
		SubTypeIndication subType = subtype_indication();
		fileElement=new InterfaceList.InterfaceFileDeclaration(list,subType);
		return fileElement;
	}

	InterfaceList.AbstractInterfaceElement  interface_element_function() {
		InterfaceList.AbstractInterfaceElement  element;
		element=null;
		if (la.kind == 1 || la.kind == 2 || la.kind == 28) {
			element = interface_constant_declaration();
		} else if (la.kind == 87) {
			element = interface_signal_declaration_function();
		} else if (la.kind == 36) {
			element = interface_file_declaration();
		} else SynErr(188);
		return element;
	}

	InterfaceList.InterfaceSignalDeclaration  interface_signal_declaration_function() {
		InterfaceList.InterfaceSignalDeclaration  signalElement;
		Expression expr=null;boolean bus=false;
		Expect(87);
		Seq<Identifier> list = identifier_list();
		Expect(123);
		if (la.kind == 45) {
			Get();
		}
		SubTypeIndication subType = subtype_indication();
		if (la.kind == 24) {
			Get();
			bus=true;
		}
		if (la.kind == 113) {
			Get();
			expr = expression();
		}
		signalElement=new InterfaceList.InterfaceSignalDeclaration(list,toOption(InterfaceMode.IN),subType,bus,toOption(expr));
		return signalElement;
	}

	InterfaceMode  interface_mode() {
		InterfaceMode  mode;
		mode=null;
		if (la.kind == 45) {
			Get();
			mode=InterfaceMode.IN;
		} else if (la.kind == 67) {
			Get();
			mode=InterfaceMode.OUT;
		} else if (la.kind == 47) {
			Get();
			mode=InterfaceMode.INOUT;
		} else if (la.kind == 23) {
			Get();
			mode=InterfaceMode.BUFFER;
		} else if (la.kind == 51) {
			Get();
			mode=InterfaceMode.LINKAGE;
		} else SynErr(189);
		return mode;
	}

	AssociationList.Element  association_element() {
		AssociationList.Element  element;
		Identifier identifier=null;Name name=null;Option<Expression> actualPart=null;
		if (la.kind == 1 || la.kind == 2 || la.kind == 6) {
			name = formal_part();
			Expect(111);
		}
		actualPart = actual_part();
		element=new AssociationList.Element(toOption(name),actualPart);
		return element;
	}

	Name  formal_part() {
		Name  formal_part;
		formal_part = name();
		return formal_part;
	}

	Option<Expression>  actual_part() {
		Option<Expression>  actual_part;
		Expression expr=null;
		if (StartOf(24)) {
			expr = expression();
		} else if (la.kind == 64) {
			Get();
		} else SynErr(190);
		actual_part = toOption(expr);
		return actual_part;
	}

	Expression  relation() {
		Expression  rel;
		Tuple2<Position,Operators.Relation> op=null;
		rel = shift_expression();
		if (StartOf(27)) {
			op = relational_operator();
			Expression right = shift_expression();
			rel=new Relation(op._1,rel,op._2,right);
		}
		return rel;
	}

	Tuple2<Position,Operators.Logical>  logical_operator() {
		Tuple2<Position,Operators.Logical>  op;
		Position pos=toPosition(la);
		Operators.Logical logOp=null;
		
		switch (la.kind) {
		case 15: {
			Get();
			logOp=Operators.Logical.AND;
			break;
		}
		case 65: {
			Get();
			logOp=Operators.Logical.OR;
			break;
		}
		case 107: {
			Get();
			logOp=Operators.Logical.XOR;
			break;
		}
		case 106: {
			Get();
			logOp=Operators.Logical.XNOR;
			break;
		}
		case 56: {
			Get();
			logOp=Operators.Logical.NAND;
			break;
		}
		case 59: {
			Get();
			logOp=Operators.Logical.NOR;
			break;
		}
		default: SynErr(191); break;
		}
		op=new Tuple2<Position,Operators.Logical>(pos,logOp);
		return op;
	}

	Expression  shift_expression() {
		Expression  shiftExpr;
		Tuple2<Position,Operators.Shift> op=null;
		shiftExpr = simple_expression();
		if (StartOf(28)) {
			op = shift_operator();
			Expression right = simple_expression();
			shiftExpr=new ShiftExpression(op._1,shiftExpr,op._2,right);
		}
		return shiftExpr;
	}

	Tuple2<Position,Operators.Relation>  relational_operator() {
		Tuple2<Position,Operators.Relation>  op;
		Position pos=toPosition(la);
		Operators.Relation relOp=null;
		
		switch (la.kind) {
		case 130: {
			Get();
			relOp=Operators.Relation.EQ;
			break;
		}
		case 112: {
			Get();
			relOp=Operators.Relation.NEQ;
			break;
		}
		case 128: {
			Get();
			relOp=Operators.Relation.LT;
			break;
		}
		case 109: {
			Get();
			relOp=Operators.Relation.LEQ;
			break;
		}
		case 129: {
			Get();
			relOp=Operators.Relation.GT;
			break;
		}
		case 110: {
			Get();
			relOp=Operators.Relation.GEQ;
			break;
		}
		default: SynErr(192); break;
		}
		op=new Tuple2<Position,Operators.Relation>(pos,relOp);
		return op;
	}

	Tuple2<Position,Operators.Shift>  shift_operator() {
		Tuple2<Position,Operators.Shift>  op;
		Position pos=toPosition(la);
		Operators.Shift shiftOp=null;
		
		switch (la.kind) {
		case 89: {
			Get();
			shiftOp=Operators.Shift.SLL;
			break;
		}
		case 91: {
			Get();
			shiftOp=Operators.Shift.SRL;
			break;
		}
		case 88: {
			Get();
			shiftOp=Operators.Shift.SLA;
			break;
		}
		case 90: {
			Get();
			shiftOp=Operators.Shift.SRA;
			break;
		}
		case 82: {
			Get();
			shiftOp=Operators.Shift.ROL;
			break;
		}
		case 83: {
			Get();
			shiftOp=Operators.Shift.ROR;
			break;
		}
		default: SynErr(193); break;
		}
		op=new Tuple2<Position,Operators.Shift>(pos,shiftOp);
		return op;
	}

	Tuple2<Position,Operators.Sign>  sign() {
		Tuple2<Position,Operators.Sign>  op;
		Position pos=toPosition(la);
		Operators.Sign signOp=null;
		
		if (la.kind == 126) {
			Get();
			signOp=Operators.Sign.PLUS;
		} else if (la.kind == 127) {
			Get();
			signOp=Operators.Sign.MINUS;
		} else SynErr(194);
		op=new Tuple2<Position,Operators.Sign>(pos,signOp);
		return op;
	}

	Expression  term() {
		Expression  term;
		Tuple2<Position,Operators.Term> op=null;
		term = factor();
		if (StartOf(29)) {
			op = multiplying_operator();
			Expression right = factor();
			term = new Term(op._1,term,op._2,right);
		}
		return term;
	}

	Tuple2<Position,Operators.Add>  adding_operator() {
		Tuple2<Position,Operators.Add>  op;
		Position pos=toPosition(la);
		Operators.Add addOp=null;
		
		if (la.kind == 126) {
			Get();
			addOp=Operators.Add.PLUS;
		} else if (la.kind == 127) {
			Get();
			addOp=Operators.Add.MINUS;
		} else if (la.kind == 118) {
			Get();
			addOp=Operators.Add.AMPERSAND;
		} else SynErr(195);
		op=new Tuple2<Position,Operators.Add>(pos,addOp);
		return op;
	}

	Tuple2<Position,Operators.Term>  multiplying_operator() {
		Tuple2<Position,Operators.Term>  op;
		Position pos=toPosition(la);
		Operators.Term mulOp=null;
		
		if (la.kind == 124) {
			Get();
			mulOp=Operators.Term.MUL;
		} else if (la.kind == 125) {
			Get();
			mulOp=Operators.Term.DIV;
		} else if (la.kind == 55) {
			Get();
			mulOp=Operators.Term.MOD;
		} else if (la.kind == 79) {
			Get();
			mulOp=Operators.Term.REM;
		} else SynErr(196);
		op=new Tuple2<Position,Operators.Term>(pos,mulOp);
		return op;
	}

	Expression  factor() {
		Expression  factor;
		factor=null;
		if (StartOf(30)) {
			factor = primary();
			if (la.kind == 108) {
				Get();
				Position pos=toPosition(t);
				Expression right = primary();
				factor = new Factor(pos,factor,Operators.Factor.POW,toOption(right));
			}
		} else if (la.kind == 10) {
			Get();
			Position pos=toPosition(t);
			Expression left = primary();
			factor = new Factor(pos,left,Operators.Factor.ABS);
		} else if (la.kind == 60) {
			Get();
			Position pos=toPosition(t);
			Expression left = primary();
			factor = new Factor(pos,left,Operators.Factor.NOT);
		} else SynErr(197);
		return factor;
	}

	Expression  primary() {
		Expression  expr;
		expr=null;
		if (la.kind == 1 || la.kind == 2 || la.kind == 6) {
			Name name = name();
			expr=new NameExpression(name);
		} else if (StartOf(31)) {
			expr = literal();
		} else if (la.kind == 57) {
			expr = allocator();
		} else if (la.kind == 119) {
			Aggregate aggregate = aggregate();
			expr=new AggregateExpression(aggregate);
		} else SynErr(198);
		return expr;
	}

	Expression  literal() {
		Expression  literal;
		LiteralType literalType=null;
		switch (la.kind) {
		case 5: {
			Get();
			literalType=LiteralType.REAL_LITERAL;
			break;
		}
		case 4: {
			Get();
			literalType=LiteralType.INTEGER_LITERAL;
			break;
		}
		case 3: {
			Get();
			literalType=LiteralType.BASED_LITERAL;
			break;
		}
		case 8: {
			Get();
			literalType=LiteralType.CHARACTER_LITERAL;
			break;
		}
		case 7: {
			Get();
			literalType=LiteralType.BIT_STRING_LITERAL;
			break;
		}
		case 61: {
			Get();
			literalType=LiteralType.NULL_LITERAL;
			break;
		}
		default: SynErr(199); break;
		}
		literal =new Literal(toPosition(t),t.val,literalType);
		if ((t.kind==_INTEGER_LITERAL || t.kind==_REAL_LITERAL) && (la.kind==_BASIC_IDENTIFIER || la.kind==_EXTENDED_IDENTIFIER)) {
			Identifier unit = identifier();
			literal = new PhysicalLiteral((Literal)literal,unit);
		}
		return literal;
	}

	Expression  allocator() {
		Expression  newExpression;
		Position pos=toPosition(la);newExpression=null;
		Expect(57);
		SelectedName selectedName = selected_name();
		if (la.kind == 9) {
			Expression expr = qualified_expression(selectedName);
			newExpression=new NewExpression(pos,new Left<Expression, SubTypeIndication>(expr));
		} else if (StartOf(32)) {
			if (la.kind == 119) {
				Seq<DiscreteRange> ranges = index_constraint();
				Either<Range,Seq<DiscreteRange>> constraint=new Right<Range,Seq<DiscreteRange>>(ranges);
				newExpression=new NewExpression(pos,new Right<Expression, SubTypeIndication>(new SubTypeIndication(selectedName,toOption(constraint))));
				
			}
		} else SynErr(200);
		return newExpression;
	}

	QualifiedExpression  qualified_expression(SelectedName typeName) {
		QualifiedExpression  expr;
		Expect(9);
		Aggregate aggregate = aggregate();
		expr=new QualifiedExpression(typeName,new AggregateExpression(aggregate));
		return expr;
	}

	FunctionCallExpression  function_call() {
		FunctionCallExpression  functionCall;
		AssociationList parameter_association_list=null;
		SelectedName function_name = selected_name();
		if (la.kind == 119) {
			Get();
			parameter_association_list = association_list();
			Expect(120);
		}
		functionCall=new FunctionCallExpression(function_name,toOption(parameter_association_list));
		return functionCall;
	}

	Identifier  name_prefix() {
		Identifier  identifier;
		identifier=null;
		if (la.kind == 1 || la.kind == 2) {
			identifier = identifier();
		} else if (la.kind == 6) {
			Get();
			identifier=toIdentifier(t);
		} else SynErr(201);
		return identifier;
	}

	Name.SelectedPart  name_selected_part() {
		Name.SelectedPart  part;
		part=null;
		Expect(132);
		if (la.kind == 1 || la.kind == 2) {
			Identifier identifier = identifier();
			part= new Name.SelectedPart(identifier);
		} else if (la.kind == 8) {
			Get();
			part= new Name.SelectedPart(toIdentifier(t));
		} else if (la.kind == 6) {
			Get();
			part= new Name.SelectedPart(toIdentifier(t));
		} else if (la.kind == 14) {
			Get();
			part= new Name.SelectedPart(toIdentifier(t));
		} else SynErr(202);
		return part;
	}

	Name.Part  name_part() {
		Name.Part  part;
		part=null;
		if (la.kind == 132) {
			part = name_selected_part();
		} else if (la.kind == 9 || la.kind == 121) {
			part = name_attribute_part();
		} else if (la.kind == 119) {
			part = name_slice_part();
		} else SynErr(203);
		return part;
	}

	Name.AttributePart  name_attribute_part() {
		Name.AttributePart  part;
		Signature signature=null;Identifier identifier=null;Expression expr=null;
		if (la.kind == 121) {
			signature = signature();
		}
		Expect(9);
		if (la.kind == 1 || la.kind == 2) {
			identifier = identifier();
		} else if (la.kind == 75) {
			Get();
			identifier=toIdentifier(t);
		} else SynErr(204);
		if (la.kind == 119) {
			Get();
			expr = expression();
			Expect(120);
		}
		part=new Name.AttributePart(toOption(signature),identifier,toOption(expr));
		return part;
	}

	Name.SlicePart  name_slice_part() {
		Name.SlicePart  part;
		Expect(119);
		DiscreteRange discreteRange = discrete_range();
		Expect(120);
		part=new Name.SlicePart(discreteRange);
		return part;
	}

	Name.IndexPart  name_indexed_part() {
		Name.IndexPart  part;
		MyListBuffer<Expression> indexes=new MyListBuffer<Expression>(); 
		Expect(119);
		Expression expr = expression();
		indexes.append(expr);
		while (la.kind == 117) {
			Get();
			expr = expression();
			indexes.append(expr);
		}
		Expect(120);
		part=new Name.IndexPart(indexes.toList());
		return part;
	}

	Aggregate.ElementAssociation  element_association() {
		Aggregate.ElementAssociation  element;
		Choices choice=null;
		Expression expr = expression();
		element=new Aggregate.ElementAssociation(toOption(choice),expr);
		return element;
	}

	Choices.Choice  choice() {
		Choices.Choice  choice;
		Expect(133);
		choice=null;
		return choice;
	}



	public void Parse() {
		la = new Token();
		la.val = "";		
		Get();
		VHDL();

		Expect(0);
	}

	private static final boolean[][] set = {
		{T,T,T,x, x,x,T,x, x,x,x,x, x,T,x,x, x,x,T,T, x,x,x,x, x,T,T,x, T,T,x,x, x,x,x,T, T,T,T,x, x,T,x,T, T,x,x,x, x,x,x,x, x,T,x,x, x,x,T,x, x,T,x,x, x,x,x,x, x,x,x,T, x,x,T,x, x,x,x,x, T,T,x,x, x,x,T,T, x,x,x,x, T,x,x,x, T,x,x,x, T,T,T,x, T,x,x,x, x,x,x,x, x,x,x,x, x,x,x,T, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x},
		{x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, T,x,x,x, x,x,x,x, x,x,x,T, x,x,x,x, x,x,T,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,T,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, T,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, T,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x},
		{x,x,x,x, x,x,x,x, x,x,x,x, x,T,x,x, x,x,x,T, x,x,x,x, x,x,x,x, T,T,x,x, x,x,x,x, T,x,T,x, x,T,x,x, T,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,T, x,x,T,x, x,x,x,x, x,x,x,x, x,x,T,T, x,x,x,x, T,x,x,x, T,x,x,x, T,T,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x},
		{x,T,T,x, x,x,T,x, x,x,x,x, x,x,x,x, x,x,T,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,T,x, T,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x},
		{x,x,x,x, x,x,x,x, x,x,x,x, x,T,x,x, x,x,x,T, x,x,x,x, x,x,T,x, T,T,x,x, x,x,x,x, T,T,T,x, x,T,x,x, T,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,T, x,x,T,x, x,x,x,x, x,x,x,x, x,x,T,T, x,x,x,x, T,x,x,x, T,x,x,x, T,T,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x},
		{x,x,x,x, x,x,x,x, x,x,x,x, x,T,x,x, x,x,x,T, x,x,x,x, x,x,x,x, T,x,x,x, x,x,x,x, T,x,T,x, x,T,x,x, T,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,T, x,x,T,x, x,x,x,x, x,x,x,x, x,x,T,T, x,x,x,x, T,x,x,x, T,x,x,x, T,T,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x},
		{x,x,x,x, x,x,x,x, x,x,x,x, x,T,x,x, x,x,x,T, x,x,x,x, x,x,T,x, T,T,x,x, x,x,x,x, T,x,T,x, x,T,x,x, T,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,T, x,x,T,x, x,x,x,x, x,x,x,x, x,x,T,T, x,x,x,x, T,x,x,x, T,x,x,x, T,T,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x},
		{x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,T, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,T,x,T, x,x,x,T, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,T, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x},
		{T,x,x,x, x,x,x,x, x,x,x,x, x,T,x,x, x,x,x,T, x,x,x,x, x,x,x,x, T,T,x,x, x,x,x,x, T,x,T,x, x,T,x,x, T,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,T, x,x,T,x, x,x,x,x, x,x,x,x, x,x,T,T, x,x,x,x, T,x,x,x, T,x,x,x, T,T,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x},
		{x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,T,x, x,x,x,x, T,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,T, x,x,T,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x},
		{x,x,x,x, x,x,x,x, x,x,x,x, x,T,x,x, x,x,x,T, x,x,x,x, x,x,x,x, T,x,x,x, x,x,x,x, T,x,T,x, x,T,x,x, T,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,T, x,x,T,x, x,x,x,x, x,x,x,x, x,x,T,x, x,x,x,x, T,x,x,x, T,x,x,x, T,T,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x},
		{T,x,x,x, x,x,x,x, x,x,x,x, x,T,x,x, x,x,x,T, x,x,x,x, x,x,T,x, T,T,x,x, x,x,x,x, T,T,T,x, x,T,x,x, T,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,T, x,x,T,x, x,x,x,x, x,x,x,x, x,x,T,T, x,x,x,x, T,x,x,x, T,x,x,x, T,T,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x},
		{x,T,T,x, x,x,T,x, x,x,x,x, x,x,x,x, x,x,T,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,T,x, T,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,T,x,x, x,x,x,x, x,x,x,x, x,x,x,T, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x},
		{T,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,T, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,T,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, T,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x},
		{x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,T, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, T,x,x,T, x,x,x,x, x,T,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,T,T, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x},
		{x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, T,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,T,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, T,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, T,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x},
		{T,x,x,x, x,x,x,x, x,x,x,x, x,T,x,x, x,x,x,T, x,x,x,x, x,x,T,x, T,T,x,x, x,x,x,x, T,x,T,x, x,T,x,x, T,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,T, x,x,T,x, x,x,x,x, x,x,x,x, x,x,T,T, x,x,x,x, T,x,x,x, T,x,x,x, T,T,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x},
		{T,x,x,x, x,x,x,x, x,x,x,x, x,T,x,x, x,x,x,T, x,x,x,x, x,x,x,x, T,x,x,x, x,x,x,x, T,x,T,x, x,T,x,x, T,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,T, x,x,T,x, x,x,x,x, x,x,x,x, x,x,T,T, x,x,x,x, T,x,x,x, T,x,x,x, T,T,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x},
		{T,x,x,x, x,x,x,x, x,x,x,x, x,T,x,x, x,x,x,T, x,x,x,x, x,x,x,x, T,x,x,x, x,x,x,x, T,x,T,x, x,T,x,x, T,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,T, x,x,T,x, x,x,x,x, x,x,x,x, x,x,T,x, x,x,x,x, T,x,x,x, T,x,x,x, T,T,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x},
		{x,T,T,x, x,x,T,x, x,x,x,x, x,x,x,x, x,x,T,x, x,x,x,x, x,T,x,x, x,x,x,x, x,x,x,T, x,T,x,x, x,x,x,T, x,x,x,x, x,x,x,x, x,T,x,x, x,x,T,x, x,T,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, T,T,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,T,x, T,x,x,x, x,x,x,x, x,x,x,x, x,x,x,T, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x},
		{x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,T, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,T,x, x,x,x,x, T,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,T, x,x,T,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, T,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x},
		{x,T,T,x, x,x,T,x, T,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x},
		{T,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,T, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,T,x, x,x,x,x, T,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,T, x,x,T,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, T,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x},
		{x,T,T,x, x,x,T,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,T, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x},
		{x,T,T,T, T,T,T,T, T,x,T,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,T,x,x, T,T,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,T, x,x,x,x, x,x,T,T, x,x,x,x, x,x,x,x},
		{x,x,x,x, x,x,x,x, x,x,x,x, x,T,x,x, x,x,x,T, T,x,x,x, x,x,T,x, T,T,x,x, x,x,x,x, T,T,T,x, x,T,x,x, T,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,T, x,x,T,x, x,x,x,x, x,x,x,x, x,x,T,T, x,x,x,x, T,x,x,x, T,x,x,x, T,T,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x},
		{T,T,T,x, x,x,T,x, x,x,x,x, x,x,x,x, x,x,T,x, x,x,x,x, x,T,x,x, x,x,x,x, x,x,x,T, x,T,x,x, x,x,x,T, x,x,x,x, x,x,x,x, x,T,x,x, x,x,T,x, x,T,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, T,T,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,T,x, T,x,x,x, x,x,x,x, x,x,x,x, x,x,x,T, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x},
		{x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,T,T,x, T,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, T,T,T,x, x,x,x,x},
		{x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,T,T, x,x,x,x, T,T,T,T, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x},
		{x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,T, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,T, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, T,T,x,x, x,x,x,x, x,x,x,x},
		{x,T,T,T, T,T,T,T, T,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,T,x,x, x,T,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,T, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x},
		{x,x,x,T, T,T,x,T, T,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,T,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x},
		{x,x,x,x, x,x,x,x, x,x,x,x, T,x,x,T, x,x,x,x, x,x,x,x, T,x,x,x, x,x,T,T, x,x,x,x, x,T,x,T, x,x,x,x, x,x,T,x, T,x,x,x, x,T,x,T, T,x,x,T, x,x,x,x, T,T,x,x, x,x,x,x, x,x,x,x, x,T,x,T, T,x,T,T, T,T,x,x, T,T,T,T, x,T,T,x, x,x,T,x, x,x,x,T, x,x,T,T, T,T,T,x, T,T,x,x, T,T,T,T, T,x,x,x, T,T,T,T, T,T,T,x, x,x,x,x}

	};
} // end Parser


class Errors {
	public int count = 0;                                    // number of errors detected
	public java.io.PrintStream errorStream = System.out;     // error messages go to this stream
	public String errMsgFormat = "-- line {0} col {1}: {2}"; // 0=line, 1=column, 2=text
	
	protected void printMsg(int line, int column, String msg) {
		StringBuffer b = new StringBuffer(errMsgFormat);
		int pos = b.indexOf("{0}");
		if (pos >= 0) { b.delete(pos, pos+3); b.insert(pos, line); }
		pos = b.indexOf("{1}");
		if (pos >= 0) { b.delete(pos, pos+3); b.insert(pos, column); }
		pos = b.indexOf("{2}");
		if (pos >= 0) b.replace(pos, pos+3, msg);
		errorStream.println(b.toString());
	}
	
	public void SynErr (int line, int col, int n) {
		String s;
		switch (n) {
			case 0: s = "EOF expected"; break;
			case 1: s = "BASIC_IDENTIFIER expected"; break;
			case 2: s = "EXTENDED_IDENTIFIER expected"; break;
			case 3: s = "BASED_LITERAL expected"; break;
			case 4: s = "INTEGER_LITERAL expected"; break;
			case 5: s = "REAL_LITERAL expected"; break;
			case 6: s = "STRING_LITERAL expected"; break;
			case 7: s = "BIT_STRING_LITERAL expected"; break;
			case 8: s = "CHARACTER_LITERAL expected"; break;
			case 9: s = "APOSTROPHE expected"; break;
			case 10: s = "ABS expected"; break;
			case 11: s = "ACCESS expected"; break;
			case 12: s = "AFTER expected"; break;
			case 13: s = "ALIAS expected"; break;
			case 14: s = "ALL expected"; break;
			case 15: s = "AND expected"; break;
			case 16: s = "ARCHITECTURE expected"; break;
			case 17: s = "ARRAY expected"; break;
			case 18: s = "ASSERT expected"; break;
			case 19: s = "ATTRIBUTE expected"; break;
			case 20: s = "BEGIN expected"; break;
			case 21: s = "BLOCK expected"; break;
			case 22: s = "BODY expected"; break;
			case 23: s = "BUFFER expected"; break;
			case 24: s = "BUS expected"; break;
			case 25: s = "CASE expected"; break;
			case 26: s = "COMPONENT expected"; break;
			case 27: s = "CONFIGURATION expected"; break;
			case 28: s = "CONSTANT expected"; break;
			case 29: s = "DISCONNECT expected"; break;
			case 30: s = "DOWNTO expected"; break;
			case 31: s = "ELSE expected"; break;
			case 32: s = "ELSIF expected"; break;
			case 33: s = "END_TOKEN expected"; break;
			case 34: s = "ENTITY expected"; break;
			case 35: s = "EXIT expected"; break;
			case 36: s = "FILE expected"; break;
			case 37: s = "FOR expected"; break;
			case 38: s = "FUNCTION expected"; break;
			case 39: s = "GENERATE expected"; break;
			case 40: s = "GENERIC expected"; break;
			case 41: s = "GROUP expected"; break;
			case 42: s = "GUARDED expected"; break;
			case 43: s = "IF_TOKEN expected"; break;
			case 44: s = "IMPURE expected"; break;
			case 45: s = "IN expected"; break;
			case 46: s = "INERTIAL expected"; break;
			case 47: s = "INOUT expected"; break;
			case 48: s = "IS expected"; break;
			case 49: s = "LABEL expected"; break;
			case 50: s = "LIBRARY expected"; break;
			case 51: s = "LINKAGE expected"; break;
			case 52: s = "LITERAL expected"; break;
			case 53: s = "LOOP expected"; break;
			case 54: s = "MAP expected"; break;
			case 55: s = "MOD expected"; break;
			case 56: s = "NAND expected"; break;
			case 57: s = "NEW expected"; break;
			case 58: s = "NEXT expected"; break;
			case 59: s = "NOR expected"; break;
			case 60: s = "NOT expected"; break;
			case 61: s = "NULL expected"; break;
			case 62: s = "OF expected"; break;
			case 63: s = "ON expected"; break;
			case 64: s = "OPEN expected"; break;
			case 65: s = "OR expected"; break;
			case 66: s = "OTHERS expected"; break;
			case 67: s = "OUT expected"; break;
			case 68: s = "PACKAGE expected"; break;
			case 69: s = "PORT expected"; break;
			case 70: s = "POSTPONED expected"; break;
			case 71: s = "PROCEDURE expected"; break;
			case 72: s = "PROCESS expected"; break;
			case 73: s = "PROTECTED expected"; break;
			case 74: s = "PURE expected"; break;
			case 75: s = "RANGE expected"; break;
			case 76: s = "RECORD expected"; break;
			case 77: s = "REGISTER expected"; break;
			case 78: s = "REJECT expected"; break;
			case 79: s = "REM expected"; break;
			case 80: s = "REPORT expected"; break;
			case 81: s = "RETURN expected"; break;
			case 82: s = "ROL expected"; break;
			case 83: s = "ROR expected"; break;
			case 84: s = "SELECT expected"; break;
			case 85: s = "SEVERITY expected"; break;
			case 86: s = "SHARED expected"; break;
			case 87: s = "SIGNAL expected"; break;
			case 88: s = "SLA expected"; break;
			case 89: s = "SLL expected"; break;
			case 90: s = "SRA expected"; break;
			case 91: s = "SRL expected"; break;
			case 92: s = "SUBTYPE expected"; break;
			case 93: s = "THEN expected"; break;
			case 94: s = "TO_TOKEN expected"; break;
			case 95: s = "TRANSPORT expected"; break;
			case 96: s = "TYPE expected"; break;
			case 97: s = "UNAFFECTED expected"; break;
			case 98: s = "UNITS expected"; break;
			case 99: s = "UNTIL expected"; break;
			case 100: s = "USE expected"; break;
			case 101: s = "VARIABLE expected"; break;
			case 102: s = "WAIT expected"; break;
			case 103: s = "WHEN expected"; break;
			case 104: s = "WHILE expected"; break;
			case 105: s = "WITH expected"; break;
			case 106: s = "XNOR expected"; break;
			case 107: s = "XOR expected"; break;
			case 108: s = "DOUBLESTAR expected"; break;
			case 109: s = "LEQ expected"; break;
			case 110: s = "GEQ expected"; break;
			case 111: s = "ARROW expected"; break;
			case 112: s = "NEQ expected"; break;
			case 113: s = "VAR_ASSIGN expected"; break;
			case 114: s = "BOX expected"; break;
			case 115: s = "DBLQUOTE expected"; break;
			case 116: s = "SEMICOLON expected"; break;
			case 117: s = "COMMA expected"; break;
			case 118: s = "AMPERSAND expected"; break;
			case 119: s = "LPAREN expected"; break;
			case 120: s = "RPAREN expected"; break;
			case 121: s = "LBRACKET expected"; break;
			case 122: s = "RBRACKET expected"; break;
			case 123: s = "COLON expected"; break;
			case 124: s = "MUL expected"; break;
			case 125: s = "DIV expected"; break;
			case 126: s = "PLUS expected"; break;
			case 127: s = "MINUS expected"; break;
			case 128: s = "LT expected"; break;
			case 129: s = "GT expected"; break;
			case 130: s = "EQ expected"; break;
			case 131: s = "BAR expected"; break;
			case 132: s = "DOT expected"; break;
			case 133: s = "\"jlskdjf\u00f6ks\" expected"; break;
			case 134: s = "??? expected"; break;
			case 135: s = "invalid library_unit"; break;
			case 136: s = "invalid entity_declaration"; break;
			case 137: s = "invalid identifier"; break;
			case 138: s = "this symbol not expected in entity_declarative_item"; break;
			case 139: s = "invalid entity_declarative_item"; break;
			case 140: s = "invalid type_declaration"; break;
			case 141: s = "invalid disconnection_specification"; break;
			case 142: s = "this symbol not expected in block_declarative_item"; break;
			case 143: s = "invalid block_declarative_item"; break;
			case 144: s = "this symbol not expected in configuration_declarative_item"; break;
			case 145: s = "invalid configuration_declarative_item"; break;
			case 146: s = "invalid block_specification"; break;
			case 147: s = "this symbol not expected in package_declarative_item"; break;
			case 148: s = "invalid package_declarative_item"; break;
			case 149: s = "this symbol not expected in package_body_declarative_item"; break;
			case 150: s = "invalid package_body_declarative_item"; break;
			case 151: s = "invalid designator"; break;
			case 152: s = "invalid subprogram_specification"; break;
			case 153: s = "this symbol not expected in subprogram_declarative_item"; break;
			case 154: s = "invalid subprogram_declarative_item"; break;
			case 155: s = "invalid type_definition"; break;
			case 156: s = "invalid array_type_definition"; break;
			case 157: s = "invalid alias_designator"; break;
			case 158: s = "invalid entity_name_list"; break;
			case 159: s = "invalid entity_class"; break;
			case 160: s = "invalid entity_designator"; break;
			case 161: s = "invalid instantiation_list"; break;
			case 162: s = "invalid entity_aspect"; break;
			case 163: s = "invalid group_constituent"; break;
			case 164: s = "invalid enumeration_literal"; break;
			case 165: s = "invalid physical_literal"; break;
			case 166: s = "this symbol not expected in protected_type_declarative_item"; break;
			case 167: s = "invalid protected_type_declarative_item"; break;
			case 168: s = "this symbol not expected in protected_type_body_declarative_item"; break;
			case 169: s = "invalid protected_type_body_declarative_item"; break;
			case 170: s = "invalid constraint"; break;
			case 171: s = "invalid direction"; break;
			case 172: s = "invalid architecture_statement"; break;
			case 173: s = "invalid architecture_statement_with_label"; break;
			case 174: s = "invalid architecture_statement_optional_label"; break;
			case 175: s = "invalid component_instantiation_statement"; break;
			case 176: s = "invalid generate_statement"; break;
			case 177: s = "invalid concurrent_signal_assignment_statement"; break;
			case 178: s = "this symbol not expected in process_declarative_item"; break;
			case 179: s = "invalid process_declarative_item"; break;
			case 180: s = "invalid target"; break;
			case 181: s = "invalid delay_mechanism"; break;
			case 182: s = "invalid waveform"; break;
			case 183: s = "this symbol not expected in sequential_statement"; break;
			case 184: s = "invalid sequential_statement"; break;
			case 185: s = "invalid signal_or_variable_assignment_statement"; break;
			case 186: s = "invalid iteration_scheme"; break;
			case 187: s = "invalid interface_element_procedure"; break;
			case 188: s = "invalid interface_element_function"; break;
			case 189: s = "invalid interface_mode"; break;
			case 190: s = "invalid actual_part"; break;
			case 191: s = "invalid logical_operator"; break;
			case 192: s = "invalid relational_operator"; break;
			case 193: s = "invalid shift_operator"; break;
			case 194: s = "invalid sign"; break;
			case 195: s = "invalid adding_operator"; break;
			case 196: s = "invalid multiplying_operator"; break;
			case 197: s = "invalid factor"; break;
			case 198: s = "invalid primary"; break;
			case 199: s = "invalid literal"; break;
			case 200: s = "invalid allocator"; break;
			case 201: s = "invalid name_prefix"; break;
			case 202: s = "invalid name_selected_part"; break;
			case 203: s = "invalid name_part"; break;
			case 204: s = "invalid name_attribute_part"; break;
			default: s = "error " + n; break;
		}
		printMsg(line, col, s);
		count++;
	}

	public void SemErr (int line, int col, String s) {	
		printMsg(line, col, s);
		count++;
	}
	
	public void SemErr (String s) {
		errorStream.println(s);
		count++;
	}
	
	public void Warning (int line, int col, String s) {	
		printMsg(line, col, s);
	}
	
	public void Warning (String s) {
		errorStream.println(s);
	}
} // Errors


class FatalError extends RuntimeException {
	public static final long serialVersionUID = 1L;
	public FatalError(String s) { super(s); }
}

