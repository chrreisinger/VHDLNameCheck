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



public class Parser {
	public static final int _EOF = 0;
	public static final int _basicIdentifier = 1;
	public static final int _extendedIdentifier = 2;
	public static final int _basedLiteral = 3;
	public static final int _integerLiteral = 4;
	public static final int _realLiteral = 5;
	public static final int _stringLiteral = 6;
	public static final int _bitStringLiteral = 7;
	public static final int _apostrophe = 8;
	public static final int _characterLiteral = 9;
	public static final int _abs = 10;
	public static final int _access = 11;
	public static final int _after = 12;
	public static final int _alias = 13;
	public static final int _all = 14;
	public static final int _and = 15;
	public static final int _architecture = 16;
	public static final int _array = 17;
	public static final int _assert = 18;
	public static final int _attribute = 19;
	public static final int _begin = 20;
	public static final int _block = 21;
	public static final int _body = 22;
	public static final int _buffer = 23;
	public static final int _bus = 24;
	public static final int _case = 25;
	public static final int _component = 26;
	public static final int _configuration = 27;
	public static final int _constant = 28;
	public static final int _disconnect = 29;
	public static final int _downto = 30;
	public static final int _else = 31;
	public static final int _ELSIF = 32;
	public static final int _end = 33;
	public static final int _entity = 34;
	public static final int _exit = 35;
	public static final int _file = 36;
	public static final int _for = 37;
	public static final int _function = 38;
	public static final int _generate = 39;
	public static final int _generic = 40;
	public static final int _group = 41;
	public static final int _guarded = 42;
	public static final int _if = 43;
	public static final int _impure = 44;
	public static final int _in = 45;
	public static final int _inertial = 46;
	public static final int _inout = 47;
	public static final int _is = 48;
	public static final int _label = 49;
	public static final int _library = 50;
	public static final int _linkage = 51;
	public static final int _literal = 52;
	public static final int _loop = 53;
	public static final int _map = 54;
	public static final int _mod = 55;
	public static final int _nand = 56;
	public static final int _new = 57;
	public static final int _next = 58;
	public static final int _nor = 59;
	public static final int _not = 60;
	public static final int _null = 61;
	public static final int _of = 62;
	public static final int _on = 63;
	public static final int _open = 64;
	public static final int _or = 65;
	public static final int _others = 66;
	public static final int _outToken = 67;
	public static final int _package = 68;
	public static final int _port = 69;
	public static final int _postponed = 70;
	public static final int _procedure = 71;
	public static final int _process = 72;
	public static final int _protected = 73;
	public static final int _pure = 74;
	public static final int _range = 75;
	public static final int _record = 76;
	public static final int _register = 77;
	public static final int _reject = 78;
	public static final int _rem = 79;
	public static final int _report = 80;
	public static final int _return = 81;
	public static final int _rol = 82;
	public static final int _ror = 83;
	public static final int _select = 84;
	public static final int _severity = 85;
	public static final int _shared = 86;
	public static final int _signal = 87;
	public static final int _sla = 88;
	public static final int _sll = 89;
	public static final int _sra = 90;
	public static final int _srl = 91;
	public static final int _subtype = 92;
	public static final int _then = 93;
	public static final int _to = 94;
	public static final int _transport = 95;
	public static final int _type = 96;
	public static final int _unaffected = 97;
	public static final int _units = 98;
	public static final int _until = 99;
	public static final int _use = 100;
	public static final int _variable = 101;
	public static final int _wait = 102;
	public static final int _when = 103;
	public static final int _while = 104;
	public static final int _with = 105;
	public static final int _xnor = 106;
	public static final int _xor = 107;
	public static final int _doublestar = 108;
	public static final int _leq = 109;
	public static final int _geq = 110;
	public static final int _arrow = 111;
	public static final int _neq = 112;
	public static final int _varAssign = 113;
	public static final int _box = 114;
	public static final int _semicolon = 115;
	public static final int _comma = 116;
	public static final int _ampersand = 117;
	public static final int _lparen = 118;
	public static final int _rparen = 119;
	public static final int _lbracket = 120;
	public static final int _rbracket = 121;
	public static final int _colon = 122;
	public static final int _mul = 123;
	public static final int _div = 124;
	public static final int _plus = 125;
	public static final int _minus = 126;
	public static final int _lt = 127;
	public static final int _gt = 128;
	public static final int _eq = 129;
	public static final int _bar = 130;
	public static final int _dot = 131;
	public static final int maxT = 132;

	static final boolean T = true;
	static final boolean x = false;
	static final int minErrDist = 2;

	public Token t;    // last recognized token
	public Token la;   // lookahead token
	int errDist = minErrDist;
	
	public Scanner scanner;
	public Errors errors;

	private static class ListBuffer<T> {
		private scala.collection.mutable.ListBuffer<T> impl=new scala.collection.mutable.ListBuffer<T>();
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
	
	//group_template = group Identifier is lparen ...
	//GroupDeclaration = group Identifier colon ...
	//la == group
	private boolean isGroupTemplate() {
		if (la.kind!=_group) return false;
		scanner.ResetPeek();
		scanner.Peek();
		Token next = scanner.Peek();
		return next.kind==_is;
	}

	//AttributeDeclaration = attribute Identifier colon TypeMark semicolon 
	//AttributeSpecification = attribute Identifier of EntityNameList colon EntityClass is Expression semicolon 
	//la == attribute
	private boolean isAttributeDeclaration() {
		if (la.kind!=_attribute) return false;
		scanner.ResetPeek();
		scanner.Peek();
		Token next=scanner.Peek();
		return next.kind==_colon;
	}

	//PhysicalTypeDefinition = range Range units ...
	//IntegerOrFloatingPointTypeDefinition = range Range semicolon
	//la == range
	private boolean isPhysicalType() {
		if (la.kind!=_range) return false;
		scanner.ResetPeek();
		Token next;
		do {
			next = scanner.Peek();
		}while (next.kind!=_units && next.kind!=_semicolon);
		return next.kind==_units;
	}

	//constrained array = lparen IndexSubtypeDefinition {comma IndexSubtypeDefinition} rparen of SubtypeIndication
	//IndexSubtypeDefinition = TypeMark range box
	//IndexConstraint = lparen DiscreteRange (comma  DiscreteRange)* rparen
	//la==lparen
	private boolean isIndexSubtypeDefinition() {
		scanner.ResetPeek();
		Token next;
		do {
			next = scanner.Peek();
		}while (next.kind!=_box && next.kind!=_semicolon && next.kind!=_rparen && next.kind!=_lparen);
		return next.kind==_box;
	}
	
	//ProcedureCallStatement = SelectedName [lparen AssociationList rparen] semicolon
	//SignalOrVariableAssignmentStatement = Target (varAssign|leq) ....
	//Target = Name | Aggregate
	private boolean isAssignmentStatement() {
		scanner.ResetPeek();
		Token next;
		int count=0;
		do {
			next = scanner.Peek();
			if (next.kind==_lparen) count++;
			else if (next.kind==_rparen) count--;
		}while (next.kind!=_varAssign && next.kind!=_leq && next.kind!=_semicolon);
		return (next.kind==_varAssign || next.kind==_leq) && count==0;
	}
	
	//ConcurrentSignalAssignmentStatement = (Target leq | with) ....
	//ConcurrentProcedureCallStatement = SelectedName [lparen AssociationList rparen] semicolon
	private boolean isConcurrentSignalAssignmentStatement() {
		if (la.kind==_with) return true;
		scanner.ResetPeek();
		Token next;
		do {
			next = scanner.Peek();
		}while (next.kind!=_leq && next.kind!=_semicolon);
		return next.kind==_leq;
	}
	
	//
	//For those parameters with modes, the only modes that are allowed for formal parameters of a procedure are
	//in, inout, and out. If the mode is in and no object class is explicitly specified, constant is assumed. If the
	//mode is inout or out, and no object class is explicitly specified, variable is assumed.
	//
	//InterfaceConstantDeclaration = [constant] IdentifierList colon [in] SubtypeIndication [varAssign Expression] 
	private boolean isInterfaceConstantDeclaration() {
		if (la.kind==_constant) return true;
		else if (la.kind==_variable || la.kind==_signal || la.kind==_file) return false;
		scanner.ResetPeek();
		Token next;
		do {
			next = scanner.Peek();
		}while (next.kind!=_varAssign && next.kind!=_semicolon && next.kind!=_in);
		return next.kind==_in;
	}
	
	//InterfaceVariableDeclaration = [variable] IdentifierList colon [InterfaceMode] SubtypeIndication [varAssign Expression]
	private boolean isInterfaceVariableDeclaration() {
		if (la.kind==_variable) return true;
		else if (la.kind==_constant || la.kind==_signal || la.kind==_file) return false;
		scanner.ResetPeek();
		Token next;
		do {
			next = scanner.Peek();
		}while (next.kind!=_varAssign && next.kind!=_semicolon && next.kind!=_in && next.kind!=_inout && next.kind!=_outToken);
		return next.kind==_inout || next.kind==_outToken;
	}

	//search fo TO or downto in Range: Expression Direction Expression
	//Choice = DiscreteRange
	//	| SimpleExpression
	//	| others		
	//Choices = Choice { bar Choice}
	//CaseStatement = .... {when Choices arrow SequentialStatementList} ...
	//ElementAssociation = [Choices arrow] Expression 
	//SelectedWaveform = Waveform when Choices
	//SelectedSignalAssignment = ... SelectedWaveform {comma SelectedWaveform} semicolon
	private boolean isRangeInChoice() {
		if (la.kind==_others) return false;
		scanner.ResetPeek();
		Token next;
		do {
			next = scanner.Peek();
		}while (next.kind!=_bar && next.kind!=_arrow && next.kind!=_comma && next.kind!=_semicolon && next.kind!=_to && next.kind!=_downto);
		return next.kind==_to || next.kind==_downto;
	}
	
	//search fo TO or downto in Range = Expression Direction Expression
	//BlockConfigurationIndex= 
	//	  DiscreteRange
	//	  |Expression
	//BlockSpecification=
	//	Identifier [lparen BlockConfigurationIndex rparen]
	//	 | SelectedName
	//BlockConfiguration =
	//		for BlockSpecification
	//		{UseClause}
	//		{
	//			blockConfiguration
	//			|ComponentConfiguration
	//		}
	//		END for semicolon 
	private boolean isDiscreteRangeInBlockConfigurationIndex() {
		scanner.ResetPeek();
		Token next;
		do {
			next = scanner.Peek();
		}while (next.kind!=_use && next.kind!=_for && next.kind!=_end && next.kind!=_semicolon && next.kind!=_to && next.kind!=_downto);
		return next.kind==_to || next.kind==_downto;
	}
	
		/*
		ArchitectureStatement =
		(
		LabelColon<out label> (
			ArchitectureStatementWithLabel<out concurrentStmt,label>
			| ArchitectureStatementOptionalLabel<out concurrentStmt,label>
			)
		| ArchitectureStatementOptionalLabel<out concurrentStmt,label>
		)
		.

		ArchitectureStatementWithLabel =
				(
				ComponentInstantiationStatement
				| BlockStatement //starts with block
				| GenerateStatement  //starts with for|IF
				)
				.
						
		ArchitectureStatementOptionalLabel<out ConcurrentStatement concurrentStmt, Identifier label> =
				[postponed] 
				(ProcessStatement //starts with process
				| ConcurrentAssertionStatement //starts with assert
				| IF(isConcurrentSignalAssignmentStatement())ConcurrentSignalAssignmentStatement
				| ConcurrentProcedureCallStatement
				)
				
		ComponentInstantiationStatement =
		(
		  [component] SelectedName
		  | entity SelectedName [lparen Identifier rparen]
		  | configuration SelectedName
		)
		[GenericMapAspect]
		[PortMapAspect] semicolon
	*/
	private boolean isArchitecutreStatementWithLabel() {
		if (la.kind==_component || la.kind==_entity || la.kind==_configuration  || la.kind==_block || la.kind==_for || la.kind==_if) return true;
		else if (la.kind==_postponed || la.kind==_process || la.kind==_assert || la.kind==_file || isConcurrentSignalAssignmentStatement()) return false;
		//untscheiden ob ComponentInstantiationStatement oder ConcurrentProcedureCallStatement
		scanner.ResetPeek();
		Token next;
		do {
			next = scanner.Peek();
		}while (next.kind!=_generic && next.kind!=_port && next.kind!=_semicolon);
		return next.kind==_generic || next.kind==_port;
	}	
	
	//NameSlicePart = lparen DiscreteRange rparen.
	//name_indexed_part = lparen Expression {comma Expression}
	//NameAssociationListPart = lparen AssociationList rparen
	private boolean isNameSlicePart() {
		if (la.kind!=_lparen) return false;
		scanner.ResetPeek();
		Token next;
		int count=1;//count lparen
		do {
			next = scanner.Peek();
			if (next.kind==_lparen) count++;
			else if (next.kind==_rparen) count--;
		}while (count!=0 && next.kind!=_bar && next.kind!=_arrow && next.kind!=_semicolon && next.kind!=_to && next.kind!=_downto);
		return (next.kind==_to || next.kind==_downto) && count<=1;
	}
		
	/*
	Choice =
			IF(isRangeInChoice())DiscreteRange
			| SimpleExpression
			| others
	Choices = Choice {bar Choice}.
	ElementAssociation = [IF(isChoiceInElementAssociation()) Choices arrow] Expression.
	Aggregate = lparen  ElementAssociation {comma ElementAssociation} rparen .
	*/	
	private boolean isChoiceInElementAssociation() {
		scanner.ResetPeek();
		Token next=la;
		int count=1;//count lparen
		do {
			if (next.kind==_lparen) count++;
			else if (next.kind==_rparen) count--;
			next = scanner.Peek();
		}while (count!=0 && next.kind!=_comma && next.kind!=_arrow && next.kind!=_semicolon && next.kind!=_others);
		return (next.kind==_arrow || next.kind==_others) && count<=1;
	}
	
	/*
	FormalPart<out Name FormalPart> = Name<out FormalPart> .	
	ActualPart<out Option<Expression> ActualPart> =  Expression | open .				
	AssociationElement = [If(isFormalPartInAssociationElement()) FormalPart arrow] ActualPart .		
	AssociationList<out AssociationList list> = AssociationElement {comma AssociationElement}.
	lparen AssociationList rparen
	*/
	private boolean isFormalPartInAssociationElement() {
		if (la.kind!=_basicIdentifier && la.kind!=_extendedIdentifier) return false;
		scanner.ResetPeek();
		Token next;
		int count=0;
		do {
			next = scanner.Peek();
			if (next.kind==_lparen) count++;
			else if (next.kind==_rparen) count--;
		}while (count>=0 && next.kind!=_comma && next.kind!=_others && next.kind!=_arrow && next.kind!=_semicolon && next.kind!=_open);
		return next.kind==_arrow && count==0;
	}
	
	//Range = SimpleExpression Direction  SimpleExpression
	//		| Name
	private boolean isNotNameInRange() {
		scanner.ResetPeek();
		Token next;
		do {
			next = scanner.Peek();
		}while (next.kind!=_units && next.kind!=_varAssign && next.kind!=_generate && next.kind!=_loop && next.kind!=_is && next.kind!=_open && next.kind!=_semicolon && next.kind!=_to && next.kind!=_downto);
		return next.kind==_to || next.kind==_downto;
	}
	
	//DiscreteRange = SubtypeIndication
	//				|Range.
	private boolean isSubtypeIndicationInDiscreteRange() {
		scanner.ResetPeek();
		Token next;
		do {
			next = scanner.Peek();
		}while (next.kind!=_range && next.kind!=_apostrophe && next.kind!=_to && next.kind!=_downto && next.kind!=_lparen && next.kind!=_semicolon);
		return next.kind==_range;
	}
	
	private boolean isQualifiedExpression() {
		if (la.kind!=_basicIdentifier && la.kind!=_extendedIdentifier) return false;
		scanner.ResetPeek();
		Token next;
		//match SelectedName
		do {
			next = scanner.Peek();
		}while (next.kind==_basicIdentifier || next.kind==_extendedIdentifier ||  next.kind==_dot);
		return next.kind==_apostrophe && scanner.Peek().kind==_lparen;
	}
	
	private boolean isComponentConfigurationInBlockConfiguration() {
		scanner.ResetPeek();
		Token next;
		do {
			next = scanner.Peek();
		}while (next.kind!=_colon && next.kind!=_lparen &&  next.kind!=_for);
		return next.kind==_colon;
	}	
	
	private Position toPosition(Token token){
		return new Position(token.line,token.col);
	}    
	
	private Identifier toIdentifier(Token token){
		return toIdentifier(token,true);
	}

	private Identifier toIdentifier(Token token,boolean toLowerCase){
    	if (token.kind!=_stringLiteral && token.kind!=_characterLiteral){
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
    public void init(){
        la = new Token();
		la.val = "";
		Get();
    }
	
	DesignFile  VHDL() {
		DesignFile  designFile;
		ListBuffer<DesignUnit> units=new ListBuffer<DesignUnit>();
		init();

		DesignUnit designUnit = DesignUnit();
		units.append(designUnit); 
		while (StartOf(1)) {
			designUnit = DesignUnit();
			units.append(designUnit); 
		}
		designFile=new DesignFile(units.toList());
		Expect(0);
		
		return designFile;
	}

	DesignUnit  DesignUnit() {
		DesignUnit  designUnit;
		ListBuffer<Identifier> libraries=new ListBuffer<Identifier>();
		ListBuffer<UseClause> useClauses=new ListBuffer<UseClause>();
		
		while (la.kind == 50 || la.kind == 100) {
			if (la.kind == 50) {
				Seq<Identifier> identifierList = LibraryClause();
				libraries.appendAll(identifierList);
			} else {
				UseClause useClause = UseClause();
				useClauses.append(useClause);
			}
		}
		Position pos=toPosition(la);
		LibraryUnit libraryUnit = LibraryUnit();
		designUnit=new DesignUnit(pos,libraries.toList(),useClauses.toList(),toOption(libraryUnit));
		return designUnit;
	}

	Seq<Identifier>  LibraryClause() {
		Seq<Identifier>  identifierList;
		Expect(50);
		identifierList = IdentifierList();
		Expect(115);
		return identifierList;
	}

	UseClause  UseClause() {
		UseClause  useClause;
		Position pos=toPosition(la);
		Expect(100);
		Seq<SelectedName> list = SelectedNameList();
		Expect(115);
		useClause=new UseClause(pos,list);
		return useClause;
	}

	LibraryUnit  LibraryUnit() {
		LibraryUnit  libraryUnit;
		libraryUnit=null;
		if (la.kind == 34) {
			libraryUnit = EntityDeclaration();
		} else if (la.kind == 16) {
			libraryUnit = ArchitectureBody();
		} else if (la.kind==_package && scanner.Peek().kind==_body) {
			libraryUnit = PackageBody();
		} else if (la.kind == 68) {
			libraryUnit = PackageDeclaration();
		} else if (la.kind == 27) {
			libraryUnit = ConfigurationDeclaration();
		} else SynErr(133);
		return libraryUnit;
	}

	EntityDeclaration  EntityDeclaration() {
		EntityDeclaration  entityDecl;
		ListBuffer<DeclarativeItem> declarativeItems=new ListBuffer<DeclarativeItem>();
		ListBuffer<ConcurrentStatement> concurrentStmts=new ListBuffer<ConcurrentStatement>();
		InterfaceList genericClause=null,portClause=null;
		ConcurrentStatement stmt=null;
		Identifier label=null;
		boolean postponed=false;
		
		Expect(34);
		Identifier identifier = Identifier();
		Expect(48);
		if (la.kind == 40) {
			genericClause = GenericClause();
		}
		if (la.kind == 69) {
			portClause = PortClause();
		}
		while (StartOf(2)) {
			DeclarativeItem item = EntityDeclarativeItem();
			declarativeItems.append(item); 
		}
		if (la.kind == 20) {
			Get();
			while (StartOf(3)) {
				if (scanner.Peek().kind==_colon) {
					label = LabelColon();
				}
				if (la.kind == 70) {
					Get();
					postponed=true;
				}
				if (la.kind == 18) {
					stmt = ConcurrentAssertionStatement(label,postponed);
				} else if (la.kind == 1 || la.kind == 2 || la.kind == 6) {
					stmt = ConcurrentProcedureCallStatement(label,postponed);
				} else if (la.kind == 72) {
					stmt = ProcessStatement(label,postponed);
				} else SynErr(134);
				concurrentStmts.append(stmt);
			}
		}
		Expect(33);
		if (la.kind == 34) {
			Get();
		}
		if (la.kind == 1 || la.kind == 2) {
			UnusedIdentifier();
		}
		Expect(115);
		entityDecl=new EntityDeclaration(identifier,toOption(genericClause),toOption(portClause),declarativeItems.toList(),concurrentStmts.toList());
		return entityDecl;
	}

	ArchitectureDeclaration  ArchitectureBody() {
		ArchitectureDeclaration  archDecl;
		ListBuffer<DeclarativeItem> declarativeItems=new ListBuffer<DeclarativeItem>();
		Expect(16);
		Identifier identifier = Identifier();
		Expect(62);
		SelectedName entityName = SelectedName();
		Expect(48);
		while (StartOf(4)) {
			DeclarativeItem item = BlockDeclarativeItem();
			declarativeItems.append(item); 
		}
		Expect(20);
		Seq<ConcurrentStatement> statementList = ArchitectureStatementList();
		Expect(33);
		if (la.kind == 16) {
			Get();
		}
		if (la.kind == 1 || la.kind == 2) {
			UnusedIdentifier();
		}
		Expect(115);
		archDecl=new ArchitectureDeclaration(identifier,declarativeItems.toList(),entityName,statementList); 
		return archDecl;
	}

	PackageBodyDeclaration  PackageBody() {
		PackageBodyDeclaration  packageBody;
		ListBuffer<DeclarativeItem> declarativeItems=new ListBuffer<DeclarativeItem>();
		Expect(68);
		Expect(22);
		Identifier identifier = Identifier();
		Expect(48);
		while (StartOf(5)) {
			DeclarativeItem item = PackageBodyDeclarativeItem();
			declarativeItems.append(item);
		}
		Expect(33);
		if (la.kind == 68) {
			Get();
			Expect(22);
		}
		if (la.kind == 1 || la.kind == 2) {
			UnusedIdentifier();
		}
		Expect(115);
		packageBody = new PackageBodyDeclaration(identifier,declarativeItems.toList());
		return packageBody;
	}

	PackageDeclaration  PackageDeclaration() {
		PackageDeclaration  packageDecl;
		ListBuffer<DeclarativeItem> declarativeItems=new ListBuffer<DeclarativeItem>(); 
		Expect(68);
		Identifier identifier = Identifier();
		Expect(48);
		while (StartOf(6)) {
			DeclarativeItem item = PackageDeclarativeItem();
			declarativeItems.append(item);
		}
		Expect(33);
		if (la.kind == 68) {
			Get();
		}
		if (la.kind == 1 || la.kind == 2) {
			UnusedIdentifier();
		}
		Expect(115);
		packageDecl=new PackageDeclaration(identifier,declarativeItems.toList());
		return packageDecl;
	}

	ConfigurationDeclaration  ConfigurationDeclaration() {
		ConfigurationDeclaration  configDecl;
		ListBuffer<DeclarativeItem> declarativeItems=new ListBuffer<DeclarativeItem>();
		Expect(27);
		Identifier identifier = Identifier();
		Expect(62);
		SelectedName entityName = SelectedName();
		Expect(48);
		while (la.kind == 19 || la.kind == 41 || la.kind == 100) {
			DeclarativeItem item = ConfigurationDeclarativeItem();
			declarativeItems.append(item);
		}
		BlockConfiguration blockConfig = BlockConfiguration();
		Expect(33);
		if (la.kind == 27) {
			Get();
		}
		if (la.kind == 1 || la.kind == 2) {
			UnusedIdentifier();
		}
		Expect(115);
		configDecl=new ConfigurationDeclaration(identifier,declarativeItems.toList(),entityName,blockConfig);
		return configDecl;
	}

	Seq<Identifier>  IdentifierList() {
		Seq<Identifier>  list;
		ListBuffer<Identifier> identifierList=new ListBuffer<Identifier>();
		Identifier identifier = Identifier();
		identifierList.append(identifier);
		while (la.kind == 116) {
			Get();
			identifier = Identifier();
			identifierList.append(identifier);
		}
		list=identifierList.toList();
		return list;
	}

	InterfaceList  GenericClause() {
		InterfaceList  genericList;
		Expect(40);
		Expect(118);
		genericList = GenericInterfaceList();
		Expect(119);
		Expect(115);
		return genericList;
	}

	InterfaceList  GenericInterfaceList() {
		InterfaceList  list;
		ListBuffer<InterfaceList.AbstractInterfaceElement> elements=new ListBuffer<InterfaceList.AbstractInterfaceElement>(); 
		InterfaceList.InterfaceConstantDeclaration declaration = InterfaceConstantDeclaration();
		elements.append(declaration);
		while (la.kind == 115) {
			Get();
			declaration = InterfaceConstantDeclaration();
			elements.append(declaration); 
		}
		list=new InterfaceList(elements.toList());
		return list;
	}

	InterfaceList.InterfaceConstantDeclaration  InterfaceConstantDeclaration() {
		InterfaceList.InterfaceConstantDeclaration  constElement;
		Expression expr=null;
		if (la.kind == 28) {
			Get();
		}
		Seq<Identifier> list = IdentifierList();
		Expect(122);
		if (la.kind == 45) {
			Get();
		}
		SubTypeIndication subType = SubtypeIndication();
		if (la.kind == 113) {
			Get();
			expr = Expression();
		}
		constElement=new InterfaceList.InterfaceConstantDeclaration(list,subType,toOption(expr));
		return constElement;
	}

	InterfaceList  PortClause() {
		InterfaceList  portList;
		Expect(69);
		Expect(118);
		portList = PortInterfaceList();
		Expect(119);
		Expect(115);
		return portList;
	}

	InterfaceList  PortInterfaceList() {
		InterfaceList  list;
		ListBuffer<InterfaceList.AbstractInterfaceElement> elements=new ListBuffer<InterfaceList.AbstractInterfaceElement>(); 
		InterfaceList.InterfaceSignalDeclaration declaration = InterfaceSignalDeclarationProcedure();
		elements.append(declaration); 
		while (la.kind == 115) {
			Get();
			declaration = InterfaceSignalDeclarationProcedure();
			elements.append(declaration);
		}
		list=new InterfaceList(elements.toList());
		return list;
	}

	InterfaceList.InterfaceSignalDeclaration  InterfaceSignalDeclarationProcedure() {
		InterfaceList.InterfaceSignalDeclaration  signalElement;
		Expression expr=null;boolean bus=false;InterfaceMode mode=null;
		if (la.kind == 87) {
			Get();
		}
		Seq<Identifier> list = IdentifierList();
		Expect(122);
		if (StartOf(7)) {
			mode = InterfaceMode();
		}
		SubTypeIndication subType = SubtypeIndication();
		if (la.kind == 24) {
			Get();
			bus=true;
		}
		if (la.kind == 113) {
			Get();
			expr = Expression();
		}
		signalElement=new InterfaceList.InterfaceSignalDeclaration(list,toOption(mode),subType,bus,toOption(expr));
		return signalElement;
	}

	Identifier  Identifier() {
		Identifier  id;
		id=null;
		if (la.kind == 1) {
			Get();
			id=toIdentifier(t);
		} else if (la.kind == 2) {
			Get();
			id=toIdentifier(t,false);
		} else SynErr(135);
		return id;
	}

	DeclarativeItem  EntityDeclarativeItem() {
		DeclarativeItem  item;
		item=null;
		while (!(StartOf(8))) {SynErr(136); Get();}
		if (StartOf(9)) {
			item = SubprogramDeclarationOrBody();
		} else if (la.kind == 96) {
			item = TypeDeclaration();
		} else if (la.kind == 92) {
			item = SubtypeDeclaration();
		} else if (la.kind == 28) {
			item = ConstantDeclaration();
		} else if (la.kind == 87) {
			item = SignalDeclaration();
		} else if (la.kind == 86 || la.kind == 101) {
			item = VariableDeclaration();
		} else if (la.kind == 36) {
			item = FileDeclaration();
		} else if (la.kind == 13) {
			item = AliasDeclaration();
		} else if (isAttributeDeclaration()) {
			item = AttributeDeclaration();
		} else if (la.kind == 19) {
			item = AttributeSpecification();
		} else if (la.kind == 29) {
			item = DisconnectionSpecification();
		} else if (la.kind == 100) {
			item = UseClause();
		} else if (isGroupTemplate()) {
			item = GroupTemplateDeclaration();
		} else if (la.kind == 41) {
			item = GroupDeclaration();
		} else SynErr(137);
		return item;
	}

	Identifier  LabelColon() {
		Identifier  label;
		label = Identifier();
		Expect(122);
		return label;
	}

	ConcurrentAssertionStatement  ConcurrentAssertionStatement(Identifier label,boolean postponed) {
		ConcurrentAssertionStatement  assertStmt;
		Position pos=toPosition(la);Expression report_expression=null,severity_expression=null;
		Expect(18);
		Expression expr = Condition();
		if (la.kind == 80) {
			Get();
			report_expression = Expression();
		}
		if (la.kind == 85) {
			Get();
			severity_expression = Expression();
		}
		Expect(115);
		assertStmt=new ConcurrentAssertionStatement(pos,toOption(label),postponed,expr,toOption(report_expression),toOption(severity_expression));
		return assertStmt;
	}

	ConcurrentProcedureCallStatement  ConcurrentProcedureCallStatement(Identifier label,boolean postponed) {
		ConcurrentProcedureCallStatement  procedureCallStmt;
		AssociationList paramterList=null;
		SelectedName procedure_name = SelectedName();
		if (la.kind == 118) {
			Get();
			paramterList = AssociationList();
			Expect(119);
		}
		Expect(115);
		procedureCallStmt=new ConcurrentProcedureCallStatement(toOption(label),postponed,procedure_name,toOption(paramterList));
		return procedureCallStmt;
	}

	ProcessStatement  ProcessStatement(Identifier label,boolean postponed) {
		ProcessStatement  processStmt;
		Position pos=toPosition(la);
		ListBuffer<DeclarativeItem> declarativeItems=new ListBuffer<DeclarativeItem>();
		Seq<Name> NameList=null;
		
		Expect(72);
		if (la.kind == 118) {
			Get();
			NameList = NameList();
			Expect(119);
		}
		if (la.kind == 48) {
			Get();
		}
		while (StartOf(10)) {
			DeclarativeItem item = ProcessDeclarativeItem();
			declarativeItems.append(item);
		}
		Expect(20);
		Seq<SequentialStatement> sequentialStatements = SequentialStatementList();
		Expect(33);
		if (la.kind == 70) {
			Get();
		}
		Expect(72);
		if (la.kind == 1 || la.kind == 2) {
			UnusedIdentifier();
		}
		Expect(115);
		processStmt=new ProcessStatement(pos,toOption(label),postponed,toOption(NameList),declarativeItems.toList(),sequentialStatements);
		return processStmt;
	}

	void UnusedIdentifier() {
		Identifier unused = Identifier();
	}

	DeclarativeItem  SubprogramDeclarationOrBody() {
		DeclarativeItem  declOrBody;
		SubProgramDefinition subProgramDef=null;
		SubProgramDeclaration decl = SubprogramSpecification();
		if (la.kind == 48) {
			subProgramDef = SubprogramBody(decl);
		}
		Expect(115);
		if (subProgramDef!=null) declOrBody=subProgramDef; else declOrBody=decl;
		return declOrBody;
	}

	AbstractTypeDeclaration  TypeDeclaration() {
		AbstractTypeDeclaration  typeDecl;
		Position pos=toPosition(la);typeDecl=null;
		Expect(96);
		Identifier id = Identifier();
		if (la.kind == 48) {
			Get();
			typeDecl = TypeDefinition(id,pos);
			Expect(115);
		} else if (la.kind == 115) {
			Get();
			typeDecl=new IncompleteTypeDeclaration(pos,id);
		} else SynErr(138);
		return typeDecl;
	}

	SubTypeDeclaration  SubtypeDeclaration() {
		SubTypeDeclaration  subTypeDecl;
		Position pos=toPosition(la);
		Expect(92);
		Identifier identifier = Identifier();
		Expect(48);
		SubTypeIndication subType = SubtypeIndication();
		Expect(115);
		subTypeDecl=new SubTypeDeclaration(pos,identifier,subType);
		return subTypeDecl;
	}

	ConstantDeclaration  ConstantDeclaration() {
		ConstantDeclaration  constantDecl;
		Position pos=toPosition(la);Expression expr=null;
		Expect(28);
		Seq<Identifier> list = IdentifierList();
		Expect(122);
		SubTypeIndication subType = SubtypeIndication();
		if (la.kind == 113) {
			Get();
			expr = Expression();
		}
		Expect(115);
		constantDecl=new ConstantDeclaration(pos,list,subType,toOption(expr));
		return constantDecl;
	}

	SignalDeclaration  SignalDeclaration() {
		SignalDeclaration  signalDecl;
		Position pos=toPosition(la);Expression expr=null;boolean reg=false,bus=false;
		Expect(87);
		Seq<Identifier> list = IdentifierList();
		Expect(122);
		SubTypeIndication subType = SubtypeIndication();
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
			expr = Expression();
		}
		Expect(115);
		SignalType signalType=null;
		if(reg) signalType=SignalType.REGISTER;
		else if (bus) signalType=SignalType.BUS;
		signalDecl=new SignalDeclaration(pos,list,subType,toOption(signalType),toOption(expr));
		
		return signalDecl;
	}

	VariableDeclaration  VariableDeclaration() {
		VariableDeclaration  varDecl;
		Position pos=toPosition(la);Expression expr=null;boolean shared=false;
		if (la.kind == 86) {
			Get();
			shared=true;
		}
		Expect(101);
		Seq<Identifier> list = IdentifierList();
		Expect(122);
		SubTypeIndication subType = SubtypeIndication();
		if (la.kind == 113) {
			Get();
			expr = Expression();
		}
		Expect(115);
		varDecl=new VariableDeclaration(pos,shared,list,subType,toOption(expr));
		return varDecl;
	}

	FileDeclaration  FileDeclaration() {
		FileDeclaration  fileDecl;
		Position pos=toPosition(la);Expression file_open_kind_expression=null,file_logical_name=null;
		Expect(36);
		Seq<Identifier> list = IdentifierList();
		Expect(122);
		SubTypeIndication subType = SubtypeIndication();
		if (la.kind == 48 || la.kind == 64) {
			if (la.kind == 64) {
				Get();
				file_open_kind_expression = Expression();
			}
			Expect(48);
			file_logical_name = Expression();
		}
		Expect(115);
		fileDecl=new FileDeclaration(pos,list,subType,toOption(file_open_kind_expression),toOption(file_logical_name));
		return fileDecl;
	}

	AliasDeclaration  AliasDeclaration() {
		AliasDeclaration  aliasDecl;
		Position pos=toPosition(la);Signature signature=null;SubTypeIndication subType=null;
		Expect(13);
		Identifier designator = AliasDesignator();
		if (la.kind == 122) {
			Get();
			subType = SubtypeIndication();
		}
		Expect(48);
		Name name = Name();
		if (la.kind == 120) {
			signature = Signature();
		}
		Expect(115);
		aliasDecl=new AliasDeclaration(pos,designator,toOption(subType),name,toOption(signature));
		return aliasDecl;
	}

	AttributeDeclaration  AttributeDeclaration() {
		AttributeDeclaration  attributeDecl;
		Position pos=toPosition(la);
		Expect(19);
		Identifier identifier = Identifier();
		Expect(122);
		SelectedName type = TypeMark();
		Expect(115);
		attributeDecl=new AttributeDeclaration(pos,identifier,type);
		return attributeDecl;
	}

	AttributeSpecification  AttributeSpecification() {
		AttributeSpecification  node;
		Position pos=toPosition(la);Either<Seq<Tuple2<Identifier,Option<Signature>>>,Identifier> nameList=null;
		Expect(19);
		Identifier identifier = Identifier();
		Expect(62);
		nameList = EntityNameList();
		Expect(122);
		EntityClass entityClass = EntityClass();
		Expect(48);
		Expression expr = Expression();
		Expect(115);
		node=new AttributeSpecification(pos,identifier,nameList,entityClass,expr);
		return node;
	}

	DisconnectionSpecification  DisconnectionSpecification() {
		DisconnectionSpecification  disconnectSpec;
		Position pos=toPosition(la);Identifier id=null;Seq<SelectedName> list=null;
		Expect(29);
		if (la.kind == 1 || la.kind == 2 || la.kind == 6) {
			list = SelectedNameList();
		} else if (la.kind == 66) {
			Get();
			id=toIdentifier(t);
		} else if (la.kind == 14) {
			Get();
			id=toIdentifier(t);
		} else SynErr(139);
		Expect(122);
		SelectedName type = TypeMark();
		Expect(12);
		Expression expr = Expression();
		Expect(115);
		disconnectSpec= new DisconnectionSpecification(pos,id==null?new Left<Seq<SelectedName>,Identifier>(list):new Right<Seq<SelectedName>,Identifier>(id),type,expr);
		return disconnectSpec;
	}

	GroupTemplateDeclaration  GroupTemplateDeclaration() {
		GroupTemplateDeclaration  groupTemplateDecl;
		Position pos=toPosition(la);
		ListBuffer<GroupTemplateDeclaration.Element> elements=new ListBuffer<GroupTemplateDeclaration.Element>(); 
		 
		Expect(41);
		Identifier identifier = Identifier();
		Expect(48);
		Expect(118);
		GroupTemplateDeclaration.Element entry = EntityClassEntry();
		elements.append(entry);
		while (la.kind == 116) {
			Get();
			entry = EntityClassEntry();
			elements.append(entry);
		}
		Expect(119);
		Expect(115);
		groupTemplateDecl=new GroupTemplateDeclaration(pos,identifier,elements.toList());
		return groupTemplateDecl;
	}

	GroupDeclaration  GroupDeclaration() {
		GroupDeclaration  groupDecl;
		Position pos=toPosition(la);Seq<Either<Name,Identifier>> list=null;
		Expect(41);
		Identifier identifier = Identifier();
		Expect(122);
		SelectedName selectedName = SelectedName();
		Expect(118);
		list = GroupConstituentList();
		Expect(119);
		Expect(115);
		groupDecl=new GroupDeclaration(pos,identifier,selectedName,list);
		return groupDecl;
	}

	SelectedName  SelectedName() {
		SelectedName  name;
		ListBuffer<Identifier> parts=new ListBuffer<Identifier>();
		Identifier prefix = NamePrefix();
		while (la.kind == 131) {
			Name.SelectedPart selectedPart = NameSelectedPart();
			parts.append(selectedPart.identifier());
		}
		parts.prepend(prefix); name =new SelectedName(parts.toList());
		return name;
	}

	DeclarativeItem  BlockDeclarativeItem() {
		DeclarativeItem  item;
		item=null;
		while (!(StartOf(11))) {SynErr(140); Get();}
		if (StartOf(9)) {
			item = SubprogramDeclarationOrBody();
		} else if (la.kind == 96) {
			item = TypeDeclaration();
		} else if (la.kind == 92) {
			item = SubtypeDeclaration();
		} else if (la.kind == 28) {
			item = ConstantDeclaration();
		} else if (la.kind == 87) {
			item = SignalDeclaration();
		} else if (la.kind == 86 || la.kind == 101) {
			item = VariableDeclaration();
		} else if (la.kind == 36) {
			item = FileDeclaration();
		} else if (la.kind == 13) {
			item = AliasDeclaration();
		} else if (la.kind == 26) {
			item = CcomponentDeclaration();
		} else if (isAttributeDeclaration()) {
			item = AttributeDeclaration();
		} else if (la.kind == 19) {
			item = AttributeSpecification();
		} else if (la.kind == 37) {
			item = ConfigurationSpecification();
		} else if (la.kind == 29) {
			item = DisconnectionSpecification();
		} else if (la.kind == 100) {
			item = UseClause();
		} else if (isGroupTemplate()) {
			item = GroupTemplateDeclaration();
		} else if (la.kind == 41) {
			item = GroupDeclaration();
		} else SynErr(141);
		return item;
	}

	Seq<ConcurrentStatement>  ArchitectureStatementList() {
		Seq<ConcurrentStatement>  list;
		ListBuffer<ConcurrentStatement> statementList=new ListBuffer<ConcurrentStatement>();
		while (StartOf(12)) {
			ConcurrentStatement stmt = ArchitectureStatement();
			statementList.append(stmt);
		}
		list=statementList.toList();
		return list;
	}

	DeclarativeItem  ConfigurationDeclarativeItem() {
		DeclarativeItem  item;
		item=null;
		while (!(StartOf(13))) {SynErr(142); Get();}
		if (la.kind == 100) {
			item = UseClause();
		} else if (la.kind == 41) {
			item = GroupDeclaration();
		} else if (la.kind == 19) {
			item = AttributeSpecification();
		} else SynErr(143);
		return item;
	}

	BlockConfiguration  BlockConfiguration() {
		BlockConfiguration  blockConfig;
		ListBuffer<UseClause> useClauses=new ListBuffer<UseClause>();
		ListBuffer<Object> configurations=new ListBuffer<Object>();
		
		Expect(37);
		BlockConfigurationSpecification blockSpec = BlockSpecification();
		while (la.kind == 100) {
			UseClause useClause = UseClause();
			useClauses.append(useClause);
		}
		while (la.kind == 37) {
			if (isComponentConfigurationInBlockConfiguration()) {
				ComponentConfiguration componentConfiguration = ComponentConfiguration();
				configurations.append(componentConfiguration);
			} else {
				BlockConfiguration blockConfiguration = BlockConfiguration();
				configurations.append(blockConfiguration);
			}
		}
		Expect(33);
		Expect(37);
		Expect(115);
		blockConfig=new BlockConfiguration(blockSpec,useClauses.toList(),configurations.toList());
		return blockConfig;
	}

	Either<DiscreteRange,Expression>  BlockConfigurationIndex() {
		Either<DiscreteRange,Expression>  index;
		index=null;
		if (isDiscreteRangeInBlockConfigurationIndex()) {
			DiscreteRange discreteRange = DiscreteRange();
			index=new Left<DiscreteRange,Expression>(discreteRange);
		} else if (StartOf(14)) {
			Expression expr = Expression();
			index=new Right<DiscreteRange,Expression>(expr);
		} else SynErr(144);
		return index;
	}

	DiscreteRange  DiscreteRange() {
		DiscreteRange  discreteRange;
		discreteRange=null;
		if (isSubtypeIndicationInDiscreteRange()) {
			SubTypeIndication subType = SubtypeIndication();
			discreteRange=new DiscreteRange(new Right<Range, SubTypeIndication>(subType));
		} else if (StartOf(14)) {
			Range range = Range();
			discreteRange=new DiscreteRange(new Left<Range, SubTypeIndication>(range));
		} else SynErr(145);
		return discreteRange;
	}

	Expression  Expression() {
		Expression  expr;
		Tuple2<Position,Operators.Logical> op=null;
		expr = Relation();
		while (StartOf(15)) {
			op = LogicalOperator();
			Expression right = Relation();
			expr=new LogicalExpression(op._1,expr,op._2,right);
		}
		return expr;
	}

	BlockConfigurationSpecification  BlockSpecification() {
		BlockConfigurationSpecification  blockSpec;
		blockSpec=null;Either<DiscreteRange,Expression> blockIndex=null;
		if (scanner.Peek().kind==_lparen) {
			Identifier identifier = Identifier();
			if (la.kind == 118) {
				Get();
				blockIndex = BlockConfigurationIndex();
				Expect(119);
			}
			blockSpec=new BlockConfigurationSpecification(new Right<SelectedName, Tuple2<Identifier, Option<Either<DiscreteRange, Expression>>>>(new Tuple2<Identifier, Option<Either<DiscreteRange, Expression>>>(identifier,toOption(blockIndex))));
		} else if (la.kind == 1 || la.kind == 2 || la.kind == 6) {
			SelectedName selectedName = SelectedName();
			blockSpec=new BlockConfigurationSpecification(new Left<SelectedName, Tuple2<Identifier, Option<Either<DiscreteRange, Expression>>>>(selectedName));
		} else SynErr(146);
		return blockSpec;
	}

	ComponentConfiguration  ComponentConfiguration() {
		ComponentConfiguration  componentConfig;
		BlockConfiguration blockConfiguration=null;Object indication=null;
		Expect(37);
		Object componentSpec = ComponentSpecification();
		if (StartOf(16)) {
			indication = BindingIndication();
			Expect(115);
		}
		if (la.kind == 37) {
			blockConfiguration = BlockConfiguration();
		}
		Expect(33);
		Expect(37);
		Expect(115);
		componentConfig=new ComponentConfiguration(componentSpec,toOption(indication),toOption(blockConfiguration));
		return componentConfig;
	}

	Object  ComponentSpecification() {
		Object  spec;
		Object list = InstantiationList();
		Expect(122);
		SelectedName name = SelectedName();
		spec=null;
		return spec;
	}

	Object  BindingIndication() {
		Object  indication;
		if (la.kind == 100) {
			Get();
			EntityAspect();
		}
		if (la.kind == 40) {
			AssociationList genericMap = GenericMapAspect();
		}
		if (la.kind == 69) {
			AssociationList portMap = PortMapAspect();
		}
		indication=null;
		return indication;
	}

	DeclarativeItem  PackageDeclarativeItem() {
		DeclarativeItem  item;
		item=null;
		while (!(StartOf(17))) {SynErr(147); Get();}
		if (StartOf(9)) {
			item = SubprogramDeclaration();
		} else if (la.kind == 96) {
			item = TypeDeclaration();
		} else if (la.kind == 92) {
			item = SubtypeDeclaration();
		} else if (la.kind == 28) {
			item = ConstantDeclaration();
		} else if (la.kind == 87) {
			item = SignalDeclaration();
		} else if (la.kind == 86 || la.kind == 101) {
			item = VariableDeclaration();
		} else if (la.kind == 36) {
			item = FileDeclaration();
		} else if (la.kind == 13) {
			item = AliasDeclaration();
		} else if (la.kind == 26) {
			item = CcomponentDeclaration();
		} else if (isAttributeDeclaration()) {
			item = AttributeDeclaration();
		} else if (la.kind == 19) {
			item = AttributeSpecification();
		} else if (la.kind == 29) {
			item = DisconnectionSpecification();
		} else if (la.kind == 100) {
			item = UseClause();
		} else if (isGroupTemplate()) {
			item = GroupTemplateDeclaration();
		} else if (la.kind == 41) {
			item = GroupDeclaration();
		} else SynErr(148);
		return item;
	}

	DeclarativeItem  SubprogramDeclaration() {
		DeclarativeItem  subprogramDecl;
		subprogramDecl = SubprogramSpecification();
		Expect(115);
		return subprogramDecl;
	}

	ComponentDeclaration  CcomponentDeclaration() {
		ComponentDeclaration  componentDecl;
		Position pos=toPosition(la);InterfaceList genericClause=null,portClause=null;
		Expect(26);
		Identifier identifier = Identifier();
		if (la.kind == 48) {
			Get();
		}
		if (la.kind == 40) {
			genericClause = GenericClause();
		}
		if (la.kind == 69) {
			portClause = PortClause();
		}
		Expect(33);
		Expect(26);
		if (la.kind == 1 || la.kind == 2) {
			UnusedIdentifier();
		}
		Expect(115);
		componentDecl=new ComponentDeclaration(pos,identifier,toOption(genericClause),toOption(portClause));
		return componentDecl;
	}

	DeclarativeItem  PackageBodyDeclarativeItem() {
		DeclarativeItem  item;
		item=null;
		while (!(StartOf(18))) {SynErr(149); Get();}
		if (StartOf(9)) {
			item = SubprogramDeclarationOrBody();
		} else if (la.kind == 96) {
			item = TypeDeclaration();
		} else if (la.kind == 92) {
			item = SubtypeDeclaration();
		} else if (la.kind == 28) {
			item = ConstantDeclaration();
		} else if (la.kind == 87) {
			item = SignalDeclaration();
		} else if (la.kind == 86 || la.kind == 101) {
			item = VariableDeclaration();
		} else if (la.kind == 36) {
			item = FileDeclaration();
		} else if (la.kind == 13) {
			item = AliasDeclaration();
		} else if (la.kind == 100) {
			item = UseClause();
		} else if (la.kind == 19) {
			item = AttributeSpecification();
		} else if (isGroupTemplate()) {
			item = GroupTemplateDeclaration();
		} else if (la.kind == 41) {
			item = GroupDeclaration();
		} else SynErr(150);
		return item;
	}

	Identifier  Designator() {
		Identifier  identifier;
		identifier = null;
		if (la.kind == 1 || la.kind == 2) {
			identifier = Identifier();
		} else if (la.kind == 6) {
			Get();
			identifier=toIdentifier(t);
		} else SynErr(151);
		return identifier;
	}

	SubProgramDeclaration  SubprogramSpecification() {
		SubProgramDeclaration  decl;
		Position pos=toPosition(la);Identifier designator=null;InterfaceList list=null;decl=null;
		if (la.kind == 71) {
			Get();
			designator = Designator();
			if (la.kind == 118) {
				Get();
				list = ParameterInterfaceListProcedure();
				Expect(119);
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
			designator = Designator();
			if (la.kind == 118) {
				Get();
				list = ParameterInterfaceListFunction();
				Expect(119);
			}
			Expect(81);
			SelectedName returnType = TypeMark();
			decl=new FunctionDeclaration(pos,pure,designator,toOption(list),returnType);
		} else SynErr(152);
		return decl;
	}

	InterfaceList  ParameterInterfaceListProcedure() {
		InterfaceList  list;
		ListBuffer<InterfaceList.AbstractInterfaceElement> elements=new ListBuffer<InterfaceList.AbstractInterfaceElement>();
		InterfaceList.AbstractInterfaceElement element=null;
		
		element = InterfaceElementProcedure();
		elements.append(element);
		while (la.kind == 115) {
			Get();
			element = InterfaceElementProcedure();
			elements.append(element);
		}
		list=new InterfaceList(elements.toList());
		return list;
	}

	InterfaceList  ParameterInterfaceListFunction() {
		InterfaceList  list;
		ListBuffer<InterfaceList.AbstractInterfaceElement> elements=new ListBuffer<InterfaceList.AbstractInterfaceElement>();
		InterfaceList.AbstractInterfaceElement element=null;
		
		element = InterfaceElementFunction();
		elements.append(element);
		while (la.kind == 115) {
			Get();
			element = InterfaceElementFunction();
			elements.append(element);
		}
		list=new InterfaceList(elements.toList());
		return list;
	}

	SelectedName  TypeMark() {
		SelectedName  typeName;
		typeName = SelectedName();
		return typeName;
	}

	SubProgramDefinition  SubprogramBody(SubProgramDeclaration subprogramDecl) {
		SubProgramDefinition  subProgramDef;
		ListBuffer<DeclarativeItem> declarativeItems=new ListBuffer<DeclarativeItem>(); 
		Expect(48);
		while (StartOf(10)) {
			DeclarativeItem item = SubprogramDeclarativeItem();
			declarativeItems.append(item); 
		}
		Expect(20);
		Seq<SequentialStatement> sequentialStatements = SequentialStatementList();
		Expect(33);
		if (la.kind == 38 || la.kind == 71) {
			if (la.kind == 71) {
				Get();
			} else {
				Get();
			}
		}
		if (la.kind == 1 || la.kind == 2 || la.kind == 6) {
			Identifier unused = Designator();
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

	DeclarativeItem  SubprogramDeclarativeItem() {
		DeclarativeItem  item;
		item=null;
		while (!(StartOf(19))) {SynErr(153); Get();}
		if (StartOf(9)) {
			item = SubprogramDeclarationOrBody();
		} else if (la.kind == 96) {
			item = TypeDeclaration();
		} else if (la.kind == 92) {
			item = SubtypeDeclaration();
		} else if (la.kind == 28) {
			item = ConstantDeclaration();
		} else if (la.kind == 86 || la.kind == 101) {
			item = VariableDeclaration();
		} else if (la.kind == 36) {
			item = FileDeclaration();
		} else if (la.kind == 13) {
			item = AliasDeclaration();
		} else if (isAttributeDeclaration()) {
			item = AttributeDeclaration();
		} else if (la.kind == 19) {
			item = AttributeSpecification();
		} else if (la.kind == 100) {
			item = UseClause();
		} else if (isGroupTemplate()) {
			item = GroupTemplateDeclaration();
		} else if (la.kind == 41) {
			item = GroupDeclaration();
		} else SynErr(154);
		return item;
	}

	Seq<SequentialStatement>  SequentialStatementList() {
		Seq<SequentialStatement>  list;
		ListBuffer<SequentialStatement> tmpList=new ListBuffer<SequentialStatement>();
		while (StartOf(20)) {
			SequentialStatement stmt = SequentialStatement();
			tmpList.append(stmt);
		}
		list=tmpList.toList();
		return list;
	}

	AbstractTypeDeclaration  TypeDefinition(Identifier id,Position pos) {
		AbstractTypeDeclaration  typeDef;
		typeDef=null;
		if (la.kind == 118) {
			typeDef = EnumerationTypeDefinition(id,pos);
		} else if (isPhysicalType()) {
			typeDef = PhysicalTypeDefinition(id,pos);
		} else if (la.kind == 75) {
			typeDef = IntegerOrFloatingPointTypeDefinition(id,pos);
		} else if (la.kind == 17) {
			typeDef = ArrayTypeDefinition(id,pos);
		} else if (la.kind == 76) {
			typeDef = RecordTypeDefinition(id,pos);
		} else if (la.kind == 11) {
			typeDef = AccessTypeDefinition(id,pos);
		} else if (la.kind == 36) {
			typeDef = FileTypeDefinition(id,pos);
		} else if (la.kind==_protected && scanner.Peek().kind==_body) {
			typeDef = ProtectedTypeBody(id,pos);
		} else if (la.kind == 73) {
			typeDef = ProtectedTypeDeclaration(id,pos);
		} else SynErr(155);
		return typeDef;
	}

	EnumerationTypeDefinition  EnumerationTypeDefinition(Identifier id,Position pos) {
		EnumerationTypeDefinition  enumTypeDef;
		ListBuffer<Identifier> elements=new ListBuffer<Identifier>(); 
		Identifier element=null;
		
		Expect(118);
		element = EnumerationLiteral();
		elements.append(element);
		while (la.kind == 116) {
			Get();
			element = EnumerationLiteral();
			elements.append(element);
		}
		Expect(119);
		enumTypeDef=new EnumerationTypeDefinition(pos,id,elements.toList());
		return enumTypeDef;
	}

	PhysicalTypeDefinition  PhysicalTypeDefinition(Identifier id,Position pos) {
		PhysicalTypeDefinition  physicalTypeDef;
		ListBuffer<PhysicalTypeDefinition.Element> elements=new ListBuffer<PhysicalTypeDefinition.Element>(); 
		Expect(75);
		Range range = Range();
		Expect(98);
		Identifier baseIdentifier = Identifier();
		Expect(115);
		while (la.kind == 1 || la.kind == 2) {
			Identifier identifier = Identifier();
			Expect(129);
			PhysicalLiteral literal = PhysicalLiteral();
			Expect(115);
			elements.append(new PhysicalTypeDefinition.Element(identifier,literal));
		}
		Expect(33);
		Expect(98);
		if (la.kind == 1 || la.kind == 2) {
			UnusedIdentifier();
		}
		physicalTypeDef=new PhysicalTypeDefinition(pos,id,range,baseIdentifier,elements.toList());
		return physicalTypeDef;
	}

	IntegerOrFloatingPointTypeDefinition  IntegerOrFloatingPointTypeDefinition(Identifier id,Position pos) {
		IntegerOrFloatingPointTypeDefinition  intOrFloat;
		Expect(75);
		Range range = Range();
		intOrFloat=new IntegerOrFloatingPointTypeDefinition(pos,id,range);
		return intOrFloat;
	}

	AbstractArrayTypeDefinition  ArrayTypeDefinition(Identifier id,Position pos) {
		AbstractArrayTypeDefinition  arrayTypeDef;
		ListBuffer<SelectedName> unConstraintList=new ListBuffer<SelectedName>();
		SubTypeIndication subType=null;
		Seq<DiscreteRange> ranges =null;
		SelectedName type=null;
		
		Expect(17);
		if (isIndexSubtypeDefinition()) {
			Expect(118);
			type = IndexSubtypeDefinition();
			unConstraintList.append(type);
			while (la.kind == 116) {
				Get();
				type = IndexSubtypeDefinition();
				unConstraintList.append(type);
			}
			Expect(119);
			Expect(62);
			subType = SubtypeIndication();
		} else if (la.kind == 118) {
			ranges = IndexConstraint();
			Expect(62);
			subType = SubtypeIndication();
		} else SynErr(156);
		if (unConstraintList.isEmpty()) arrayTypeDef=new ConstrainedArrayTypeDefinition(pos,id,ranges,subType);
		else arrayTypeDef=new UnconstrainedArrayTypeDefinition(pos,id,unConstraintList.toList(),subType);
		
		return arrayTypeDef;
	}

	RecordTypeDefinition  RecordTypeDefinition(Identifier id,Position pos) {
		RecordTypeDefinition  recordTypeDef;
		ListBuffer<RecordTypeDefinition.Element> elements=new ListBuffer<RecordTypeDefinition.Element>(); 
		Expect(76);
		Seq<Identifier> list = IdentifierList();
		Expect(122);
		SubTypeIndication subType = SubtypeIndication();
		Expect(115);
		elements.append(new RecordTypeDefinition.Element(list, subType));
		while (la.kind == 1 || la.kind == 2) {
			list = IdentifierList();
			Expect(122);
			subType = SubtypeIndication();
			Expect(115);
			elements.append(new RecordTypeDefinition.Element(list, subType));
		}
		Expect(33);
		Expect(76);
		if (la.kind == 1 || la.kind == 2) {
			UnusedIdentifier();
		}
		recordTypeDef=new RecordTypeDefinition(pos,id,elements.toList());
		return recordTypeDef;
	}

	AccessTypeDefinition  AccessTypeDefinition(Identifier id,Position pos) {
		AccessTypeDefinition  accessTypeDef;
		Expect(11);
		SubTypeIndication subType = SubtypeIndication();
		accessTypeDef=new AccessTypeDefinition(pos,id,subType);
		return accessTypeDef;
	}

	FileTypeDefinition  FileTypeDefinition(Identifier id,Position pos) {
		FileTypeDefinition  fileTypeDef;
		Expect(36);
		Expect(62);
		SelectedName type = TypeMark();
		fileTypeDef=new FileTypeDefinition(pos,id,type);
		return fileTypeDef;
	}

	ProtectedTypeBodyDeclaration  ProtectedTypeBody(Identifier id,Position pos) {
		ProtectedTypeBodyDeclaration  protectedTypeBody;
		ListBuffer<DeclarativeItem> items=new ListBuffer<DeclarativeItem>(); 
		Expect(73);
		Expect(22);
		while (StartOf(10)) {
			DeclarativeItem item = ProtectedTypeBodyDeclarativeItem();
			items.append(item);
		}
		Expect(33);
		Expect(73);
		Expect(22);
		if (la.kind == 1 || la.kind == 2) {
			UnusedIdentifier();
		}
		protectedTypeBody=new ProtectedTypeBodyDeclaration(pos,id,items.toList());
		return protectedTypeBody;
	}

	ProtectedTypeDeclaration  ProtectedTypeDeclaration(Identifier id,Position pos) {
		ProtectedTypeDeclaration  protectedTypeDecl;
		ListBuffer<DeclarativeItem> items=new ListBuffer<DeclarativeItem>(); 
		Expect(73);
		while (StartOf(21)) {
			DeclarativeItem item = ProtectedTypeDeclarativeItem();
			items.append(item);
		}
		Expect(33);
		Expect(73);
		if (la.kind == 1 || la.kind == 2) {
			UnusedIdentifier();
		}
		protectedTypeDecl=new ProtectedTypeDeclaration(pos,id,items.toList());
		return protectedTypeDecl;
	}

	SubTypeIndication  SubtypeIndication() {
		SubTypeIndication  subType;
		Either<Range,Seq<DiscreteRange>> constraint=null;SelectedName n2=null;
		SelectedName n1 = SelectedName();
		if (la.kind == 1 || la.kind == 2 || la.kind == 6) {
			n2 = SelectedName();
		}
		if (la.kind == 75 || la.kind == 118) {
			constraint = Constraint();
		}
		if (n2!=null) subType=new SubTypeIndication(toOption(n1),n2,toOption(constraint));
		else subType=new SubTypeIndication(n1,toOption(constraint));
		
		return subType;
	}

	Identifier  AliasDesignator() {
		Identifier  identifier;
		identifier=null;
		if (la.kind == 1 || la.kind == 2) {
			identifier = Identifier();
		} else if (la.kind == 9) {
			Get();
			identifier=toIdentifier(t);
		} else if (la.kind == 6) {
			Get();
			identifier=toIdentifier(t);
		} else SynErr(157);
		return identifier;
	}

	Name  Name() {
		Name  name;
		ListBuffer<Name.Part> parts=new ListBuffer<Name.Part>();
		Identifier prefix = NamePrefix();
		while (StartOf(22)) {
			Name.Part part = NamePart();
			parts.append(part);
		}
		name =new Name(prefix,parts.toList());
		return name;
	}

	Signature  Signature() {
		Signature  signature;
		Seq<SelectedName> list=null;SelectedName returnType=null;
		Expect(120);
		if (la.kind == 1 || la.kind == 2 || la.kind == 6) {
			list = SelectedNameList();
		}
		if (la.kind == 81) {
			Get();
			returnType = TypeMark();
		}
		Expect(121);
		signature =new Signature(toOption(list),toOption(returnType));
		return signature;
	}

	Either<Seq<Tuple2<Identifier,Option<Signature>>>,Identifier>  EntityNameList() {
		Either<Seq<Tuple2<Identifier,Option<Signature>>>,Identifier>  list;
		ListBuffer<Tuple2<Identifier,Option<Signature>>> elements=new ListBuffer<Tuple2<Identifier,Option<Signature>>>();
		Tuple2<Identifier,Option<Signature>> designator=null;
		list=null;
		
		if (StartOf(23)) {
			designator = EntityDesignator();
			elements.append(designator); 
			while (la.kind == 116) {
				Get();
				designator = EntityDesignator();
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

	EntityClass  EntityClass() {
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

	Tuple2<Identifier,Option<Signature>>  EntityDesignator() {
		Tuple2<Identifier,Option<Signature>>  designator;
		Identifier identifier=null; Signature signature=null;
		if (la.kind == 1 || la.kind == 2) {
			identifier = Identifier();
		} else if (la.kind == 9) {
			Get();
			identifier=toIdentifier(t);
		} else if (la.kind == 6) {
			Get();
			identifier=toIdentifier(t);
		} else SynErr(160);
		if (la.kind == 120) {
			signature = Signature();
		}
		designator=new Tuple2<Identifier,Option<Signature>>(identifier,toOption(signature)); 
		return designator;
	}

	ConfigurationSpecification  ConfigurationSpecification() {
		ConfigurationSpecification  configSpec;
		Position pos=toPosition(la);
		Expect(37);
		Object componentSpec = ComponentSpecification();
		Object indication = BindingIndication();
		Expect(115);
		configSpec= new ConfigurationSpecification(pos);
		return configSpec;
	}

	Either<Seq<Identifier>,Identifier>  InstantiationList() {
		Either<Seq<Identifier>,Identifier>  list;
		list=null;
		if (la.kind == 1 || la.kind == 2) {
			Seq<Identifier> identifierList = IdentifierList();
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

	void EntityAspect() {
		if (la.kind == 34) {
			Get();
			SelectedName entity_name = SelectedName();
			if (la.kind == 118) {
				Get();
				Identifier architecture_identifier = Identifier();
				Expect(119);
			}
		} else if (la.kind == 27) {
			Get();
			SelectedName configuration_name = SelectedName();
		} else if (la.kind == 64) {
			Get();
		} else SynErr(162);
	}

	AssociationList  GenericMapAspect() {
		AssociationList  associationList;
		Expect(40);
		Expect(54);
		Expect(118);
		associationList = AssociationList();
		Expect(119);
		return associationList;
	}

	AssociationList  PortMapAspect() {
		AssociationList  associationList;
		Expect(69);
		Expect(54);
		Expect(118);
		associationList = AssociationList();
		Expect(119);
		return associationList;
	}

	Seq<SelectedName>  SelectedNameList() {
		Seq<SelectedName>  list;
		ListBuffer<SelectedName> tmpList=new ListBuffer<SelectedName>();
		SelectedName name = SelectedName();
		tmpList.append(name);
		while (la.kind == 116) {
			Get();
			name = SelectedName();
			tmpList.append(name);
		}
		list=tmpList.toList();
		return list;
	}

	at.jku.ssw.openvc.ast.declarations.GroupTemplateDeclaration.Element  EntityClassEntry() {
		at.jku.ssw.openvc.ast.declarations.GroupTemplateDeclaration.Element  entry;
		boolean box=false;
		EntityClass  entityClass = EntityClass();
		if (la.kind == 114) {
			Get();
			box=true;
		}
		entry = new GroupTemplateDeclaration.Element(entityClass,box);
		return entry;
	}

	Seq<Either<Name,Identifier>>  GroupConstituentList() {
		Seq<Either<Name,Identifier>>  list;
		ListBuffer<Either<Name,Identifier>> elements=new ListBuffer<Either<Name,Identifier>>(); 
		Either<Name,Identifier> element=null;
		
		element = GroupConstituent();
		elements.append(element); 
		while (la.kind == 116) {
			Get();
			element = GroupConstituent();
			elements.append(element);
		}
		list=elements.toList();
		return list;
	}

	Either<Name,Identifier>  GroupConstituent() {
		Either<Name,Identifier>  constituent;
		constituent=null;
		if (la.kind == 1 || la.kind == 2 || la.kind == 6) {
			Name name = Name();
			constituent=new Left<Name,Identifier>(name);
		} else if (la.kind == 9) {
			Get();
			constituent=new Right<Name,Identifier>(toIdentifier(t));
		} else SynErr(163);
		return constituent;
	}

	Identifier  EnumerationLiteral() {
		Identifier  identifier;
		identifier=null;
		if (la.kind == 1 || la.kind == 2) {
			identifier = Identifier();
		} else if (la.kind == 9) {
			Get();
			identifier=toIdentifier(t);
		} else SynErr(164);
		return identifier;
	}

	Range  Range() {
		Range  range;
		range=null;
		if (isNotNameInRange()) {
			Expression from = SimpleExpression();
			Direction rangeDirection = Direction();
			Expression to = SimpleExpression();
			range =new Range(from,rangeDirection,to);
		} else if (la.kind == 1 || la.kind == 2 || la.kind == 6) {
			Name name = Name();
			range =new Range(null,null,null,toOption(name));
		} else SynErr(165);
		return range;
	}

	PhysicalLiteral  PhysicalLiteral() {
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
		} else SynErr(166);
		Identifier unit_name = Identifier();
		literal =new PhysicalLiteral(pos,text,unit_name,literalType);
		return literal;
	}

	SelectedName  IndexSubtypeDefinition() {
		SelectedName  typeMark;
		typeMark = TypeMark();
		Expect(75);
		Expect(114);
		return typeMark;
	}

	Seq<DiscreteRange>  IndexConstraint() {
		Seq<DiscreteRange>  ranges;
		ListBuffer<DiscreteRange> list=new ListBuffer<DiscreteRange>();
		DiscreteRange discreteRange=null;
		
		Expect(118);
		discreteRange = DiscreteRange();
		list.append(discreteRange);
		while (la.kind == 116) {
			Get();
			discreteRange = DiscreteRange();
			list.append(discreteRange);
		}
		Expect(119);
		ranges = list.toList();
		return ranges;
	}

	DeclarativeItem  ProtectedTypeDeclarativeItem() {
		DeclarativeItem  item;
		item=null;
		while (!(StartOf(24))) {SynErr(167); Get();}
		if (StartOf(9)) {
			item = SubprogramDeclaration();
		} else if (la.kind == 19) {
			item = AttributeSpecification();
		} else if (la.kind == 100) {
			item = UseClause();
		} else SynErr(168);
		return item;
	}

	DeclarativeItem  ProtectedTypeBodyDeclarativeItem() {
		DeclarativeItem  item;
		item=null;
		while (!(StartOf(19))) {SynErr(169); Get();}
		if (StartOf(9)) {
			item = SubprogramDeclarationOrBody();
		} else if (la.kind == 96) {
			item = TypeDeclaration();
		} else if (la.kind == 92) {
			item = SubtypeDeclaration();
		} else if (la.kind == 28) {
			item = ConstantDeclaration();
		} else if (la.kind == 86 || la.kind == 101) {
			item = VariableDeclaration();
		} else if (la.kind == 36) {
			item = FileDeclaration();
		} else if (la.kind == 13) {
			item = AliasDeclaration();
		} else if (isAttributeDeclaration()) {
			item = AttributeDeclaration();
		} else if (la.kind == 19) {
			item = AttributeSpecification();
		} else if (la.kind == 100) {
			item = UseClause();
		} else if (isGroupTemplate()) {
			item = GroupTemplateDeclaration();
		} else if (la.kind == 41) {
			item = GroupDeclaration();
		} else SynErr(170);
		return item;
	}

	Either<Range,Seq<DiscreteRange>>  Constraint() {
		Either<Range,Seq<DiscreteRange>>  constraint;
		constraint=null;
		if (la.kind == 75) {
			Range rangeContraint = RangeConstraint();
			constraint = new Left<Range,Seq<DiscreteRange>>(rangeContraint);
		} else if (la.kind == 118) {
			Seq<DiscreteRange> ranges = IndexConstraint();
			constraint = new Right<Range,Seq<DiscreteRange>>(ranges);
		} else SynErr(171);
		return constraint;
	}

	Direction  Direction() {
		Direction  rangeDirection;
		rangeDirection=null;
		if (la.kind == 94) {
			Get();
			rangeDirection=Direction.To;
		} else if (la.kind == 30) {
			Get();
			rangeDirection=Direction.Downto;
		} else SynErr(172);
		return rangeDirection;
	}

	Range  RangeConstraint() {
		Range  rangeContraint;
		Expect(75);
		rangeContraint = Range();
		return rangeContraint;
	}

	Expression  SimpleExpression() {
		Expression  simpleExpr;
		Tuple2<Position,Operators.Sign> sign=null;Tuple2<Position,Operators.Add> op=null;
		if (la.kind == 125 || la.kind == 126) {
			sign = Sign();
		}
		simpleExpr = Term();
		if (sign!=null) simpleExpr=new SimpleExpression(sign._1,sign._2,simpleExpr);
		while (la.kind == 117 || la.kind == 125 || la.kind == 126) {
			op = AddingOperator();
			Expression right = Term();
			simpleExpr=new SimpleExpression(op._1,simpleExpr,op._2,right);
		}
		return simpleExpr;
	}

	ConcurrentStatement  ArchitectureStatement() {
		ConcurrentStatement  concurrentStmt;
		concurrentStmt=null; Identifier label=null;
		if (scanner.Peek().kind==_colon) {
			label = LabelColon();
			if (isArchitecutreStatementWithLabel()) {
				concurrentStmt = ArchitectureStatementWithLabel(label);
			} else if (StartOf(12)) {
				concurrentStmt = ArchitectureStatementOptionalLabel(label);
			} else SynErr(173);
		} else if (StartOf(12)) {
			concurrentStmt = ArchitectureStatementOptionalLabel(label);
		} else SynErr(174);
		return concurrentStmt;
	}

	ConcurrentStatement  ArchitectureStatementWithLabel(Identifier label) {
		ConcurrentStatement  concurrentStmt;
		concurrentStmt=null;
		if (StartOf(25)) {
			concurrentStmt = ComponentInstantiationStatement(label);
		} else if (la.kind == 21) {
			concurrentStmt = BlockStatement(label);
		} else if (la.kind == 37 || la.kind == 43) {
			concurrentStmt = GenerateStatement(label);
		} else SynErr(175);
		return concurrentStmt;
	}

	ConcurrentStatement  ArchitectureStatementOptionalLabel(Identifier label) {
		ConcurrentStatement  concurrentStmt;
		concurrentStmt=null;boolean postponed=false;
		if (la.kind == 70) {
			Get();
			postponed=true;
		}
		if (la.kind == 72) {
			concurrentStmt = ProcessStatement(label,postponed);
		} else if (la.kind == 18) {
			concurrentStmt = ConcurrentAssertionStatement(label,postponed);
		} else if (isConcurrentSignalAssignmentStatement()) {
			concurrentStmt = ConcurrentSignalAssignmentStatement(label,postponed);
		} else if (la.kind == 1 || la.kind == 2 || la.kind == 6) {
			concurrentStmt = ConcurrentProcedureCallStatement(label,postponed);
		} else SynErr(176);
		return concurrentStmt;
	}

	ComponentInstantiationStatement  ComponentInstantiationStatement(Identifier label) {
		ComponentInstantiationStatement  stmt;
		ComponentType componentType =null;
		AssociationList genericMap=null,portMap=null;
		Position pos=toPosition(la);
		SelectedName name=null;
		Identifier architecture=null;
		
		if (StartOf(26)) {
			if (la.kind == 26) {
				Get();
			}
			name = SelectedName();
			componentType=ComponentType.COMPONENT;
		} else if (la.kind == 34) {
			Get();
			name = SelectedName();
			if (la.kind == 118) {
				Get();
				architecture = Identifier();
				Expect(119);
			}
			componentType=ComponentType.ENTITY;
		} else if (la.kind == 27) {
			Get();
			name = SelectedName();
			componentType=ComponentType.CONFIGURATION;
		} else SynErr(177);
		if (la.kind == 40) {
			genericMap = GenericMapAspect();
		}
		if (la.kind == 69) {
			portMap = PortMapAspect();
		}
		Expect(115);
		stmt=new ComponentInstantiationStatement(pos,label,componentType,name,toOption(architecture),toOption(genericMap),toOption(portMap));
		return stmt;
	}

	BlockStatement  BlockStatement(Identifier label) {
		BlockStatement  blockStmt;
		Position pos=toPosition(la);
		ListBuffer<DeclarativeItem> declarativeItems=new ListBuffer<DeclarativeItem>(); 
		Expression guard_expression=null;
		InterfaceList genericClause=null,portClause=null;
		AssociationList genericMap=null,portMap=null;
		
		Expect(21);
		if (la.kind == 118) {
			Get();
			guard_expression = Expression();
			Expect(119);
		}
		if (la.kind == 48) {
			Get();
		}
		if (la.kind == 40) {
			genericClause = GenericClause();
			if (la.kind == 40) {
				genericMap = GenericMapAspect();
				Expect(115);
			}
		}
		if (la.kind == 69) {
			portClause = PortClause();
			if (la.kind == 69) {
				portMap = PortMapAspect();
				Expect(115);
			}
		}
		while (StartOf(4)) {
			DeclarativeItem item = BlockDeclarativeItem();
			declarativeItems.append(item);
		}
		Expect(20);
		Seq<ConcurrentStatement> statementList = ArchitectureStatementList();
		Expect(33);
		Expect(21);
		if (la.kind == 1 || la.kind == 2) {
			UnusedIdentifier();
		}
		Expect(115);
		blockStmt=new BlockStatement(pos,toOption(label),toOption(guard_expression),toOption(genericClause),toOption(genericMap),toOption(portClause),toOption(portMap),declarativeItems.toList(),statementList);
		return blockStmt;
	}

	ConcurrentStatement  GenerateStatement(Identifier label) {
		ConcurrentStatement  generateStmt;
		generateStmt=null;
		if (la.kind == 37) {
			generateStmt = ForGenerateStatement(label);
		} else if (la.kind == 43) {
			generateStmt = IfGenerateStatement(label);
		} else SynErr(178);
		return generateStmt;
	}

	ConcurrentSignalAssignmentStatement  ConcurrentSignalAssignmentStatement(Identifier label,boolean postponed) {
		ConcurrentSignalAssignmentStatement  signalAssignmentStatement;
		signalAssignmentStatement=null;
		if (StartOf(27)) {
			signalAssignmentStatement = ConditionalSignalAssignment(label,postponed);
		} else if (la.kind == 105) {
			signalAssignmentStatement = SelectedSignalAssignment(label,postponed);
		} else SynErr(179);
		return signalAssignmentStatement;
	}

	AssociationList  AssociationList() {
		AssociationList  list;
		ListBuffer<AssociationList.Element> elements=new ListBuffer<AssociationList.Element>();
		AssociationList.Element element=null;
		
		element = AssociationElement();
		elements.append(element);
		while (la.kind == 116) {
			Get();
			element = AssociationElement();
			elements.append(element);
		}
		list=new AssociationList(elements.toList());
		return list;
	}

	Seq<Name>  NameList() {
		Seq<Name>  list;
		ListBuffer<Name> tmpList=new ListBuffer<Name>();
		Name name = Name();
		tmpList.append(name);
		while (la.kind == 116) {
			Get();
			name = Name();
			tmpList.append(name);
		}
		list=tmpList.toList();
		return list;
	}

	DeclarativeItem  ProcessDeclarativeItem() {
		DeclarativeItem  item;
		item=null;
		while (!(StartOf(19))) {SynErr(180); Get();}
		if (StartOf(9)) {
			item = SubprogramDeclarationOrBody();
		} else if (la.kind == 96) {
			item = TypeDeclaration();
		} else if (la.kind == 92) {
			item = SubtypeDeclaration();
		} else if (la.kind == 28) {
			item = ConstantDeclaration();
		} else if (la.kind == 86 || la.kind == 101) {
			item = VariableDeclaration();
		} else if (la.kind == 36) {
			item = FileDeclaration();
		} else if (la.kind == 13) {
			item = AliasDeclaration();
		} else if (la.kind == 100) {
			item = UseClause();
		} else if (isAttributeDeclaration()) {
			item = AttributeDeclaration();
		} else if (la.kind == 19) {
			item = AttributeSpecification();
		} else if (isGroupTemplate()) {
			item = GroupTemplateDeclaration();
		} else if (la.kind == 41) {
			item = GroupDeclaration();
		} else SynErr(181);
		return item;
	}

	Expression  Condition() {
		Expression  expr;
		expr = Expression();
		return expr;
	}

	ConcurrentConditionalSignalAssignment  ConditionalSignalAssignment(Identifier label,boolean postponed) {
		ConcurrentConditionalSignalAssignment  signalAssignment;
		ListBuffer<ConcurrentConditionalSignalAssignment.When> elements=new ListBuffer<ConcurrentConditionalSignalAssignment.When>();
		boolean guarded=false;
		DelayMechanism delay=null;
		
		Target target = Target();
		Expect(109);
		Position pos=toPosition(t);
		if (la.kind == 42) {
			Get();
			guarded=true;
		}
		if (la.kind == 46 || la.kind == 78 || la.kind == 95) {
			delay = DelayMechanism();
		}
		ConditionalWaveforms(elements);
		Expect(115);
		signalAssignment=new ConcurrentConditionalSignalAssignment(pos,toOption(label),postponed,target,guarded,toOption(delay),elements.toList());
		return signalAssignment;
	}

	ConcurrentSelectedSignalAssignment  SelectedSignalAssignment(Identifier label,boolean postponed) {
		ConcurrentSelectedSignalAssignment  signalAssignment;
		Position pos=toPosition(la);
		ListBuffer<ConcurrentSelectedSignalAssignment.When> elements=new ListBuffer<ConcurrentSelectedSignalAssignment.When>(); 
		boolean guarded=false;
		ConcurrentSelectedSignalAssignment.When when=null;
		DelayMechanism delay=null;
		
		Expect(105);
		Expression expr = Expression();
		Expect(84);
		Target target = Target();
		Expect(109);
		if (la.kind == 42) {
			Get();
			guarded=true;
		}
		if (la.kind == 46 || la.kind == 78 || la.kind == 95) {
			delay = DelayMechanism();
		}
		when = SelectedWaveform();
		elements.append(when);
		while (la.kind == 116) {
			Get();
			when = SelectedWaveform();
			elements.append(when);
		}
		Expect(115);
		signalAssignment=new ConcurrentSelectedSignalAssignment(pos,toOption(label),postponed,expr,target,guarded,toOption(delay),elements.toList());
		return signalAssignment;
	}

	Target  Target() {
		Target  target;
		target=null;
		if (la.kind == 1 || la.kind == 2 || la.kind == 6) {
			Name name = Name();
			target = new Target(new Left<Name, Aggregate>(name));
		} else if (la.kind == 118) {
			Aggregate aggregate = Aggregate();
			target = new Target(new Right<Name, Aggregate>(aggregate));
		} else SynErr(182);
		return target;
	}

	DelayMechanism  DelayMechanism() {
		DelayMechanism  mechanism;
		Expression time_expression=null;
		if (la.kind == 95) {
			Get();
		} else if (la.kind == 46 || la.kind == 78) {
			if (la.kind == 78) {
				Get();
				time_expression = Expression();
			}
			Expect(46);
		} else SynErr(183);
		if (time_expression==null) mechanism=new DelayMechanism(DelayType.TRANSPORT,toOption(time_expression));
		else mechanism=new DelayMechanism(DelayType.INERTIAL,toOption(time_expression));
		
		return mechanism;
	}

	void ConditionalWaveforms(ListBuffer<ConcurrentConditionalSignalAssignment.When> elements) {
		Expression expr=null;
		Waveform waveform = Waveform();
		if (la.kind == 103) {
			Get();
			expr = Condition();
			if (la.kind == 31) {
				Get();
				ConditionalWaveforms(elements);
			}
		}
		elements.prepend(new ConcurrentConditionalSignalAssignment.When(waveform,expr));
	}

	Waveform  Waveform() {
		Waveform  waveForm;
		ListBuffer<Waveform.Element> elements=new ListBuffer<Waveform.Element>();
		if (StartOf(14)) {
			Waveform.Element element = WaveformElement();
			elements.append(element);
			while (la.kind == 116) {
				Get();
				element = WaveformElement();
				elements.append(element);
			}
		} else if (la.kind == 97) {
			Get();
		} else SynErr(184);
		waveForm=new Waveform(elements.toList());
		return waveForm;
	}

	ConcurrentSelectedSignalAssignment.When  SelectedWaveform() {
		ConcurrentSelectedSignalAssignment.When  whenClause;
		Waveform waveform = Waveform();
		Expect(103);
		Choices choices = Choices();
		whenClause = new ConcurrentSelectedSignalAssignment.When(waveform,choices);
		return whenClause;
	}

	Choices  Choices() {
		Choices  choices;
		ListBuffer<Choices.Choice> elements=new ListBuffer<Choices.Choice>(); 
		Choices.Choice choice = Choice();
		elements.append(choice);
		while (la.kind == 130) {
			Get();
			choice = Choice();
			elements.append(choice);
		}
		choices =new Choices(elements.toList());
		return choices;
	}

	Aggregate  Aggregate() {
		Aggregate  aggregate;
		ListBuffer<Aggregate.ElementAssociation> elements=new ListBuffer<Aggregate.ElementAssociation>(); 
		Expect(118);
		Position pos=toPosition(t);
		Aggregate.ElementAssociation element = ElementAssociation();
		elements.append(element);
		while (la.kind == 116) {
			Get();
			element = ElementAssociation();
			elements.append(element);
		}
		Expect(119);
		aggregate =new Aggregate(pos,elements.toList());
		return aggregate;
	}

	ForGenerateStatement  ForGenerateStatement(Identifier label) {
		ForGenerateStatement  forGenerateStmt;
		Position pos=toPosition(la);Tuple2<Seq<DeclarativeItem>,Seq<ConcurrentStatement>> body=null;
		Expect(37);
		Identifier loopIdentifier = Identifier();
		Expect(45);
		DiscreteRange discreteRange = DiscreteRange();
		Expect(39);
		body = GenerateStatementBody();
		Expect(33);
		Expect(39);
		if (la.kind == 1 || la.kind == 2) {
			UnusedIdentifier();
		}
		Expect(115);
		forGenerateStmt=new ForGenerateStatement(pos,toOption(label),loopIdentifier,discreteRange,body._1,body._2);
		return forGenerateStmt;
	}

	IfGenerateStatement  IfGenerateStatement(Identifier label) {
		IfGenerateStatement  ifGenerateStmt;
		Position pos=toPosition(la);Tuple2<Seq<DeclarativeItem>,Seq<ConcurrentStatement>> body=null;
		Expect(43);
		Expression expr = Condition();
		Expect(39);
		body = GenerateStatementBody();
		Expect(33);
		Expect(39);
		if (la.kind == 1 || la.kind == 2) {
			UnusedIdentifier();
		}
		Expect(115);
		ifGenerateStmt=new IfGenerateStatement(pos,toOption(label),expr,body._1,body._2);
		return ifGenerateStmt;
	}

	Tuple2<Seq<DeclarativeItem>,Seq<ConcurrentStatement>>  GenerateStatementBody() {
		Tuple2<Seq<DeclarativeItem>,Seq<ConcurrentStatement>>  statementList;
		ListBuffer<DeclarativeItem> declarativeItems=new ListBuffer<DeclarativeItem>();
		if (StartOf(28)) {
			while (StartOf(4)) {
				DeclarativeItem item = BlockDeclarativeItem();
				declarativeItems.append(item);
			}
			Expect(20);
		}
		Seq<ConcurrentStatement> concurrentStatements = ArchitectureStatementList();
		statementList=new Tuple2<Seq<DeclarativeItem>,Seq<ConcurrentStatement>>(declarativeItems.toList(),concurrentStatements);
		
		return statementList;
	}

	SequentialStatement  SequentialStatement() {
		SequentialStatement  sequentialStatement;
		sequentialStatement=null;Identifier label=null;
		if (scanner.Peek().kind==_colon) {
			label = LabelColon();
		}
		while (!(StartOf(29))) {SynErr(185); Get();}
		if (la.kind == 102) {
			sequentialStatement = WaitStatement(label);
		} else if (la.kind == 18) {
			sequentialStatement = AssertionStatement(label);
		} else if (la.kind == 80) {
			sequentialStatement = ReportStatement(label);
		} else if (la.kind == 43) {
			sequentialStatement = IfStatement(label);
		} else if (la.kind == 25) {
			sequentialStatement = CaseStatement(label);
		} else if (la.kind == 37 || la.kind == 53 || la.kind == 104) {
			sequentialStatement = LoopStatement(label);
		} else if (la.kind == 58) {
			sequentialStatement = NextStatement(label);
		} else if (la.kind == 35) {
			sequentialStatement = ExitStatement(label);
		} else if (la.kind == 81) {
			sequentialStatement = ReturnStatement(label);
		} else if (la.kind == 61) {
			sequentialStatement = NullStatement(label);
		} else if (isAssignmentStatement()) {
			sequentialStatement = SignalOrVariableAssignmentStatement(label);
		} else if (la.kind == 1 || la.kind == 2 || la.kind == 6) {
			sequentialStatement = ProcedureCallStatement(label);
		} else SynErr(186);
		return sequentialStatement;
	}

	WaitStatement  WaitStatement(Identifier label) {
		WaitStatement  waitStmt;
		Position pos=toPosition(la);Expression untilExpr=null,forExpression=null;Seq<Name> NameList=null;
		Expect(102);
		if (la.kind == 63) {
			Get();
			NameList = NameList();
		}
		if (la.kind == 99) {
			Get();
			untilExpr = Condition();
		}
		if (la.kind == 37) {
			Get();
			forExpression = Expression();
		}
		Expect(115);
		waitStmt=new WaitStatement(pos,toOption(label),toOption(NameList),toOption(untilExpr),toOption(forExpression));
		return waitStmt;
	}

	AssertStatement  AssertionStatement(Identifier label) {
		AssertStatement  assertStmt;
		Position pos=toPosition(la);Expression report_expression=null, severity_expression= null;
		Expect(18);
		Expression expr = Condition();
		if (la.kind == 80) {
			Get();
			report_expression = Expression();
		}
		if (la.kind == 85) {
			Get();
			severity_expression = Expression();
		}
		Expect(115);
		assertStmt=new AssertStatement(pos,toOption(label),expr,toOption(report_expression),toOption(severity_expression));
		return assertStmt;
	}

	ReportStatement  ReportStatement(Identifier label) {
		ReportStatement  reportStmt;
		Position pos=toPosition(la);Expression severity_expression=null;
		Expect(80);
		Expression report_expression = Expression();
		if (la.kind == 85) {
			Get();
			severity_expression = Expression();
		}
		Expect(115);
		reportStmt=new ReportStatement(pos,toOption(label),report_expression,toOption(severity_expression));
		return reportStmt;
	}

	IfStatement  IfStatement(Identifier label) {
		IfStatement  ifStmt;
		Position pos=toPosition(la); 
		ListBuffer<IfStatement.IfThenPart> ifList=new ListBuffer<IfStatement.IfThenPart>(); 
		Seq<SequentialStatement> sequentialStatements = null;
		Seq<SequentialStatement> else_sequential_statement = null;
		
		Expect(43);
		Expression if_condition = Condition();
		Expect(93);
		sequentialStatements = SequentialStatementList();
		ifList.append(new IfStatement.IfThenPart(if_condition,sequentialStatements));
		while (la.kind == 32) {
			Get();
			Expression elsif_condition = Condition();
			Expect(93);
			sequentialStatements = SequentialStatementList();
			ifList.append(new IfStatement.IfThenPart(elsif_condition,sequentialStatements));
		}
		if (la.kind == 31) {
			Get();
			else_sequential_statement = SequentialStatementList();
		}
		Expect(33);
		Expect(43);
		if (la.kind == 1 || la.kind == 2) {
			UnusedIdentifier();
		}
		Expect(115);
		ifStmt=new IfStatement(pos,toOption(label),ifList.toList(),toOption(else_sequential_statement));
		return ifStmt;
	}

	CaseStatement  CaseStatement(Identifier label) {
		CaseStatement  caseStmt;
		Position pos=toPosition(la);
		ListBuffer<CaseStatement.When> alternatives=new ListBuffer<CaseStatement.When>(); 
		
		Expect(25);
		Expression expr = Expression();
		Expect(48);
		Expect(103);
		Choices choices = Choices();
		Expect(111);
		Seq<SequentialStatement> sequentialStatements = SequentialStatementList();
		alternatives.append(new CaseStatement.When(choices,sequentialStatements));
		while (la.kind == 103) {
			Get();
			choices = Choices();
			Expect(111);
			sequentialStatements = SequentialStatementList();
			alternatives.append(new CaseStatement.When(choices,sequentialStatements));
		}
		Expect(33);
		Expect(25);
		if (la.kind == 1 || la.kind == 2) {
			UnusedIdentifier();
		}
		Expect(115);
		caseStmt=new CaseStatement(pos,toOption(label),expr,alternatives.toList());
		return caseStmt;
	}

	SequentialStatement  LoopStatement(Identifier label) {
		SequentialStatement  loopStmt;
		Position pos=toPosition(la);Either<Expression,Tuple2<Identifier,DiscreteRange>> stmtType=null;
		if (la.kind == 37 || la.kind == 104) {
			stmtType = IterationScheme();
		}
		Expect(53);
		Seq<SequentialStatement> sequentialStatements = SequentialStatementList();
		Expect(33);
		Expect(53);
		if (la.kind == 1 || la.kind == 2) {
			UnusedIdentifier();
		}
		Expect(115);
		if (stmtType!=null){
		if (stmtType instanceof Left) loopStmt=new WhileStatement(pos,toOption(label),((Left<Expression,Tuple2<Identifier,DiscreteRange>>)stmtType).a,sequentialStatements);
		else {
			Tuple2<Identifier,DiscreteRange> r=((Right<Expression,Tuple2<Identifier,DiscreteRange>>)stmtType).b;
			loopStmt=new ForStatement(pos,toOption(label),r._1,r._2,sequentialStatements);
		}
		}else loopStmt=new LoopStatement(pos,toOption(label),sequentialStatements);
		
		return loopStmt;
	}

	NextStatement  NextStatement(Identifier label) {
		NextStatement  nextStmt;
		Position pos=toPosition(la);Identifier identifier=null;Expression expr=null;
		Expect(58);
		if (la.kind == 1 || la.kind == 2) {
			identifier = Identifier();
		}
		if (la.kind == 103) {
			Get();
			expr = Condition();
		}
		Expect(115);
		nextStmt=new NextStatement(pos,toOption(label),toOption(identifier),toOption(expr));
		return nextStmt;
	}

	ExitStatement  ExitStatement(Identifier label) {
		ExitStatement  exitStmt;
		Position pos=toPosition(la);Expression expr=null;Identifier identifier=null;
		Expect(35);
		if (la.kind == 1 || la.kind == 2) {
			identifier = Identifier();
		}
		if (la.kind == 103) {
			Get();
			expr = Condition();
		}
		Expect(115);
		exitStmt=new ExitStatement(pos,toOption(label),toOption(identifier),toOption(expr));
		return exitStmt;
	}

	ReturnStatement  ReturnStatement(Identifier label) {
		ReturnStatement  returnStmt;
		Position pos=toPosition(la);Expression expr=null;
		Expect(81);
		if (StartOf(14)) {
			expr = Expression();
		}
		Expect(115);
		returnStmt=new ReturnStatement(pos,toOption(label),toOption(expr));
		return returnStmt;
	}

	NullStatement  NullStatement(Identifier label) {
		NullStatement  nullStmt;
		Position pos=toPosition(la);
		Expect(61);
		Expect(115);
		nullStmt=new NullStatement(pos,toOption(label));
		return nullStmt;
	}

	SequentialStatement  SignalOrVariableAssignmentStatement(Identifier label) {
		SequentialStatement  statement;
		statement=null;
		Target target = Target();
		if (la.kind == 109) {
			statement = SignalAssignmentStatement(label,target);
		} else if (la.kind == 113) {
			statement = VariableAssignmentStatement(label,target);
		} else SynErr(187);
		return statement;
	}

	ProcedureCallStatement  ProcedureCallStatement(Identifier label) {
		ProcedureCallStatement  procedureCallStmt;
		AssociationList paramterList=null;
		SelectedName procedure_name = SelectedName();
		if (la.kind == 118) {
			Get();
			paramterList = AssociationList();
			Expect(119);
		}
		Expect(115);
		procedureCallStmt=new ProcedureCallStatement(toOption(label),procedure_name,toOption(paramterList));
		return procedureCallStmt;
	}

	SignalAssignmentStatement  SignalAssignmentStatement(Identifier label,Target target) {
		SignalAssignmentStatement  signalAssignStmt;
		DelayMechanism delay=null;
		Expect(109);
		Position pos=toPosition(t);
		if (la.kind == 46 || la.kind == 78 || la.kind == 95) {
			delay = DelayMechanism();
		}
		Waveform waveform = Waveform();
		Expect(115);
		signalAssignStmt=new SimpleSignalAssignmentStatement(pos,toOption(label),target,toOption(delay),waveform);
		return signalAssignStmt;
	}

	VariableAssignmentStatement  VariableAssignmentStatement(Identifier label,Target target) {
		VariableAssignmentStatement  varAssignStmt;
		Expect(113);
		Position pos=toPosition(t);
		Expression expr = Expression();
		Expect(115);
		varAssignStmt=new SimpleVariableAssignmentStatement(pos,toOption(label),target,expr);
		return varAssignStmt;
	}

	at.jku.ssw.openvc.ast.Waveform.Element  WaveformElement() {
		at.jku.ssw.openvc.ast.Waveform.Element  element;
		Expression time_expression=null;
		Expression value_expression = Expression();
		if (la.kind == 12) {
			Get();
			time_expression = Expression();
		}
		element= new Waveform.Element(value_expression,toOption(time_expression));
		return element;
	}

	Either<Expression,Tuple2<Identifier,DiscreteRange>>  IterationScheme() {
		Either<Expression,Tuple2<Identifier,DiscreteRange>>  scheme;
		scheme=null;
		if (la.kind == 104) {
			Get();
			Expression expr = Condition();
			scheme=new Left<Expression,Tuple2<Identifier,DiscreteRange>>(expr);
		} else if (la.kind == 37) {
			Get();
			Identifier identifier = Identifier();
			Expect(45);
			DiscreteRange discreteRange = DiscreteRange();
			scheme=new Right<Expression,Tuple2<Identifier,DiscreteRange>>(new Tuple2<Identifier,DiscreteRange>(identifier,discreteRange));
		} else SynErr(188);
		return scheme;
	}

	InterfaceList.AbstractInterfaceElement  InterfaceElementProcedure() {
		InterfaceList.AbstractInterfaceElement  element;
		element=null;
		if (isInterfaceConstantDeclaration()) {
			element = InterfaceConstantDeclaration();
		} else if (isInterfaceVariableDeclaration()) {
			element = InterfaceVariableDeclaration();
		} else if (la.kind == 1 || la.kind == 2 || la.kind == 87) {
			element = InterfaceSignalDeclarationProcedure();
		} else if (la.kind == 36) {
			element = InterfaceFileDeclaration();
		} else SynErr(189);
		return element;
	}

	InterfaceList.InterfaceVariableDeclaration  InterfaceVariableDeclaration() {
		InterfaceList.InterfaceVariableDeclaration  varElement;
		Expression expr=null;InterfaceMode mode=null;
		if (la.kind == 101) {
			Get();
		}
		Seq<Identifier> list = IdentifierList();
		Expect(122);
		if (StartOf(7)) {
			mode = InterfaceMode();
		}
		SubTypeIndication subType = SubtypeIndication();
		if (la.kind == 113) {
			Get();
			expr = Expression();
		}
		varElement=new InterfaceList.InterfaceVariableDeclaration(list,toOption(mode),subType,toOption(expr));
		return varElement;
	}

	InterfaceList.InterfaceFileDeclaration  InterfaceFileDeclaration() {
		InterfaceList.InterfaceFileDeclaration  fileElement;
		Expect(36);
		Seq<Identifier> list = IdentifierList();
		Expect(122);
		SubTypeIndication subType = SubtypeIndication();
		fileElement=new InterfaceList.InterfaceFileDeclaration(list,subType);
		return fileElement;
	}

	InterfaceList.AbstractInterfaceElement  InterfaceElementFunction() {
		InterfaceList.AbstractInterfaceElement  element;
		element=null;
		if (la.kind == 1 || la.kind == 2 || la.kind == 28) {
			element = InterfaceConstantDeclaration();
		} else if (la.kind == 87) {
			element = InterfaceSignalDeclarationFunction();
		} else if (la.kind == 36) {
			element = InterfaceFileDeclaration();
		} else SynErr(190);
		return element;
	}

	InterfaceList.InterfaceSignalDeclaration  InterfaceSignalDeclarationFunction() {
		InterfaceList.InterfaceSignalDeclaration  signalElement;
		Expression expr=null;boolean bus=false;
		Expect(87);
		Seq<Identifier> list = IdentifierList();
		Expect(122);
		if (la.kind == 45) {
			Get();
		}
		SubTypeIndication subType = SubtypeIndication();
		if (la.kind == 24) {
			Get();
			bus=true;
		}
		if (la.kind == 113) {
			Get();
			expr = Expression();
		}
		signalElement=new InterfaceList.InterfaceSignalDeclaration(list,toOption(InterfaceMode.IN),subType,bus,toOption(expr));
		return signalElement;
	}

	InterfaceMode  InterfaceMode() {
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
		} else SynErr(191);
		return mode;
	}

	at.jku.ssw.openvc.ast.AssociationList.Element  AssociationElement() {
		at.jku.ssw.openvc.ast.AssociationList.Element  element;
		Name name=null;Option<Expression> actualPart=null;
		if (isFormalPartInAssociationElement()) {
			name = FormalPart();
			Expect(111);
		}
		actualPart = ActualPart();
		element=new AssociationList.Element(toOption(name),actualPart);
		return element;
	}

	Name  FormalPart() {
		Name  formalPart;
		formalPart = Name();
		return formalPart;
	}

	Option<Expression>  ActualPart() {
		Option<Expression>  actualPart;
		Expression expr=null;
		if (StartOf(14)) {
			expr = Expression();
		} else if (la.kind == 64) {
			Get();
		} else SynErr(192);
		actualPart = toOption(expr);
		return actualPart;
	}

	Expression  Relation() {
		Expression  rel;
		Tuple2<Position,Operators.Relation> op=null;
		rel = ShiftExpression();
		if (StartOf(30)) {
			op = RelationalOperator();
			Expression right = ShiftExpression();
			rel=new Relation(op._1,rel,op._2,right);
		}
		return rel;
	}

	Tuple2<Position,Operators.Logical>  LogicalOperator() {
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
		default: SynErr(193); break;
		}
		op=new Tuple2<Position,Operators.Logical>(pos,logOp);
		return op;
	}

	Expression  ShiftExpression() {
		Expression  shiftExpr;
		Tuple2<Position,Operators.Shift> op=null;
		shiftExpr = SimpleExpression();
		if (StartOf(31)) {
			op = ShiftOperator();
			Expression right = SimpleExpression();
			shiftExpr=new ShiftExpression(op._1,shiftExpr,op._2,right);
		}
		return shiftExpr;
	}

	Tuple2<Position,Operators.Relation>  RelationalOperator() {
		Tuple2<Position,Operators.Relation>  op;
		Position pos=toPosition(la);
		Operators.Relation relOp=null;
		
		switch (la.kind) {
		case 129: {
			Get();
			relOp=Operators.Relation.EQ;
			break;
		}
		case 112: {
			Get();
			relOp=Operators.Relation.NEQ;
			break;
		}
		case 127: {
			Get();
			relOp=Operators.Relation.LT;
			break;
		}
		case 109: {
			Get();
			relOp=Operators.Relation.LEQ;
			break;
		}
		case 128: {
			Get();
			relOp=Operators.Relation.GT;
			break;
		}
		case 110: {
			Get();
			relOp=Operators.Relation.GEQ;
			break;
		}
		default: SynErr(194); break;
		}
		op=new Tuple2<Position,Operators.Relation>(pos,relOp);
		return op;
	}

	Tuple2<Position,Operators.Shift>  ShiftOperator() {
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
		default: SynErr(195); break;
		}
		op=new Tuple2<Position,Operators.Shift>(pos,shiftOp);
		return op;
	}

	Tuple2<Position,Operators.Sign>  Sign() {
		Tuple2<Position,Operators.Sign>  op;
		Position pos=toPosition(la);
		Operators.Sign signOp=null;
		
		if (la.kind == 125) {
			Get();
			signOp=Operators.Sign.PLUS;
		} else if (la.kind == 126) {
			Get();
			signOp=Operators.Sign.MINUS;
		} else SynErr(196);
		op=new Tuple2<Position,Operators.Sign>(pos,signOp);
		return op;
	}

	Expression  Term() {
		Expression  term;
		Tuple2<Position,Operators.Term> op=null;
		term = Factor();
		while (StartOf(32)) {
			op = MultiplyingOperator();
			Expression right = Factor();
			term = new Term(op._1,term,op._2,right);
		}
		return term;
	}

	Tuple2<Position,Operators.Add>  AddingOperator() {
		Tuple2<Position,Operators.Add>  op;
		Position pos=toPosition(la);
		Operators.Add addOp=null;
		
		if (la.kind == 125) {
			Get();
			addOp=Operators.Add.PLUS;
		} else if (la.kind == 126) {
			Get();
			addOp=Operators.Add.MINUS;
		} else if (la.kind == 117) {
			Get();
			addOp=Operators.Add.AMPERSAND;
		} else SynErr(197);
		op=new Tuple2<Position,Operators.Add>(pos,addOp);
		return op;
	}

	Tuple2<Position,Operators.Term>  MultiplyingOperator() {
		Tuple2<Position,Operators.Term>  op;
		Position pos=toPosition(la);
		Operators.Term mulOp=null;
		
		if (la.kind == 123) {
			Get();
			mulOp=Operators.Term.MUL;
		} else if (la.kind == 124) {
			Get();
			mulOp=Operators.Term.DIV;
		} else if (la.kind == 55) {
			Get();
			mulOp=Operators.Term.MOD;
		} else if (la.kind == 79) {
			Get();
			mulOp=Operators.Term.REM;
		} else SynErr(198);
		op=new Tuple2<Position,Operators.Term>(pos,mulOp);
		return op;
	}

	Expression  Factor() {
		Expression  factor;
		factor=null;
		if (StartOf(33)) {
			factor = Primary();
			if (la.kind == 108) {
				Get();
				Position pos=toPosition(t);
				Expression right = Primary();
				factor = new Factor(pos,factor,Operators.Factor.POW,toOption(right));
			}
		} else if (la.kind == 10) {
			Get();
			Position pos=toPosition(t);
			Expression left = Primary();
			factor = new Factor(pos,left,Operators.Factor.ABS);
		} else if (la.kind == 60) {
			Get();
			Position pos=toPosition(t);
			Expression left = Primary();
			factor = new Factor(pos,left,Operators.Factor.NOT);
		} else SynErr(199);
		return factor;
	}

	Expression  Primary() {
		Expression  expr;
		expr=null;
		if (isQualifiedExpression()) {
			SelectedName typeName = SelectedName();
			expr = QualifiedExpression(typeName);
		} else if (la.kind == 1 || la.kind == 2 || la.kind == 6) {
			Name name = Name();
			expr=new NameExpression(name);
		} else if (StartOf(34)) {
			expr = Literal();
		} else if (la.kind == 57) {
			expr = Allocator();
		} else if (la.kind == 118) {
			Aggregate aggregate = Aggregate();
			expr=new AggregateExpression(aggregate);
		} else SynErr(200);
		return expr;
	}

	QualifiedExpression  QualifiedExpression(SelectedName typeName) {
		QualifiedExpression  expr;
		Expect(8);
		Aggregate aggregate = Aggregate();
		expr=new QualifiedExpression(typeName,new AggregateExpression(aggregate));
		return expr;
	}

	Expression  Literal() {
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
		case 9: {
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
		default: SynErr(201); break;
		}
		literal =new Literal(toPosition(t),t.val,literalType);
		if ((t.kind==_integerLiteral || t.kind==_realLiteral) && (la.kind==_basicIdentifier || la.kind==_extendedIdentifier)) {
			Identifier unit = Identifier();
			literal = new PhysicalLiteral((Literal)literal,unit);
		}
		return literal;
	}

	Expression  Allocator() {
		Expression  newExpression;
		Position pos=toPosition(la);newExpression=null;
		Expect(57);
		SelectedName selectedName = SelectedName();
		if (la.kind == 8) {
			Expression expr = QualifiedExpression(selectedName);
			newExpression=new NewExpression(pos,new Left<Expression, SubTypeIndication>(expr));
		} else if (StartOf(35)) {
			if (la.kind == 118) {
				Seq<DiscreteRange> ranges = IndexConstraint();
				Either<Range,Seq<DiscreteRange>> constraint=new Right<Range,Seq<DiscreteRange>>(ranges);
				newExpression=new NewExpression(pos,new Right<Expression, SubTypeIndication>(new SubTypeIndication(selectedName,toOption(constraint))));
				
			}
		} else SynErr(202);
		return newExpression;
	}

	Identifier  NamePrefix() {
		Identifier  identifier;
		identifier=null;
		if (la.kind == 1 || la.kind == 2) {
			identifier = Identifier();
		} else if (la.kind == 6) {
			Get();
			identifier=toIdentifier(t);
		} else SynErr(203);
		return identifier;
	}

	at.jku.ssw.openvc.ast.Name.SelectedPart  NameSelectedPart() {
		at.jku.ssw.openvc.ast.Name.SelectedPart  part;
		part=null;
		Expect(131);
		if (la.kind == 1 || la.kind == 2) {
			Identifier identifier = Identifier();
			part= new Name.SelectedPart(identifier);
		} else if (la.kind == 9) {
			Get();
			part= new Name.SelectedPart(toIdentifier(t));
		} else if (la.kind == 6) {
			Get();
			part= new Name.SelectedPart(toIdentifier(t));
		} else if (la.kind == 14) {
			Get();
			part= new Name.SelectedPart(toIdentifier(t));
		} else SynErr(204);
		return part;
	}

	at.jku.ssw.openvc.ast.Name.Part  NamePart() {
		at.jku.ssw.openvc.ast.Name.Part  part;
		part=null;
		if (la.kind == 131) {
			part = NameSelectedPart();
		} else if (la.kind == 8 || la.kind == 120) {
			part = NameAttributePart();
		} else if (isNameSlicePart()) {
			part = NameSlicePart();
		} else if (la.kind == 118) {
			part = NameAssociationListPart();
		} else SynErr(205);
		return part;
	}

	at.jku.ssw.openvc.ast.Name.AttributePart  NameAttributePart() {
		at.jku.ssw.openvc.ast.Name.AttributePart  part;
		Signature signature=null;Identifier identifier=null;Expression expr=null;
		if (la.kind == 120) {
			signature = Signature();
		}
		Expect(8);
		if (la.kind == 1 || la.kind == 2) {
			identifier = Identifier();
		} else if (la.kind == 75) {
			Get();
			identifier=toIdentifier(t);
		} else SynErr(206);
		if (la.kind == 118) {
			Get();
			expr = Expression();
			Expect(119);
		}
		part=new Name.AttributePart(toOption(signature),identifier,toOption(expr));
		return part;
	}

	at.jku.ssw.openvc.ast.Name.SlicePart  NameSlicePart() {
		at.jku.ssw.openvc.ast.Name.SlicePart  part;
		Expect(118);
		DiscreteRange discreteRange = DiscreteRange();
		Expect(119);
		part=new Name.SlicePart(discreteRange);
		return part;
	}

	at.jku.ssw.openvc.ast.Name.AssociationListPart  NameAssociationListPart() {
		at.jku.ssw.openvc.ast.Name.AssociationListPart  part;
		Expect(118);
		Position pos=toPosition(t);
		AssociationList list = AssociationList();
		Expect(119);
		part=new Name.AssociationListPart(pos,list);
		return part;
	}

	at.jku.ssw.openvc.ast.Aggregate.ElementAssociation  ElementAssociation() {
		at.jku.ssw.openvc.ast.Aggregate.ElementAssociation  element;
		Choices choices=null;
		if (isChoiceInElementAssociation()) {
			choices = Choices();
			Expect(111);
		}
		Expression expr = Expression();
		element=new Aggregate.ElementAssociation(toOption(choices),expr);
		return element;
	}

	at.jku.ssw.openvc.ast.Choices.Choice  Choice() {
		at.jku.ssw.openvc.ast.Choices.Choice  choice;
		choice=null;
		if (isRangeInChoice()) {
			DiscreteRange range = DiscreteRange();
			Either<DiscreteRange, Expression> left=new Left<DiscreteRange, Expression>(range);choice =new Choices.Choice(toOption(left));
		} else if (StartOf(14)) {
			Expression expr = SimpleExpression();
			Either<DiscreteRange, Expression> right=new Right<DiscreteRange, Expression>(expr);choice = new Choices.Choice(toOption(right));
		} else if (la.kind == 66) {
			Get();
			choice =new Choices.Choice();
		} else SynErr(207);
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
		{T,T,T,x, x,x,T,x, x,x,x,x, x,T,x,x, x,x,T,T, x,x,x,x, x,T,T,x, T,T,x,x, x,x,x,T, T,T,T,x, x,T,x,T, T,x,x,x, x,x,x,x, x,T,x,x, x,x,T,x, x,T,x,x, x,x,x,x, x,x,x,T, x,x,T,x, x,x,x,x, T,T,x,x, x,x,T,T, x,x,x,x, T,x,x,x, T,x,x,x, T,T,T,x, T,x,x,x, x,x,x,x, x,x,x,x, x,x,T,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x},
		{x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, T,x,x,x, x,x,x,x, x,x,x,T, x,x,x,x, x,x,T,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,T,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, T,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, T,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x},
		{x,x,x,x, x,x,x,x, x,x,x,x, x,T,x,x, x,x,x,T, x,x,x,x, x,x,x,x, T,T,x,x, x,x,x,x, T,x,T,x, x,T,x,x, T,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,T, x,x,T,x, x,x,x,x, x,x,x,x, x,x,T,T, x,x,x,x, T,x,x,x, T,x,x,x, T,T,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x},
		{x,T,T,x, x,x,T,x, x,x,x,x, x,x,x,x, x,x,T,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,T,x, T,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x},
		{x,x,x,x, x,x,x,x, x,x,x,x, x,T,x,x, x,x,x,T, x,x,x,x, x,x,T,x, T,T,x,x, x,x,x,x, T,T,T,x, x,T,x,x, T,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,T, x,x,T,x, x,x,x,x, x,x,x,x, x,x,T,T, x,x,x,x, T,x,x,x, T,x,x,x, T,T,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x},
		{x,x,x,x, x,x,x,x, x,x,x,x, x,T,x,x, x,x,x,T, x,x,x,x, x,x,x,x, T,x,x,x, x,x,x,x, T,x,T,x, x,T,x,x, T,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,T, x,x,T,x, x,x,x,x, x,x,x,x, x,x,T,T, x,x,x,x, T,x,x,x, T,x,x,x, T,T,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x},
		{x,x,x,x, x,x,x,x, x,x,x,x, x,T,x,x, x,x,x,T, x,x,x,x, x,x,T,x, T,T,x,x, x,x,x,x, T,x,T,x, x,T,x,x, T,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,T, x,x,T,x, x,x,x,x, x,x,x,x, x,x,T,T, x,x,x,x, T,x,x,x, T,x,x,x, T,T,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x},
		{x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,T, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,T,x,T, x,x,x,T, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,T, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x},
		{T,x,x,x, x,x,x,x, x,x,x,x, x,T,x,x, x,x,x,T, x,x,x,x, x,x,x,x, T,T,x,x, x,x,x,x, T,x,T,x, x,T,x,x, T,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,T, x,x,T,x, x,x,x,x, x,x,x,x, x,x,T,T, x,x,x,x, T,x,x,x, T,x,x,x, T,T,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x},
		{x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,T,x, x,x,x,x, T,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,T, x,x,T,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x},
		{x,x,x,x, x,x,x,x, x,x,x,x, x,T,x,x, x,x,x,T, x,x,x,x, x,x,x,x, T,x,x,x, x,x,x,x, T,x,T,x, x,T,x,x, T,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,T, x,x,T,x, x,x,x,x, x,x,x,x, x,x,T,x, x,x,x,x, T,x,x,x, T,x,x,x, T,T,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x},
		{T,x,x,x, x,x,x,x, x,x,x,x, x,T,x,x, x,x,x,T, x,x,x,x, x,x,T,x, T,T,x,x, x,x,x,x, T,T,T,x, x,T,x,x, T,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,T, x,x,T,x, x,x,x,x, x,x,x,x, x,x,T,T, x,x,x,x, T,x,x,x, T,x,x,x, T,T,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x},
		{x,T,T,x, x,x,T,x, x,x,x,x, x,x,x,x, x,x,T,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,T,x, T,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,T,x,x, x,x,x,x, x,x,x,x, x,x,T,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x},
		{T,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,T, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,T,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, T,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x},
		{x,T,T,T, T,T,T,T, x,T,T,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,T,x,x, T,T,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,T,x, x,x,x,x, x,T,T,x, x,x,x,x, x,x},
		{x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,T, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, T,x,x,T, x,x,x,x, x,T,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,T,T, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x},
		{x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, T,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,T,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, T,x,x,x, x,x,x,x, x,x,x,x, x,x,x,T, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x},
		{T,x,x,x, x,x,x,x, x,x,x,x, x,T,x,x, x,x,x,T, x,x,x,x, x,x,T,x, T,T,x,x, x,x,x,x, T,x,T,x, x,T,x,x, T,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,T, x,x,T,x, x,x,x,x, x,x,x,x, x,x,T,T, x,x,x,x, T,x,x,x, T,x,x,x, T,T,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x},
		{T,x,x,x, x,x,x,x, x,x,x,x, x,T,x,x, x,x,x,T, x,x,x,x, x,x,x,x, T,x,x,x, x,x,x,x, T,x,T,x, x,T,x,x, T,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,T, x,x,T,x, x,x,x,x, x,x,x,x, x,x,T,T, x,x,x,x, T,x,x,x, T,x,x,x, T,T,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x},
		{T,x,x,x, x,x,x,x, x,x,x,x, x,T,x,x, x,x,x,T, x,x,x,x, x,x,x,x, T,x,x,x, x,x,x,x, T,x,T,x, x,T,x,x, T,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,T, x,x,T,x, x,x,x,x, x,x,x,x, x,x,T,x, x,x,x,x, T,x,x,x, T,x,x,x, T,T,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x},
		{x,T,T,x, x,x,T,x, x,x,x,x, x,x,x,x, x,x,T,x, x,x,x,x, x,T,x,x, x,x,x,x, x,x,x,T, x,T,x,x, x,x,x,T, x,x,x,x, x,x,x,x, x,T,x,x, x,x,T,x, x,T,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, T,T,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,T,x, T,x,x,x, x,x,x,x, x,x,x,x, x,x,T,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x},
		{x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,T, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,T,x, x,x,x,x, T,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,T, x,x,T,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, T,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x},
		{x,x,x,x, x,x,x,x, T,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,T,x, T,x,x,x, x,x,x,x, x,x,x,T, x,x},
		{x,T,T,x, x,x,T,x, x,T,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x},
		{T,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,T, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,T,x, x,x,x,x, T,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,T, x,x,T,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, T,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x},
		{x,T,T,x, x,x,T,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,T,T, x,x,x,x, x,x,T,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x},
		{x,T,T,x, x,x,T,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,T,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x},
		{x,T,T,x, x,x,T,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,T,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x},
		{x,x,x,x, x,x,x,x, x,x,x,x, x,T,x,x, x,x,x,T, T,x,x,x, x,x,T,x, T,T,x,x, x,x,x,x, T,T,T,x, x,T,x,x, T,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,T, x,x,T,x, x,x,x,x, x,x,x,x, x,x,T,T, x,x,x,x, T,x,x,x, T,x,x,x, T,T,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x},
		{T,T,T,x, x,x,T,x, x,x,x,x, x,x,x,x, x,x,T,x, x,x,x,x, x,T,x,x, x,x,x,x, x,x,x,T, x,T,x,x, x,x,x,T, x,x,x,x, x,x,x,x, x,T,x,x, x,x,T,x, x,T,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, T,T,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,T,x, T,x,x,x, x,x,x,x, x,x,x,x, x,x,T,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x},
		{x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,T,T,x, T,x,x,x, x,x,x,x, x,x,x,x, x,x,x,T, T,T,x,x, x,x},
		{x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,T,T, x,x,x,x, T,T,T,T, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x},
		{x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,T, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,T, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,T, T,x,x,x, x,x,x,x, x,x},
		{x,T,T,T, T,T,T,T, x,T,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,T,x,x, x,T,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,T,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x},
		{x,x,x,T, T,T,x,T, x,T,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,T,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x},
		{x,x,x,x, x,x,x,x, x,x,x,x, T,x,x,T, x,x,x,x, x,x,x,x, T,x,x,x, x,x,T,T, x,x,x,x, x,T,x,T, x,x,x,x, x,x,T,x, T,x,x,x, x,T,x,T, T,x,x,T, x,x,x,x, T,T,x,x, x,x,x,x, x,x,x,x, x,T,x,T, T,x,T,T, T,T,x,x, T,T,T,T, x,T,T,x, x,x,T,x, x,x,x,T, x,x,T,T, T,T,T,T, T,T,x,T, T,T,T,T, x,x,x,T, T,T,T,T, T,T,T,x, x,x}

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
			case 1: s = "basicIdentifier expected"; break;
			case 2: s = "extendedIdentifier expected"; break;
			case 3: s = "basedLiteral expected"; break;
			case 4: s = "integerLiteral expected"; break;
			case 5: s = "realLiteral expected"; break;
			case 6: s = "stringLiteral expected"; break;
			case 7: s = "bitStringLiteral expected"; break;
			case 8: s = "apostrophe expected"; break;
			case 9: s = "characterLiteral expected"; break;
			case 10: s = "abs expected"; break;
			case 11: s = "access expected"; break;
			case 12: s = "after expected"; break;
			case 13: s = "alias expected"; break;
			case 14: s = "all expected"; break;
			case 15: s = "and expected"; break;
			case 16: s = "architecture expected"; break;
			case 17: s = "array expected"; break;
			case 18: s = "assert expected"; break;
			case 19: s = "attribute expected"; break;
			case 20: s = "begin expected"; break;
			case 21: s = "block expected"; break;
			case 22: s = "body expected"; break;
			case 23: s = "buffer expected"; break;
			case 24: s = "bus expected"; break;
			case 25: s = "case expected"; break;
			case 26: s = "component expected"; break;
			case 27: s = "configuration expected"; break;
			case 28: s = "constant expected"; break;
			case 29: s = "disconnect expected"; break;
			case 30: s = "downto expected"; break;
			case 31: s = "else expected"; break;
			case 32: s = "ELSIF expected"; break;
			case 33: s = "end expected"; break;
			case 34: s = "entity expected"; break;
			case 35: s = "exit expected"; break;
			case 36: s = "file expected"; break;
			case 37: s = "for expected"; break;
			case 38: s = "function expected"; break;
			case 39: s = "generate expected"; break;
			case 40: s = "generic expected"; break;
			case 41: s = "group expected"; break;
			case 42: s = "guarded expected"; break;
			case 43: s = "if expected"; break;
			case 44: s = "impure expected"; break;
			case 45: s = "in expected"; break;
			case 46: s = "inertial expected"; break;
			case 47: s = "inout expected"; break;
			case 48: s = "is expected"; break;
			case 49: s = "label expected"; break;
			case 50: s = "library expected"; break;
			case 51: s = "linkage expected"; break;
			case 52: s = "literal expected"; break;
			case 53: s = "loop expected"; break;
			case 54: s = "map expected"; break;
			case 55: s = "mod expected"; break;
			case 56: s = "nand expected"; break;
			case 57: s = "new expected"; break;
			case 58: s = "next expected"; break;
			case 59: s = "nor expected"; break;
			case 60: s = "not expected"; break;
			case 61: s = "null expected"; break;
			case 62: s = "of expected"; break;
			case 63: s = "on expected"; break;
			case 64: s = "open expected"; break;
			case 65: s = "or expected"; break;
			case 66: s = "others expected"; break;
			case 67: s = "outToken expected"; break;
			case 68: s = "package expected"; break;
			case 69: s = "port expected"; break;
			case 70: s = "postponed expected"; break;
			case 71: s = "procedure expected"; break;
			case 72: s = "process expected"; break;
			case 73: s = "protected expected"; break;
			case 74: s = "pure expected"; break;
			case 75: s = "range expected"; break;
			case 76: s = "record expected"; break;
			case 77: s = "register expected"; break;
			case 78: s = "reject expected"; break;
			case 79: s = "rem expected"; break;
			case 80: s = "report expected"; break;
			case 81: s = "return expected"; break;
			case 82: s = "rol expected"; break;
			case 83: s = "ror expected"; break;
			case 84: s = "select expected"; break;
			case 85: s = "severity expected"; break;
			case 86: s = "shared expected"; break;
			case 87: s = "signal expected"; break;
			case 88: s = "sla expected"; break;
			case 89: s = "sll expected"; break;
			case 90: s = "sra expected"; break;
			case 91: s = "srl expected"; break;
			case 92: s = "subtype expected"; break;
			case 93: s = "then expected"; break;
			case 94: s = "to expected"; break;
			case 95: s = "transport expected"; break;
			case 96: s = "type expected"; break;
			case 97: s = "unaffected expected"; break;
			case 98: s = "units expected"; break;
			case 99: s = "until expected"; break;
			case 100: s = "use expected"; break;
			case 101: s = "variable expected"; break;
			case 102: s = "wait expected"; break;
			case 103: s = "when expected"; break;
			case 104: s = "while expected"; break;
			case 105: s = "with expected"; break;
			case 106: s = "xnor expected"; break;
			case 107: s = "xor expected"; break;
			case 108: s = "doublestar expected"; break;
			case 109: s = "leq expected"; break;
			case 110: s = "geq expected"; break;
			case 111: s = "arrow expected"; break;
			case 112: s = "neq expected"; break;
			case 113: s = "varAssign expected"; break;
			case 114: s = "box expected"; break;
			case 115: s = "semicolon expected"; break;
			case 116: s = "comma expected"; break;
			case 117: s = "ampersand expected"; break;
			case 118: s = "lparen expected"; break;
			case 119: s = "rparen expected"; break;
			case 120: s = "lbracket expected"; break;
			case 121: s = "rbracket expected"; break;
			case 122: s = "colon expected"; break;
			case 123: s = "mul expected"; break;
			case 124: s = "div expected"; break;
			case 125: s = "plus expected"; break;
			case 126: s = "minus expected"; break;
			case 127: s = "lt expected"; break;
			case 128: s = "gt expected"; break;
			case 129: s = "eq expected"; break;
			case 130: s = "bar expected"; break;
			case 131: s = "dot expected"; break;
			case 132: s = "??? expected"; break;
			case 133: s = "invalid LibraryUnit"; break;
			case 134: s = "invalid EntityDeclaration"; break;
			case 135: s = "invalid Identifier"; break;
			case 136: s = "this symbol not expected in EntityDeclarativeItem"; break;
			case 137: s = "invalid EntityDeclarativeItem"; break;
			case 138: s = "invalid TypeDeclaration"; break;
			case 139: s = "invalid DisconnectionSpecification"; break;
			case 140: s = "this symbol not expected in BlockDeclarativeItem"; break;
			case 141: s = "invalid BlockDeclarativeItem"; break;
			case 142: s = "this symbol not expected in ConfigurationDeclarativeItem"; break;
			case 143: s = "invalid ConfigurationDeclarativeItem"; break;
			case 144: s = "invalid BlockConfigurationIndex"; break;
			case 145: s = "invalid DiscreteRange"; break;
			case 146: s = "invalid BlockSpecification"; break;
			case 147: s = "this symbol not expected in PackageDeclarativeItem"; break;
			case 148: s = "invalid PackageDeclarativeItem"; break;
			case 149: s = "this symbol not expected in PackageBodyDeclarativeItem"; break;
			case 150: s = "invalid PackageBodyDeclarativeItem"; break;
			case 151: s = "invalid Designator"; break;
			case 152: s = "invalid SubprogramSpecification"; break;
			case 153: s = "this symbol not expected in SubprogramDeclarativeItem"; break;
			case 154: s = "invalid SubprogramDeclarativeItem"; break;
			case 155: s = "invalid TypeDefinition"; break;
			case 156: s = "invalid ArrayTypeDefinition"; break;
			case 157: s = "invalid AliasDesignator"; break;
			case 158: s = "invalid EntityNameList"; break;
			case 159: s = "invalid EntityClass"; break;
			case 160: s = "invalid EntityDesignator"; break;
			case 161: s = "invalid InstantiationList"; break;
			case 162: s = "invalid EntityAspect"; break;
			case 163: s = "invalid GroupConstituent"; break;
			case 164: s = "invalid EnumerationLiteral"; break;
			case 165: s = "invalid Range"; break;
			case 166: s = "invalid PhysicalLiteral"; break;
			case 167: s = "this symbol not expected in ProtectedTypeDeclarativeItem"; break;
			case 168: s = "invalid ProtectedTypeDeclarativeItem"; break;
			case 169: s = "this symbol not expected in ProtectedTypeBodyDeclarativeItem"; break;
			case 170: s = "invalid ProtectedTypeBodyDeclarativeItem"; break;
			case 171: s = "invalid Constraint"; break;
			case 172: s = "invalid Direction"; break;
			case 173: s = "invalid ArchitectureStatement"; break;
			case 174: s = "invalid ArchitectureStatement"; break;
			case 175: s = "invalid ArchitectureStatementWithLabel"; break;
			case 176: s = "invalid ArchitectureStatementOptionalLabel"; break;
			case 177: s = "invalid ComponentInstantiationStatement"; break;
			case 178: s = "invalid GenerateStatement"; break;
			case 179: s = "invalid ConcurrentSignalAssignmentStatement"; break;
			case 180: s = "this symbol not expected in ProcessDeclarativeItem"; break;
			case 181: s = "invalid ProcessDeclarativeItem"; break;
			case 182: s = "invalid Target"; break;
			case 183: s = "invalid DelayMechanism"; break;
			case 184: s = "invalid Waveform"; break;
			case 185: s = "this symbol not expected in SequentialStatement"; break;
			case 186: s = "invalid SequentialStatement"; break;
			case 187: s = "invalid SignalOrVariableAssignmentStatement"; break;
			case 188: s = "invalid IterationScheme"; break;
			case 189: s = "invalid InterfaceElementProcedure"; break;
			case 190: s = "invalid InterfaceElementFunction"; break;
			case 191: s = "invalid InterfaceMode"; break;
			case 192: s = "invalid ActualPart"; break;
			case 193: s = "invalid LogicalOperator"; break;
			case 194: s = "invalid RelationalOperator"; break;
			case 195: s = "invalid ShiftOperator"; break;
			case 196: s = "invalid Sign"; break;
			case 197: s = "invalid AddingOperator"; break;
			case 198: s = "invalid MultiplyingOperator"; break;
			case 199: s = "invalid Factor"; break;
			case 200: s = "invalid Primary"; break;
			case 201: s = "invalid Literal"; break;
			case 202: s = "invalid Allocator"; break;
			case 203: s = "invalid NamePrefix"; break;
			case 204: s = "invalid NameSelectedPart"; break;
			case 205: s = "invalid NamePart"; break;
			case 206: s = "invalid NameAttributePart"; break;
			case 207: s = "invalid Choice"; break;
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

