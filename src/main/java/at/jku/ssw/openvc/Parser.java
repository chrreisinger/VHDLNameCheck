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
	public static final int _INTEGER = 1;
	public static final int _LETTER = 2;
	public static final int _APOSTROPHE = 3;
	public static final int _CHARACTER_LITERAL = 4;
	public static final int _EXTENDED_IDENTIFIER = 5;
	public static final int _BASIC_IDENTIFIER = 6;
	public static final int _STRING_LITERAL = 7;
	public static final int _ABS = 8;
	public static final int _ACCESS = 9;
	public static final int _AFTER = 10;
	public static final int _ALIAS = 11;
	public static final int _ALL = 12;
	public static final int _AND = 13;
	public static final int _ARCHITECTURE = 14;
	public static final int _ARRAY = 15;
	public static final int _ASSERT = 16;
	public static final int _ATTRIBUTE = 17;
	public static final int _BEGIN = 18;
	public static final int _BLOCK = 19;
	public static final int _BODY = 20;
	public static final int _BUFFER = 21;
	public static final int _BUS = 22;
	public static final int _CASE = 23;
	public static final int _COMPONENT = 24;
	public static final int _CONFIGURATION = 25;
	public static final int _CONSTANT = 26;
	public static final int _DISCONNECT = 27;
	public static final int _DOWNTO = 28;
	public static final int _ELSE = 29;
	public static final int _ELSIF = 30;
	public static final int _END_TOKEN = 31;
	public static final int _ENTITY = 32;
	public static final int _EXIT = 33;
	public static final int _FILE = 34;
	public static final int _FOR = 35;
	public static final int _FUNCTION = 36;
	public static final int _GENERATE = 37;
	public static final int _GENERIC = 38;
	public static final int _GROUP = 39;
	public static final int _GUARDED = 40;
	public static final int _IF_TOKEN = 41;
	public static final int _IMPURE = 42;
	public static final int _IN = 43;
	public static final int _INERTIAL = 44;
	public static final int _INOUT = 45;
	public static final int _IS = 46;
	public static final int _LABEL = 47;
	public static final int _LIBRARY = 48;
	public static final int _LINKAGE = 49;
	public static final int _LITERAL = 50;
	public static final int _LOOP = 51;
	public static final int _MAP = 52;
	public static final int _MOD = 53;
	public static final int _NAND = 54;
	public static final int _NEW = 55;
	public static final int _NEXT = 56;
	public static final int _NOR = 57;
	public static final int _NOT = 58;
	public static final int _NULL = 59;
	public static final int _OF = 60;
	public static final int _ON = 61;
	public static final int _OPEN = 62;
	public static final int _OR = 63;
	public static final int _OTHERS = 64;
	public static final int _OUT = 65;
	public static final int _PACKAGE = 66;
	public static final int _PORT = 67;
	public static final int _POSTPONED = 68;
	public static final int _PROCEDURE = 69;
	public static final int _PROCESS = 70;
	public static final int _PROTECTED = 71;
	public static final int _PURE = 72;
	public static final int _RANGE = 73;
	public static final int _RECORD = 74;
	public static final int _REGISTER = 75;
	public static final int _REJECT = 76;
	public static final int _REM = 77;
	public static final int _REPORT = 78;
	public static final int _RETURN = 79;
	public static final int _ROL = 80;
	public static final int _ROR = 81;
	public static final int _SELECT = 82;
	public static final int _SEVERITY = 83;
	public static final int _SHARED = 84;
	public static final int _SIGNAL = 85;
	public static final int _SLA = 86;
	public static final int _SLL = 87;
	public static final int _SRA = 88;
	public static final int _SRL = 89;
	public static final int _SUBTYPE = 90;
	public static final int _THEN = 91;
	public static final int _TO_TOKEN = 92;
	public static final int _TRANSPORT = 93;
	public static final int _TYPE = 94;
	public static final int _UNAFFECTED = 95;
	public static final int _UNITS = 96;
	public static final int _UNTIL = 97;
	public static final int _USE = 98;
	public static final int _VARIABLE = 99;
	public static final int _WAIT = 100;
	public static final int _WHEN = 101;
	public static final int _WHILE = 102;
	public static final int _WITH = 103;
	public static final int _XNOR = 104;
	public static final int _XOR = 105;
	public static final int _DOUBLESTAR = 106;
	public static final int _LEQ = 107;
	public static final int _GEQ = 108;
	public static final int _ARROW = 109;
	public static final int _NEQ = 110;
	public static final int _VAR_ASSIGN = 111;
	public static final int _BOX = 112;
	public static final int _DBLQUOTE = 113;
	public static final int _SEMICOLON = 114;
	public static final int _COMMA = 115;
	public static final int _AMPERSAND = 116;
	public static final int _LPAREN = 117;
	public static final int _RPAREN = 118;
	public static final int _LBRACKET = 119;
	public static final int _RBRACKET = 120;
	public static final int _COLON = 121;
	public static final int _MUL = 122;
	public static final int _DIV = 123;
	public static final int _PLUS = 124;
	public static final int _MINUS = 125;
	public static final int _LT = 126;
	public static final int _GT = 127;
	public static final int _EQ = 128;
	public static final int _BAR = 129;
	public static final int _DOT = 130;
	public static final int maxT = 134;

	static final boolean T = true;
	static final boolean x = false;
	static final int minErrDist = 2;

	public Token t;    // last recognized token
	public Token la;   // lookahead token
	int errDist = minErrDist;
	
	public Scanner scanner;
	public Errors errors;

	boolean IsGroupTemplate() {
	Token next;
	do {
		next = scanner.Peek();
	}while (next.kind!=_IS && next.kind!=_COLON); 
	return next.kind==_IS;
}

  //attribute_declaration: ATTRIBUTE identifier COLON type_mark SEMICOLON 
  //attribute_specification: ATTRIBUTE identifier OF entity_name_list COLON entity_class IS expression SEMICOLON 
  //la == ATTRIBUTE
boolean IsAttributeDeclaration() {
	Token next;
	do {
		next = scanner.Peek();
	}while (next.kind!=_COLON && next.kind!=_OF);
	return next.kind==_COLON;
}

private Position toPosition(Token token){
 return new Position(token.line,token.col);
}    

private Position toIdentifier(Token token){
	return toIdentifier(token,true);
}

private Position toIdentifier(Token token,boolean toLowerCase){
    		if (token.kind!=_STRING_LITERAL && token.kind!=_CHARACTER_LITERAL){
    			return new Identifier(toPosition(token),toLowerCase?token.val.toLowerCase():token.val.replace("\\\\","\\"));   
    		}else{
    			return new Identifier(toPosition(token),token.val);
    		}
}      
/*--------------------------------------------------------------------*/




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
		ListBuffer<DesignUnit> units=new ListBuffer<DesignUnit>(); 
		while (StartOf(1)) {
			designUnit = design_unit();
			units.append(designUnit); 
		}
		designFile=new DesignFile(units.toList());
		return designFile;
	}

	DesignUnit  design_unit() {
		DesignUnit  designUnit;
		ListBuffer<Identifier> libraries=new ListBuffer<Identifier>();
		ListBuffer<UseClause> useClauses=new ListBuffer<UseClause>();
		//val firstToken=la
		
		while (la.kind == 48 || la.kind == 98) {
			if (la.kind == 48) {
				identifierList = library_clause();
				libraries.appendAll(identifierList);
			} else {
				useClause = use_clause();
				useClauses.append(useClause);
			}
		}
		libraryUnit = library_unit();
		designUnit=new DesignUnit(toPosition(firstToken),libraries.toList(),useClauses.toList(),libraryUnit);
		return designUnit;
	}

	Seq<Identifier>  library_clause() {
		Seq<Identifier>  identifierList;
		Expect(48);
		identifierList = identifier_list();
		Expect(114);
		return identifierList;
	}

	UseClause  use_clause() {
		UseClause  useClause;
		Position pos=toPosition(la);
		Expect(98);
		list = selected_name_list();
		Expect(114);
		useClause=new UseClause(pos,list);
		return useClause;
	}

	LibraryUnit  library_unit() {
		LibraryUnit  libraryUnit;
		if (la.kind == 32) {
			libraryUnit=null;
			libraryUnit = entity_declaration();
		} else if (la.kind == 14) {
			libraryUnit = architecture_body();
		} else if (scanner.Peek().kind==_BODY) {
			libraryUnit = package_body();
		} else if (la.kind == 66) {
			libraryUnit = package_declaration();
		} else if (la.kind == 25) {
			libraryUnit = configuration_declaration();
		} else SynErr(135);
		return libraryUnit;
	}

	EntityDeclaration  entity_declaration() {
		EntityDeclaration  entityDecl;
		ListBuffer<DeclarativeItem> declarativeItems=new ListBuffer<DeclarativeItem>();
		ListBuffer<ConcurrentStatement> concurrentStmt=new ListBuffer<ConcurrentStatement>();
		
		Expect(32);
		start_identifier = identifier();
		Expect(46);
		if (la.kind == 38) {
			genericClause = generic_clause();
		}
		if (la.kind == 67) {
			portClause = port_clause();
		}
		while (StartOf(2)) {
			item = entity_declarative_item();
			declarativeItems.append(item); 
		}
		if (la.kind == 18) {
			Get();
		}
		Expect(31);
		if (la.kind == 32) {
			Get();
		}
		if (la.kind == 5 || la.kind == 6) {
			end_identifier = identifier();
		}
		Expect(114);
		entityDecl=new EntityDeclaration(start_identifier,genericClause,portClause,declarativeItems.toList(),concurrentStmt.toList(),end_identifier);
		return entityDecl;
	}

	ArchitectureDeclaration  architecture_body() {
		ArchitectureDeclaration  archDecl;
		ListBuffer<DeclarativeItem> declarativeItems=new ListBuffer<DeclarativeItem>();
		Expect(14);
		start_identifier = identifier();
		Expect(60);
		entityName = selected_name();
		Expect(46);
		while (StartOf(3)) {
			item = block_declarative_item();
			declarativeItems.append(item); 
		}
		Expect(18);
		statementList = architecture_statement_list();
		Expect(31);
		if (la.kind == 14) {
			Get();
		}
		if (la.kind == 5 || la.kind == 6) {
			end_identifier = identifier();
		}
		Expect(114);
		archDecl=new ArchitectureDeclaration(start_identifier,declarativeItems.toList(),entityName,statementList,end_identifier); 
		return archDecl;
	}

	PackageBodyDeclaration  package_body() {
		PackageBodyDeclaration  packageBody;
		ListBuffer<DeclarativeItem> declarativeItems=new ListBuffer<DeclarativeItem>();
		Expect(66);
		Expect(20);
		start_identifier = identifier();
		Expect(46);
		while (StartOf(4)) {
			item = package_body_declarative_item();
			declarativeItems.append(item);
		}
		Expect(31);
		if (la.kind == 66) {
			Get();
			Expect(20);
		}
		if (la.kind == 5 || la.kind == 6) {
			end_identifier = identifier();
		}
		Expect(114);
		packageBody = new PackageBodyDeclaration(start_identifier,declarativeItems.toList(),end_identifier);
		return packageBody;
	}

	PackageDeclaration  package_declaration() {
		PackageDeclaration  packageDecl;
		ListBuffer<DeclarativeItem> declarativeItems=new ListBuffer<DeclarativeItem>(); 
		Expect(66);
		start_identifier = identifier();
		Expect(46);
		while (StartOf(5)) {
			item = package_declarative_item();
			declarativeItems.append(item);
		}
		Expect(31);
		if (la.kind == 66) {
			Get();
		}
		if (la.kind == 5 || la.kind == 6) {
			end_identifier = identifier();
		}
		Expect(114);
		packageDecl=new PackageDeclaration(start_identifier,declarativeItems.toList(),end_identifier);
		return packageDecl;
	}

	ConfigurationDeclaration  configuration_declaration() {
		ConfigurationDeclaration  configDecl;
		ListBuffer<DeclarativeItem> declarativeItems=new ListBuffer<DeclarativeItem>();
		Expect(25);
		start_identifier = identifier();
		Expect(60);
		entityName = selected_name();
		Expect(46);
		while (StartOf(6)) {
			item = configuration_declarative_item();
			declarativeItems.append(item);
		}
		blockConfig = block_configuration();
		Expect(31);
		if (la.kind == 25) {
			Get();
		}
		if (la.kind == 5 || la.kind == 6) {
			end_identifier = identifier();
		}
		Expect(114);
		configDecl=new ConfigurationDeclaration(start_identifier,declarativeItems.toList(),entityName,blockConfig,end_identifier);
		return configDecl;
	}

	Seq<Identifier>  identifier_list() {
		Seq<Identifier>  list;
		ListBuffer<Identifier> tmpList=new ListBuffer<Identifier>();
		Identifier identifier=null;
		
		identifier = identifier();
		tmpList.append(identifier);
		while (la.kind == 115) {
			Get();
			identifier = identifier();
			tmpList.append(identifier);
		}
		list=tmpList.toList();
		return list;
	}

	InterfaceList  generic_clause() {
		InterfaceList  genericList;
		Expect(38);
		Expect(117);
		genericList = generic_interface_list();
		Expect(118);
		Expect(114);
		return genericList;
	}

	InterfaceList  generic_interface_list() {
		InterfaceList  list;
		ListBuffer<InterfaceList.InterfaceConstantDeclaration> elements=new ListBuffer<InterfaceList.InterfaceConstantDeclaration>(); 
		decl = interface_constant_declaration();
		elements.append(decl);
		while (la.kind == 114) {
			Get();
			decl = interface_constant_declaration();
			elements.append(decl); 
		}
		list=new InterfaceList(elements.toList());
		return list;
	}

	InterfaceList.InterfaceConstantDeclaration  interface_constant_declaration() {
		InterfaceList.InterfaceConstantDeclaration  constElement;
		if (la.kind == 26) {
			Get();
		}
		list = identifier_list();
		Expect(121);
		if (la.kind == 43) {
			Get();
		}
		subType = subtype_indication();
		if (la.kind == 111) {
			Get();
			expr = expression();
		}
		constElement=new InterfaceList.InterfaceConstantDeclaration(list,subType,expr);
		return constElement;
	}

	InterfaceList  port_clause() {
		InterfaceList  portList;
		Expect(67);
		Expect(117);
		portList = port_interface_list();
		Expect(118);
		Expect(114);
		return portList;
	}

	InterfaceList  port_interface_list() {
		InterfaceList  list;
		ListBuffer<InterfaceList.InterfaceSignalDeclaration> elements=new ListBuffer<InterfaceList.InterfaceSignalDeclaration>(); 
		decl = interface_signal_declaration_procedure();
		elements.append(decl); 
		while (la.kind == 114) {
			Get();
			decl = interface_signal_declaration_procedure();
			elements.append(decl);
		}
		list=new InterfaceList(elements.toList());
		return list;
	}

	InterfaceList.InterfaceSignalDeclaration  interface_signal_declaration_procedure() {
		InterfaceList.InterfaceSignalDeclaration  signalElement;
		if (la.kind == 85) {
			Get();
		}
		list = identifier_list();
		Expect(121);
		if (StartOf(7)) {
			mode = interface_mode();
		}
		subType = subtype_indication();
		if (la.kind == 22) {
			Get();
		}
		if (la.kind == 111) {
			Get();
			expr = expression();
		}
		signalElement=new InterfaceList.InterfaceSignalDeclaration(list,mode,subType,$BUS!=null,expr);
		return signalElement;
	}

	Identifier  identifier() {
		Identifier  id;
		if (la.kind == 6) {
			Get();
			id=toIdentifier(t);
		} else if (la.kind == 5) {
			Get();
			id=toIdentifier(t,false);
		} else SynErr(136);
		return id;
	}

	DeclarativeItem  entity_declarative_item() {
		DeclarativeItem  item;
		if (StartOf(8)) {
			item=null;
			item = subprogram_declartion_or_body();
		} else if (la.kind == 94) {
			item = type_declaration();
		} else if (la.kind == 90) {
			item = subtype_declaration();
		} else if (la.kind == 26) {
			item = constant_declaration();
		} else if (la.kind == 85) {
			item = signal_declaration();
		} else if (la.kind == 84 || la.kind == 99) {
			item = variable_declaration();
		} else if (la.kind == 34) {
			item = file_declaration();
		} else if (la.kind == 11) {
			item = alias_declaration();
		} else if (IsAttributeDeclaration()) {
			item = attribute_declaration();
		} else if (la.kind == 17) {
			item = attribute_specification();
		} else if (la.kind == 27) {
			item = disconnection_specification();
		} else if (la.kind == 98) {
			item = use_clause();
		} else if (IsGroupTemplate()) {
			item = group_template_declaration();
		} else if (la.kind == 39) {
			item = group_declaration();
		} else SynErr(137);
		return item;
	}

	DeclarativeItem  subprogram_declartion_or_body() {
		DeclarativeItem  declOrBody;
		decl = subprogram_specification();
		if (la.kind == 46) {
			subProgramDef = subprogram_body(decl);
		}
		Expect(114);
		if (subProgramDef!=null) declOrBody=subProgramDef; else declOrBody=decl;
		return declOrBody;
	}

	AbstractTypeDeclaration  type_declaration() {
		AbstractTypeDeclaration  typeDecl;
		Position pos=toPosition(la);
		Expect(94);
		id = identifier();
		if (la.kind == 46) {
			Get();
			typeDef = type_definition(id,pos);
			Expect(114);
			typeDecl=typeDef;
		} else if (la.kind == 114) {
			Get();
			typeDecl=new IncompleteTypeDeclaration(pos,id);
		} else SynErr(138);
		return typeDecl;
	}

	SubTypeDeclaration  subtype_declaration() {
		SubTypeDeclaration  subTypeDecl;
		Position pos=toPosition(la);
		Expect(90);
		identifier = identifier();
		Expect(46);
		subType = subtype_indication();
		Expect(114);
		subTypeDecl=new SubTypeDeclaration(pos,identifier,subType);
		return subTypeDecl;
	}

	ConstantDeclaration  constant_declaration() {
		ConstantDeclaration  constantDecl;
		Position pos=toPosition(la);
		Expect(26);
		list = identifier_list();
		Expect(121);
		subType = subtype_indication();
		if (la.kind == 111) {
			Get();
			expr = expression();
		}
		Expect(114);
		constantDecl=new ConstantDeclaration(pos,list,subType,expr);
		return constantDecl;
	}

	SignalDeclaration  signal_declaration() {
		SignalDeclaration  signalDecl;
		Position pos=toPosition(la);
		Expect(85);
		list = identifier_list();
		Expect(121);
		subType = subtype_indication();
		if (la.kind == 22 || la.kind == 75) {
			if (la.kind == 75) {
				Get();
			} else {
				Get();
			}
		}
		if (la.kind == 111) {
			Get();
			expr = expression();
		}
		Expect(114);
		Option<SignalDeclaration.Type> signalType=null;
		if(reg!=null) signalType=new Some(SignalDeclaration.Type.REGISTER);
		else if (bus!=null) signalType=new Some(SignalDeclaration.Type.BUS);
		else signalType=None;
		signalDecl=new SignalDeclaration(pos,list,subType,signalType,expr);
		
		return signalDecl;
	}

	VariableDeclaration  variable_declaration() {
		VariableDeclaration  varDecl;
		Position pos=toPosition(la);
		if (la.kind == 84) {
			Get();
		}
		Expect(99);
		list = identifier_list();
		Expect(121);
		subType = subtype_indication();
		if (la.kind == 111) {
			Get();
			expr = expression();
		}
		Expect(114);
		varDecl=new VariableDeclaration(pos,$SHARED!=null,list,subType,expr);
		return varDecl;
	}

	FileDeclaration  file_declaration() {
		FileDeclaration  fileDecl;
		Position pos=toPosition(la);
		Expect(34);
		list = identifier_list();
		Expect(121);
		subType = subtype_indication();
		if (la.kind == 46 || la.kind == 62) {
			if (la.kind == 62) {
				Get();
				file_open_kind_expression = expression();
			}
			Expect(46);
			file_logical_name = expression();
		}
		Expect(114);
		fileDecl=new FileDeclaration(pos,list,subType,file_open_kind_expression,file_logical_name);
		return fileDecl;
	}

	AliasDeclaration  alias_declaration() {
		AliasDeclaration  aliasDecl;
		Position pos=toPosition(la);
		Expect(11);
		designator = alias_designator();
		if (la.kind == 121) {
			Get();
			subType = subtype_indication();
		}
		Expect(46);
		name = name();
		if (la.kind == 119) {
			signature = signature();
		}
		Expect(114);
		aliasDecl=new AliasDeclaration(pos,designator,subType,name,signature);
		return aliasDecl;
	}

	AttributeDeclaration  attribute_declaration() {
		AttributeDeclaration  attributeDecl;
		Position pos=toPosition(la);
		Expect(17);
		identifier = identifier();
		Expect(121);
		type = type_mark();
		Expect(114);
		attributeDecl=new AttributeDeclaration(pos,identifier,type);
		return attributeDecl;
	}

	AttributeSpecification  attribute_specification() {
		AttributeSpecification  node;
		Position pos=toPosition(la);
		Expect(17);
		identifier = identifier();
		Expect(60);
		nameList = entity_name_list();
		Expect(121);
		entityClass = entity_class();
		Expect(46);
		expr = expression();
		Expect(114);
		node=new AttributeSpecification(pos,identifier,nameList,entityClass,expr);
		return node;
	}

	DisconnectionSpecification  disconnection_specification() {
		DisconnectionSpecification  disconnectSpec;
		Position pos=toPosition(la);
		Expect(27);
		if (la.kind == 5 || la.kind == 6 || la.kind == 7) {
			list = selected_name_list();
		} else if (la.kind == 64) {
			Get();
		} else if (la.kind == 12) {
			Get();
		} else SynErr(139);
		Expect(121);
		type = type_mark();
		Expect(10);
		expr = expression();
		Expect(114);
		disconnectSpec= new DisconnectionSpecification(pos,id==null?new Left(list):new Right(toIdentifier(id)),type,expr);
		return disconnectSpec;
	}

	GroupTemplateDeclaration  group_template_declaration() {
		GroupTemplateDeclaration  groupTemplateDecl;
		Position pos=toPosition(la);
		ListBuffer<GroupTemplateDeclaration.Element> elements=new ListBuffer<GroupTemplateDeclaration.Element>(); 
		 
		Expect(39);
		identifier = identifier();
		Expect(46);
		Expect(117);
		e1 = entity_class_entry();
		elements.append(e1);
		while (la.kind == 115) {
			Get();
			e2 = entity_class_entry();
			elements.append(e2);
		}
		Expect(118);
		Expect(114);
		groupTemplateDecl=new GroupTemplateDeclaration(pos,identifier,elements.toList());
		return groupTemplateDecl;
	}

	GroupDeclaration  group_declaration() {
		GroupDeclaration  groupDecl;
		Position pos=toPosition(la);
		Expect(39);
		identifier = identifier();
		Expect(121);
		selectedName = selected_name();
		Expect(117);
		list = group_constituent_list();
		Expect(118);
		Expect(114);
		groupDecl=new GroupDeclaration(pos,identifier,selectedName,list);
		return groupDecl;
	}

	SelectedName  selected_name() {
		SelectedName  name;
		ListBuffer<Identifier> parts=new ListBuffer<Identifier>();
		Identifier prefix = name_prefix();
		while (la.kind == 130) {
			selectedPart = name_selected_part();
			parts.append(selectedPart);
		}
		prepend.prepend(prefix); name =new SelectedName(parts.toList());
		return name;
	}

	DeclarativeItem  block_declarative_item() {
		DeclarativeItem  item;
		if (StartOf(8)) {
			item=null;
			item = subprogram_declartion_or_body();
		} else if (la.kind == 94) {
			item = type_declaration();
		} else if (la.kind == 90) {
			item = subtype_declaration();
		} else if (la.kind == 26) {
			item = constant_declaration();
		} else if (la.kind == 85) {
			item = signal_declaration();
		} else if (la.kind == 84 || la.kind == 99) {
			item = variable_declaration();
		} else if (la.kind == 34) {
			item = file_declaration();
		} else if (la.kind == 11) {
			item = alias_declaration();
		} else if (la.kind == 24) {
			item = component_declaration();
		} else if (IsAttributeDeclaration()) {
			item = attribute_declaration();
		} else if (la.kind == 17) {
			item = attribute_specification();
		} else if (la.kind == 35) {
			item = configuration_specification();
		} else if (la.kind == 27) {
			item = disconnection_specification();
		} else if (la.kind == 98) {
			item = use_clause();
		} else if (IsGroupTemplate()) {
			item = group_template_declaration();
		} else if (la.kind == 39) {
			item = group_declaration();
		} else SynErr(140);
		return item;
	}

	Seq<ConcurrentStatement>  architecture_statement_list() {
		Seq<ConcurrentStatement>  list;
		ListBuffer<ConcurrentStatement> statementList=new ListBuffer<ConcurrentStatement>();
		
		while (StartOf(9)) {
			ConcurrentStatement stmt = architecture_statement();
			statementList.append(stmt);
		}
		list=statementList.toList();
		return list;
	}

	DeclarativeItem  configuration_declarative_item() {
		DeclarativeItem  item;
		if (StartOf(6)) {
			item=null;
		} else if (la.kind == 98) {
			item = use_clause();
		} else if (la.kind == 39) {
			item = group_declaration();
		} else if (la.kind == 17) {
			item = attribute_specification();
		} else SynErr(141);
		return item;
	}

	BlockConfiguration  block_configuration() {
		BlockConfiguration  blockConfig;
		ListBuffer<UseClause> useClauses=new ListBuffer<UseClause>();
		ListBuffer<Object> configurations=new ListBuffer<Object>();
		
		Expect(35);
		block_specification();
		while (la.kind == 98) {
			uUseClause seClause = use_clause();
			useClauses.append(useClause);
		}
		while (la.kind == 35) {
			if (la.kind == 35) {
				blockConfiguration = block_configuration();
				configurations.append(blockConfiguration);
			} else {
				componentConfiguration = component_configuration();
				configurations.append(componentConfiguration);
			}
		}
		Expect(31);
		Expect(35);
		Expect(114);
		blockConfig=new BlockConfiguration(blockConfiguration,useClauses.toList(),configurations.toList());
		return blockConfig;
	}

	void block_specification() {
		Expect(131);
	}

	ComponentConfiguration  component_configuration() {
		ComponentConfiguration  componentConfig;
		Expect(35);
		componentSpec = component_specification();
		if (StartOf(10)) {
			indication = binding_indication();
			Expect(114);
		}
		if (la.kind == 35) {
			blockConfiguration = block_configuration();
		}
		Expect(31);
		Expect(35);
		Expect(114);
		componentConfig=new ComponentConfiguration(componentSpec,indication,blockConfiguration);
		return componentConfig;
	}

	Object  component_specification() {
		Object  spec;
		list = instantiation_list();
		Expect(121);
		name = selected_name();
		return spec;
	}

	Object  binding_indication() {
		Object  indication;
		if (la.kind == 98) {
			Get();
			entity_aspect();
		}
		if (la.kind == 38) {
			genericMap = generic_map_aspect();
		}
		if (la.kind == 67) {
			portMap = port_map_aspect();
		}
		return indication;
	}

	DeclarativeItem  package_declarative_item() {
		DeclarativeItem  item;
		if (StartOf(8)) {
			item=null;
			item = subprogram_declartion_or_body();
		} else if (la.kind == 94) {
			item = type_declaration();
		} else if (la.kind == 90) {
			item = subtype_declaration();
		} else if (la.kind == 26) {
			item = constant_declaration();
		} else if (la.kind == 85) {
			item = signal_declaration();
		} else if (la.kind == 84 || la.kind == 99) {
			item = variable_declaration();
		} else if (la.kind == 34) {
			item = file_declaration();
		} else if (la.kind == 11) {
			item = alias_declaration();
		} else if (la.kind == 24) {
			item = component_declaration();
		} else if (IsAttributeDeclaration()) {
			item = attribute_declaration();
		} else if (la.kind == 17) {
			item = attribute_specification();
		} else if (la.kind == 27) {
			item = disconnection_specification();
		} else if (la.kind == 98) {
			item = use_clause();
		} else if (IsGroupTemplate()) {
			item = group_template_declaration();
		} else if (la.kind == 39) {
			item = group_declaration();
		} else SynErr(142);
		return item;
	}

	ComponentDeclaration  component_declaration() {
		ComponentDeclaration  componentDecl;
		Position pos=toPosition(la);
		Expect(24);
		start_identifier = identifier();
		if (la.kind == 46) {
			Get();
		}
		gernicClause = generic_clause();
		portClause = port_clause();
		Expect(31);
		Expect(24);
		if (la.kind == 5 || la.kind == 6) {
			end_identifier = identifier();
		}
		Expect(114);
		componentDecl=new ComponentDeclaration(pos,start_identifier,gernicClause,portClause,end_identifier);
		return componentDecl;
	}

	DeclarativeItem  package_body_declarative_item() {
		DeclarativeItem  item;
		if (StartOf(8)) {
			item=null;
			item = subprogram_declartion_or_body();
		} else if (la.kind == 94) {
			item = type_declaration();
		} else if (la.kind == 90) {
			item = subtype_declaration();
		} else if (la.kind == 26) {
			item = constant_declaration();
		} else if (la.kind == 85) {
			item = signal_declaration();
		} else if (la.kind == 84 || la.kind == 99) {
			item = variable_declaration();
		} else if (la.kind == 34) {
			item = file_declaration();
		} else if (la.kind == 11) {
			item = alias_declaration();
		} else if (la.kind == 98) {
			item = use_clause();
		} else if (la.kind == 17) {
			item = attribute_specification();
		} else if (IsGroupTemplate()) {
			item = group_template_declaration();
		} else if (la.kind == 39) {
			item = group_declaration();
		} else SynErr(143);
		return item;
	}

	Identifier  designator() {
		Identifier  id;
		if (la.kind == 5 || la.kind == 6) {
			identifier = identifier();
			id=identifier;
		} else if (la.kind == 7) {
			Get();
			id=toIdentifier(t);
		} else SynErr(144);
		return id;
	}

	SubProgramDeclaration  subprogram_specification() {
		SubProgramDeclaration  decl;
		Position pos=toPosition(la);
		if (la.kind == 69) {
			Get();
			designator = designator();
			if (la.kind == 117) {
				Get();
				list = parameter_interface_list_procedure();
				Expect(118);
			}
			decl=new ProcedureDeclaration(pos,designator,list);
		} else if (la.kind == 36 || la.kind == 42 || la.kind == 72) {
			if (la.kind == 42 || la.kind == 72) {
				if (la.kind == 72) {
					Get();
				} else {
					Get();
				}
			}
			Expect(36);
			designator = designator();
			if (la.kind == 117) {
				Get();
				list = parameter_interface_list_function();
				Expect(118);
			}
			Expect(79);
			returnType = type_mark();
			decl=new FunctionDeclaration(pos,i==null,designator,list,returnType);
		} else SynErr(145);
		return decl;
	}

	InterfaceList  parameter_interface_list_procedure() {
		InterfaceList  list;
		ListBuffer<InterfaceList.AbstractInterfaceElement> elements=new ListBuffer<InterfaceList.AbstractInterfaceElement>();
		InterfaceList.AbstractInterfaceElement element=null;
		
		element = interface_element_procedure();
		elements.append(element);
		while (la.kind == 114) {
			Get();
			element = interface_element_procedure();
			elements.append(element);
		}
		list=new InterfaceList(elements.toList());
		return list;
	}

	InterfaceList  parameter_interface_list_function() {
		InterfaceList  list;
		ListBuffer<InterfaceList.AbstractInterfaceElement> elements=new ListBuffer<InterfaceList.AbstractInterfaceElement>();
		InterfaceList.AbstractInterfaceElement element=null;
		
		element = interface_element_function();
		elements.append(element);
		while (la.kind == 114) {
			Get();
			e2 = interface_element_function();
			elements.append(elements);
		}
		list=new InterfaceList(elements.toList());
		return list;
	}

	SelectedName  type_mark() {
		SelectedName  typeName;
		name = selected_name();
		typeName=name;
		return typeName;
	}

	SubProgramDefinition  subprogram_body(SubProgramDeclaration subprogramDecl) {
		SubProgramDefinition  subProgramDef;
		ListBuffer<DeclarativeItem> declarativeItems=new ListBuffer<DeclarativeItem>(); 
		Expect(46);
		while (StartOf(11)) {
			item = subprogram_declarative_item();
			declItems.append(item); 
		}
		Expect(18);
		sequentialStatements = sequential_statement_list();
		Expect(31);
		if (la.kind == 36 || la.kind == 69) {
			if (la.kind == 69) {
				Get();
			} else {
				Get();
			}
		}
		if (la.kind == 5 || la.kind == 6 || la.kind == 7) {
			endDesignator = designator();
		}
		if (subprogramDecl instanceof ProcedureDeclaration){
		ProcedureDeclaration procDecl = (ProcedureDeclaration)subprogramDecl;
		subProgramDef=new ProcedureDefinition(subprogramDecl.position,procDecl.identifier,procDecl.parameterInterfaceList,declItems.toList(),sequentialStatements,endIdent);
		}else {
			FunctionDeclaration funcDecl=(FunctionDeclaration)subprogramDecl;
			subProgramDef=new FunctionDefinition(subprogramDecl.position,funcDecl.pure,funcDecl.identifier,funcDecl.parameterInterfaceList,funcDecl.returnType,declItems.toList(),sequentialStatements,endIdent);
		}
		
		return subProgramDef;
	}

	DeclarativeItem  subprogram_declaration() {
		DeclarativeItem  subprogramDecl;
		decl = subprogram_specification();
		Expect(114);
		subprogramDecl=decl;
		return subprogramDecl;
	}

	DeclarativeItem  subprogram_declarative_item() {
		DeclarativeItem  item;
		if (StartOf(8)) {
			item=null;
			item = subprogram_declartion_or_body();
		} else if (la.kind == 94) {
			item = type_declaration();
		} else if (la.kind == 90) {
			item = subtype_declaration();
		} else if (la.kind == 26) {
			item = constant_declaration();
		} else if (la.kind == 84 || la.kind == 99) {
			item = variable_declaration();
		} else if (la.kind == 34) {
			item = file_declaration();
		} else if (la.kind == 11) {
			item = alias_declaration();
		} else if (IsAttributeDeclaration()) {
			item = attribute_declaration();
		} else if (la.kind == 17) {
			item = attribute_specification();
		} else if (la.kind == 98) {
			item = use_clause();
		} else if (IsGroupTemplate()) {
			item = group_template_declaration();
		} else if (la.kind == 39) {
			item = group_declaration();
		} else SynErr(146);
		return item;
	}

	Seq<SequentialStatement>  sequential_statement_list() {
		Seq<SequentialStatement>  list;
		ListBuffer<SequentialStatement> tmpList=new ListBuffer<SequentialStatement>();
		
		while (StartOf(12)) {
			SequentialStatement stmt = sequential_statement();
			tmpList.append(stmt);
		}
		list=tmpList.toList();
		return list;
	}

	AbstractTypeDeclaration  type_definition(Identifier id,Position pos) {
		AbstractTypeDeclaration  typeDef;
		if (la.kind == 117) {
			typeDef=null;
			typeDef = enumeration_type_definition(id,pos);
		} else if (la.kind == 73) {
			typeDef = integer_or_floating_point_type_definition(id,pos);
		} else if (la.kind == 15) {
			typeDef = array_type_definition(id,pos);
		} else if (la.kind == 74) {
			typeDef = record_type_definition(id,pos);
		} else if (la.kind == 9) {
			typeDef = access_type_definition(id,pos);
		} else if (la.kind == 34) {
			typeDef = file_type_definition(id,pos);
		} else if (scanner.Peek().kind==_BODY) {
			typeDef = protected_type_body(id,pos);
		} else if (la.kind == 71) {
			typeDef = protected_type_declaration(id,pos);
		} else SynErr(147);
		return typeDef;
	}

	EnumerationTypeDefinition  enumeration_type_definition(Identifier id,Position pos) {
		EnumerationTypeDefinition  enumTypeDef;
		ListBuffer<Identifier> elements=new ListBuffer<Identifier>(); 
		Identifier element=null;
		
		Expect(117);
		element = enumeration_literal();
		elements.append(element);
		while (la.kind == 115) {
			Get();
			element = enumeration_literal();
			elements.append(element);
		}
		Expect(118);
		enumTypeDef=new EnumerationTypeDefinition(pos,id,elements.toList());
		return enumTypeDef;
	}

	IntegerOrFloatingPointTypeDefinition  integer_or_floating_point_type_definition(Identifier id,Position pos) {
		IntegerOrFloatingPointTypeDefinition  intOrFloat;
		Expect(73);
		range = range();
		intOrFloat=new IntegerOrFloatingPointTypeDefinition(pos,id,range);
		return intOrFloat;
	}

	AbstractArrayTypeDefinition  array_type_definition(Identifier id,Position pos) {
		AbstractArrayTypeDefinition  arrayTypeDef;
		ListBuffer<SelectedName> unConstraintList=new ListBuffer<SelectedName>(); 
		Expect(15);
		if (unConstraintList.isEmpty()) arrayTypeDef=new ConstrainedArrayTypeDefinition(pos,id,$index_constraint.ranges,$subType.subType);
		else arrayTypeDef=new UnconstrainedArrayTypeDefinition(pos,id,unConstraintList.toList(),$subType.subType);
		
		return arrayTypeDef;
	}

	RecordTypeDefinition  record_type_definition(Identifier id,Position pos) {
		RecordTypeDefinition  recordTypeDef;
		ListBuffer<RecordTypeDefinition.Element> elements=new ListBuffer<RecordTypeDefinition.Element>(); 
		Expect(74);
		while (la.kind == 5 || la.kind == 6) {
			list = identifier_list();
			Expect(121);
			subType = subtype_indication();
			Expect(114);
			elements.append(new RecordTypeDefinition.Element(list, subType));
		}
		Expect(31);
		Expect(74);
		if (la.kind == 5 || la.kind == 6) {
			endIdentifier = identifier();
		}
		recordTypeDef=new RecordTypeDefinition(pos,id,elements.toList(),endIdentifier);
		return recordTypeDef;
	}

	AccessTypeDefinition  access_type_definition(Identifier id,Position pos) {
		AccessTypeDefinition  accessTypeDef;
		Expect(9);
		SubTypeIndication subType = subtype_indication();
		accessTypeDef=new AccessTypeDefinition(pos,id,subType);
		return accessTypeDef;
	}

	FileTypeDefinition  file_type_definition(Identifier id,Position pos) {
		FileTypeDefinition  fileTypeDef;
		Expect(34);
		Expect(60);
		SelectedName type = type_mark();
		fileTypeDef=new FileTypeDefinition(pos,id,type);
		return fileTypeDef;
	}

	ProtectedTypeBodyDeclaration  protected_type_body(Identifier id,Position pos) {
		ProtectedTypeBodyDeclaration  protectedTypeBody;
		ListBuffer<DeclarativeItem> items=new ListBuffer<DeclarativeItem>(); 
		Expect(71);
		Expect(20);
		while (StartOf(11)) {
			item = protected_type_body_declarative_item();
			items.append(item);
		}
		Expect(31);
		Expect(71);
		Expect(20);
		if (la.kind == 5 || la.kind == 6) {
			endIdentifier = identifier();
		}
		protectedTypeBody=new ProtectedTypeBodyDeclaration(pos,id,items.toList(),endIdentifier);
		return protectedTypeBody;
	}

	ProtectedTypeDeclaration  protected_type_declaration(Identifier id,Position pos) {
		ProtectedTypeDeclaration  protectedTypeDecl;
		ListBuffer<DeclarativeItem> items=new ListBuffer<DeclarativeItem>(); 
		Expect(71);
		while (StartOf(13)) {
			item = protected_type_declarative_item();
			items.append(item);
		}
		Expect(31);
		Expect(71);
		if (la.kind == 5 || la.kind == 6) {
			endIdentifier = identifier();
		}
		protectedTypeDecl=new ProtectedTypeDeclaration(pos,id,items.toList(),endIdentifier);
		return protectedTypeDecl;
	}

	SubTypeIndication  subtype_indication() {
		SubTypeIndication  subType;
		n1 = selected_name();
		if (la.kind == 5 || la.kind == 6 || la.kind == 7) {
			n2 = selected_name();
		}
		if (la.kind == 73 || la.kind == 117) {
			constraint = constraint();
		}
		if (n2!=null) subType=new SubTypeIndication(n1,n2,constraint);
		else subType=new SubTypeIndication(None,n1,constraint);
		
		return subType;
	}

	Expression  expression() {
		Expression  expr;
		expr = relation();
		while (StartOf(14)) {
			op = logical_operator();
			Expression right = relation();
			expr=new LogicalExpression(op._1,expr,op._2,right);
		}
		return expr;
	}

	Identifier  alias_designator() {
		Identifier  identifier;
		if (la.kind == 5 || la.kind == 6) {
			identifier = identifier();
		} else if (la.kind == 4) {
			Get();
			identifier=toIdentifier(t);
		} else if (la.kind == 7) {
			Get();
			identifier=toIdentifier(t);
		} else SynErr(148);
		return identifier;
	}

	Name  name() {
		Name  name;
		ListBuffer<Name.Part> parts=new ListBuffer<Name.Part>();
		Identifier prefix = name_prefix();
		name =new Name(prefix,parts.toList());
		return name;
	}

	Signature  signature() {
		Signature  signature;
		Expect(119);
		if (la.kind == 5 || la.kind == 6 || la.kind == 7) {
			list = selected_name_list();
		}
		if (la.kind == 79) {
			Get();
			returnType = type_mark();
		}
		Expect(120);
		signature =new Signature(list,returnType);
		return signature;
	}

	Either<Seq<Tuple2<Identifier,Option<Signature>>>,Identifier>  entity_name_list() {
		Either<Seq<Tuple2<Identifier,Option<Signature>>>,Identifier>  list;
		if (StartOf(15)) {
			ListBuffer<Tuple2<Identifier,Option<Signature>>> elements=new ListBuffer<Tuple2<Identifier,Option<Signature>>>(); 
			designator = entity_designator();
			elements.append(designator); 
			while (la.kind == 115) {
				Get();
				designator = entity_designator();
				elements.append(designator);
			}
			list=new Left(elements.toList());
		} else if (la.kind == 64) {
			Get();
			list=new Right(toIdentifier(t));
		} else if (la.kind == 12) {
			Get();
			list=new Right(toIdentifier(t));
		} else SynErr(149);
		return list;
	}

	EntityClass.Value  entity_class() {
		EntityClass.Value  entityClass;
		switch (la.kind) {
		case 32: {
			Get();
			entityClass=EntityClass.ENTITY;
			break;
		}
		case 14: {
			Get();
			entityClass=EntityClass.ARCHITECTURE;
			break;
		}
		case 25: {
			Get();
			entityClass=EntityClass.CONFIGURATION;
			break;
		}
		case 66: {
			Get();
			entityClass=EntityClass.PACKAGE;
			break;
		}
		case 69: {
			Get();
			entityClass=EntityClass.PROCEDURE;
			break;
		}
		case 36: {
			Get();
			entityClass=EntityClass.FUNCTION;
			break;
		}
		case 94: {
			Get();
			entityClass=EntityClass.TYPE;
			break;
		}
		case 90: {
			Get();
			entityClass=EntityClass.SUBTYPE;
			break;
		}
		case 26: {
			Get();
			entityClass=EntityClass.CONSTANT;
			break;
		}
		case 85: {
			Get();
			entityClass=EntityClass.SIGNAL;
			break;
		}
		case 99: {
			Get();
			entityClass=EntityClass.VARIABLE;
			break;
		}
		case 34: {
			Get();
			entityClass=EntityClass.FILE;
			break;
		}
		case 24: {
			Get();
			entityClass=EntityClass.COMPONENT;
			break;
		}
		case 47: {
			Get();
			entityClass=EntityClass.LABEL;
			break;
		}
		case 50: {
			Get();
			entityClass=EntityClass.LITERAL;
			break;
		}
		case 96: {
			Get();
			entityClass=EntityClass.UNITS;
			break;
		}
		case 39: {
			Get();
			entityClass=EntityClass.GROUP;
			break;
		}
		default: SynErr(150); break;
		}
		return entityClass;
	}

	Tuple2<Identifier,Option<Signature>>  entity_designator() {
		Tuple2<Identifier,Option<Signature>>  designator;
		Identifier id=null; 
		if (la.kind == 5 || la.kind == 6) {
			identifier = identifier();
			id=identifier;
		} else if (la.kind == 4) {
			Get();
			id=toIdentifier(t);
		} else if (la.kind == 7) {
			Get();
			id=toIdentifier(t);
		} else SynErr(151);
		if (la.kind == 119) {
			sig = signature();
		}
		designator=new Tuple2<Identifier,Option<Signature>>(id,new Option<Signature>(sig)); 
		return designator;
	}

	ConfigurationSpecification  configuration_specification() {
		ConfigurationSpecification  configSpec;
		Position pos=toPosition(la);
		Expect(35);
		componentSpec = component_specification();
		indication = binding_indication();
		Expect(114);
		configSpec= new ConfigurationSpecification(pos);
		return configSpec;
	}

	Either<Seq<Identifier>,Identifier>  instantiation_list() {
		Either<Seq<Identifier>,Identifier>  list;
		if (la.kind == 5 || la.kind == 6) {
			list = identifier_list();
			list=new Left(list);
		} else if (la.kind == 64) {
			Get();
			list=new Right(toIdentifier(t));
		} else if (la.kind == 12) {
			Get();
			list=new Right(toIdentifier(t));
		} else SynErr(152);
		return list;
	}

	void entity_aspect() {
		if (la.kind == 32) {
			Get();
			entity_name = selected_name();
			if (la.kind == 117) {
				Get();
				architecture_identifier = identifier();
				Expect(118);
			}
		} else if (la.kind == 25) {
			Get();
			configuration_name = selected_name();
		} else if (la.kind == 62) {
			Get();
		} else SynErr(153);
	}

	AssociationList  generic_map_aspect() {
		AssociationList  associationList;
		Expect(38);
		Expect(52);
		Expect(117);
		associationList = association_list();
		Expect(118);
		return associationList;
	}

	AssociationList  port_map_aspect() {
		AssociationList  associationList;
		Expect(67);
		Expect(52);
		Expect(117);
		associationList = association_list();
		Expect(118);
		return associationList;
	}

	Seq<SelectedName>  selected_name_list() {
		Seq<SelectedName>  list;
		ListBuffer<SelectedName> tmpList=new ListBuffer<SelectedName>();
		SelectedName name;
		
		name = selected_name();
		tmpList.append(name);
		while (la.kind == 115) {
			Get();
			name = selected_name();
			tmpList.append(name);
		}
		list=tmpList.toList();
		return list;
	}

	GroupTemplateDeclaration.Element  entity_class_entry() {
		GroupTemplateDeclaration.Element  entry;
		entityClass = entity_class();
		if (la.kind == 112) {
			Get();
		}
		entry = new GroupTemplateDeclaration.Element(entityClass,$BOX!=null);
		return entry;
	}

	Seq<Either<Name,Identifier>>  group_constituent_list() {
		Seq<Either<Name,Identifier>>  list;
		ListBuffer<Either<Name,Identifier>> elements=new ListBuffer<Either<Name,Identifier>>(); 
		Either<Name,Identifier> element=null;
		
		element = group_constituent();
		elements.append(element); 
		while (la.kind == 115) {
			Get();
			element = group_constituent();
			elements.append(element);
		}
		list=elements.toList();
		return list;
	}

	Either<Name,Identifier>  group_constituent() {
		Either<Name,Identifier>  constituent;
		if (la.kind == 5 || la.kind == 6 || la.kind == 7) {
			name = name();
			constituent=new Left(name);
		} else if (la.kind == 4) {
			Get();
			constituent=new Right(toIdentifier(t));
		} else SynErr(154);
		return constituent;
	}

	Identifier  enumeration_literal() {
		Identifier  id;
		if (la.kind == 5 || la.kind == 6) {
			identifier = identifier();
			id=identifier;
		} else if (la.kind == 4) {
			Get();
			id=toIdentifier(t);
		} else SynErr(155);
		return id;
	}

	Range  range() {
		Range  range;
		range =new Range(from.simpleExpr,direction.rangeDirection,to.simpleExpr,name.name_);
		return range;
	}

	PhysicalTypeDefinition  physical_type_definition(Identifier id,Position pos) {
		PhysicalTypeDefinition  physicalTypeDef;
		ListBuffer<PhysicalTypeDefinition.Element> elements=new ListBuffer<PhysicalTypeDefinition.Element>(); 
		Expect(73);
		range = range();
		Expect(96);
		baseIdentifier = identifier();
		Expect(114);
		while (la.kind == 5 || la.kind == 6) {
			identifier = identifier();
			Expect(128);
			literal = physical_literal();
			Expect(114);
			elements.append(new PhysicalTypeDefinition.Element(identifier,literal));
		}
		Expect(31);
		Expect(96);
		if (la.kind == 5 || la.kind == 6) {
			endIdent = identifier();
		}
		physicalTypeDef=new PhysicalTypeDefinition(pos,id,range,baseIdentifier,elements.toList(),endIdent.id);
		return physicalTypeDef;
	}

	Object  physical_literal() {
		Object  x;
		Expect(133);
		return x;
	}

	SelectedName  index_subtype_definition() {
		SelectedName  typeMark;
		typeMark = type_mark();
		Expect(73);
		Expect(112);
		return typeMark;
	}

	DeclarativeItem  protected_type_declarative_item() {
		DeclarativeItem  item;
		if (StartOf(8)) {
			item = subprogram_declartion_or_body();
		} else if (la.kind == 17) {
			item = attribute_specification();
		} else if (la.kind == 98) {
			item = use_clause();
		} else SynErr(156);
		return item;
	}

	DeclarativeItem  protected_type_body_declarative_item() {
		DeclarativeItem  item;
		if (StartOf(8)) {
			item=null;
			item = subprogram_declartion_or_body();
		} else if (la.kind == 94) {
			item = type_declaration();
		} else if (la.kind == 90) {
			item = subtype_declaration();
		} else if (la.kind == 26) {
			item = constant_declaration();
		} else if (la.kind == 84 || la.kind == 99) {
			item = variable_declaration();
		} else if (la.kind == 34) {
			item = file_declaration();
		} else if (la.kind == 11) {
			item = alias_declaration();
		} else if (IsAttributeDeclaration()) {
			item = attribute_declaration();
		} else if (la.kind == 17) {
			item = attribute_specification();
		} else if (la.kind == 98) {
			item = use_clause();
		} else if (IsGroupTemplate()) {
			item = group_template_declaration();
		} else if (la.kind == 39) {
			item = group_declaration();
		} else SynErr(157);
		return item;
	}

	Either<Range,Seq<DiscreteRange>>  constraint() {
		Either<Range,Seq<DiscreteRange>>  constraint;
		if (la.kind == 73) {
			rangeContraint = range_constraint();
			constraint = new Left(rangeContraint);
		} else if (la.kind == 117) {
			ranges = index_constraint();
			constraint = new Right(ranges);
		} else SynErr(158);
		return constraint;
	}

	Range.Direction.Value  direction() {
		Range.Direction.Value  rangeDirection;
		if (la.kind == 92) {
			Get();
			rangeDirection=Range.Direction.To;
		} else if (la.kind == 28) {
			Get();
			rangeDirection=Range.Direction.Downto;
		} else SynErr(159);
		return rangeDirection;
	}

	Range  range_constraint() {
		Range  rangeContraint;
		Expect(73);
		range = range();
		rangeContraint=range;
		return rangeContraint;
	}

	Seq<DiscreteRange>  index_constraint() {
		Seq<DiscreteRange>  ranges;
		ListBuffer<DiscreteRange> list=new ListBuffer<DiscreteRange>();
		DiscreteRange discreteRange=null;
		
		Expect(117);
		discreteRange = discrete_range();
		list.append(discreteRange);
		while (la.kind == 115) {
			Get();
			discreteRange = discrete_range();
			list.append(discreteRange);
		}
		Expect(118);
		ranges = list.toList();
		return ranges;
	}

	DiscreteRange  discrete_range() {
		DiscreteRange  discreteRange;
		range = range();
		discreteRange=new DiscreteRange(new Left(range));
		return discreteRange;
	}

	ConcurrentStatement  architecture_statement() {
		ConcurrentStatement  concurrentStmt;
		if (la.kind == 5 || la.kind == 6) {
			concurrentStmt=null;
			Identifier label=null;
			
			label = label_colon();
			concurrentStmt = architecture_statement_optional_label(label);
		} else if (StartOf(9)) {
			concurrentStmt = architecture_statement_optional_label(label);
		} else SynErr(160);
		return concurrentStmt;
	}

	Identifier  label_colon() {
		Identifier  label;
		label = identifier();
		Expect(121);
		return label;
	}

	ConcurrentStatement  architecture_statement_optional_label(Identifier label) {
		ConcurrentStatement  concurrentStmt;
		if (la.kind == 68) {
			Get();
		}
		if (la.kind == 70) {
			stmt = process_statement(label,postponed!=null);
			concurrentStmt=stmt;
		} else if (la.kind == 16) {
			stmt = concurrent_assertion_statement(label,postponed!=null);
			concurrentStmt=stmt;
		} else if (la.kind == 5 || la.kind == 6 || la.kind == 7) {
			stmt = concurrent_procedure_call_statement(label,postponed!=null);
			concurrentStmt=stmt;
		} else SynErr(161);
		return concurrentStmt;
	}

	ConcurrentStatement  architecture_statement_with_label(Identifier label) {
		ConcurrentStatement  concurrentStmt;
		if (la.kind == 38 || la.kind == 67 || la.kind == 114) {
			concurrentStmt = component_instantiation_statement(label);
		} else if (la.kind == 19) {
			concurrentStmt = block_statement(label);
		} else if (la.kind == 35 || la.kind == 41) {
			concurrentStmt = generate_statement(label);
		} else SynErr(162);
		return concurrentStmt;
	}

	ComponentInstantiationStatement  component_instantiation_statement(Identifier label) {
		ComponentInstantiationStatement  stmt;
		ComponentInstantiationStatement.ComponentType.Value componentType =null;
		Token firstToken=la;
		
		if (la.kind == 38) {
			genericMap = generic_map_aspect();
		}
		if (la.kind == 67) {
			portMap = port_map_aspect();
		}
		Expect(114);
		stmt=new ComponentInstantiationStatement(toPosition(firstToken),label,componentType,$n.name_,$architecture_identifier.id,genericMap,portMap);
		return stmt;
	}

	BlockStatement  block_statement(Identifier label) {
		BlockStatement  blockStmt;
		Position pos=toPosition(la);
		ListBuffer<DeclarativeItem> declItem=new ListBuffer<DeclarativeItem>(); 
		
		Expect(19);
		if (la.kind == 117) {
			Get();
			guard_expression = expression();
			Expect(118);
		}
		if (la.kind == 46) {
			Get();
		}
		if (la.kind == 38) {
			genericClause = generic_clause();
			if (la.kind == 38) {
				genericMap = generic_map_aspect();
				Expect(114);
			}
		}
		if (la.kind == 67) {
			portClause = port_clause();
			if (la.kind == 67) {
				portMap = port_map_aspect();
				Expect(114);
			}
		}
		while (StartOf(3)) {
			item = block_declarative_item();
			declItems.append(item);
		}
		Expect(18);
		statementList = architecture_statement_list();
		Expect(31);
		Expect(19);
		if (la.kind == 5 || la.kind == 6) {
			end_block_label = identifier();
		}
		Expect(114);
		blockStmt=new BlockStatement(pos,label,guard_expression,genericClause,genericMap,portClause,portMap,declItems.toList(),statementList,end_block_label);
		return blockStmt;
	}

	ConcurrentStatement  generate_statement(Identifier label) {
		ConcurrentStatement  generateStmt;
		if (la.kind == 35) {
			generateStmt=null;
			generateStmt = for_generate_statement(label);
		} else if (la.kind == 41) {
			generateStmt = if_generate_statement(label);
		} else SynErr(163);
		return generateStmt;
	}

	ProcessStatement  process_statement(Identifier label,Boolean postponed) {
		ProcessStatement  processStmt;
		Position pos=toPosition(la);
		ListBuffer<DeclarativeItem> declItem=new ListBuffer<DeclarativeItem>(); 
		
		Expect(70);
		if (la.kind == 117) {
			Get();
			name_list = name_list();
			Expect(118);
		}
		if (la.kind == 46) {
			Get();
		}
		while (StartOf(11)) {
			item = process_declarative_item();
			declItem.append(item);
		}
		Expect(18);
		sequentialStatements = sequential_statement_list();
		Expect(31);
		if (la.kind == 68) {
			Get();
		}
		Expect(70);
		if (la.kind == 5 || la.kind == 6) {
			end_process_label = identifier();
		}
		Expect(114);
		processStmt=new ProcessStatement(pos,label,postponed,name_list,declItem.toList(),sequentialStatements,end_process_label);
		return processStmt;
	}

	ConcurrentAssertionStatement  concurrent_assertion_statement(Identifier label,Boolean postponed) {
		ConcurrentAssertionStatement  assertStmt;
		Position pos=toPosition(la);
		Expect(16);
		expr = condition();
		if (la.kind == 78) {
			Get();
			report_expression = expression();
		}
		if (la.kind == 83) {
			Get();
			severity_expression = expression();
		}
		Expect(114);
		assertStmt=new ConcurrentAssertionStatement(pos,label,postponed,expr,report_expression,severity_expression);
		return assertStmt;
	}

	ConcurrentProcedureCallStatement  concurrent_procedure_call_statement(Identifier label,Boolean postponed) {
		ConcurrentProcedureCallStatement  procedureCallStmt;
		SelectedName procedure_name = selected_name();
		if (la.kind == 117) {
			Get();
			associationList = association_list();
			Expect(118);
		}
		Expect(114);
		procedureCallStmt=new ConcurrentProcedureCallStatement(label,postponed,procedure_name,associationList);
		return procedureCallStmt;
	}

	AssociationList  association_list() {
		AssociationList  list;
		ListBuffer<AssociationList.Element> elements=new ListBuffer<AssociationList.Element>();
		AssociationList.Element element=null;
		
		element = association_element();
		elements.append(element);
		while (la.kind == 115) {
			Get();
			element = association_element();
			elements.append(element);
		}
		list=new AssociationList(elements.toList());
		return list;
	}

	Seq<Name>  name_list() {
		Seq<Name>  list;
		ListBuffer<Name> tmpList=new ListBuffer<Name>();
		
		n1 = name();
		tmpList.append(n1);
		while (la.kind == 115) {
			Get();
			n2 = name();
			tmpList.append(n2);
		}
		list=tmpList.toList();
		return list;
	}

	DeclarativeItem  process_declarative_item() {
		DeclarativeItem  item;
		if (StartOf(8)) {
			item=null;
			item = subprogram_declartion_or_body();
		} else if (la.kind == 94) {
			item = type_declaration();
		} else if (la.kind == 90) {
			item = subtype_declaration();
		} else if (la.kind == 26) {
			item = constant_declaration();
		} else if (la.kind == 84 || la.kind == 99) {
			item = variable_declaration();
		} else if (la.kind == 34) {
			item = file_declaration();
		} else if (la.kind == 11) {
			item = alias_declaration();
		} else if (la.kind == 98) {
			item = use_clause();
		} else if (IsAttributeDeclaration()) {
			item = attribute_declaration();
		} else if (la.kind == 17) {
			item = attribute_specification();
		} else if (IsGroupTemplate()) {
			item = group_template_declaration();
		} else if (la.kind == 39) {
			item = group_declaration();
		} else SynErr(164);
		return item;
	}

	Expression  condition() {
		Expression  con;
		expr = expression();
		con=expr;
		return con;
	}

	void concurrent_signal_assignment_statement(ConcurrentSignalAssignmentStatement signalAssignmentStatement, Identifier label,Boolean postponed) {
		if (StartOf(16)) {
			signalAssignmentStatement = conditional_signal_assignment(label,postponed);
		} else if (la.kind == 103) {
			signalAssignmentStatement = selected_signal_assignment(label,postponed);
		} else SynErr(165);
	}

	ConcurrentConditionalSignalAssignment  conditional_signal_assignment(Identifier label,Boolean postponed) {
		ConcurrentConditionalSignalAssignment  signalAssignment;
		ListBuffer<ConcurrentConditionalSignalAssignment.When> elements=new ListBuffer<ConcurrentConditionalSignalAssignment.When>();
		target = target();
		Expect(107);
		if (la.kind == 40) {
			Get();
		}
		if (la.kind == 44 || la.kind == 76 || la.kind == 93) {
			delay = delay_mechanism();
		}
		conditional_waveforms(elements);
		Expect(114);
		signalAssignment=new ConcurrentConditionalSignalAssignment(toPosition($LEQ),label,postponed,target,$GUARDED!=null,delay,elements.toList());
		return signalAssignment;
	}

	ConcurrentSelectedSignalAssignment  selected_signal_assignment(Identifier label,Boolean postponed) {
		ConcurrentSelectedSignalAssignment  signalAssignment;
		Position pos=toPosition(la);
		ListBuffer<ConcurrentSelectedSignalAssignment.When> elements=new ListBuffer<ConcurrentSelectedSignalAssignment.When>(); 
		
		Expect(103);
		expr = expression();
		Expect(82);
		target = target();
		Expect(107);
		if (la.kind == 40) {
			Get();
		}
		if (la.kind == 44 || la.kind == 76 || la.kind == 93) {
			delay = delay_mechanism();
		}
		s1 = selected_waveform();
		elements.append(s1);
		while (la.kind == 115) {
			Get();
			s2 = selected_waveform();
			elements.append(s2);
		}
		Expect(114);
		signalAssignment=new ConcurrentSelectedSignalAssignment(pos,label,postponed,expr,target,$GUARDED!=null,delay,elements.toList());
		return signalAssignment;
	}

	Target  target() {
		Target  target;
		if (la.kind == 5 || la.kind == 6 || la.kind == 7) {
			name = name();
			target = new Target(new Left(name));
		} else if (la.kind == 117) {
			aggregate = aggregate();
			target = new Target(new Right(aggregate));
		} else SynErr(166);
		return target;
	}

	DelayMechanism  delay_mechanism() {
		DelayMechanism  mechanism;
		if (la.kind == 93) {
			Get();
		} else if (la.kind == 44 || la.kind == 76) {
			if (la.kind == 76) {
				Get();
				time_expression = expression();
			}
			Expect(44);
			if (time_expression==null) mechanism=new DelayMechanism(DelayMechanism.DelayType.TRANSPORT,None);
			else mechanism=new DelayMechanism(DelayMechanism.DelayType.INERTIAL,time_expression);
			
		} else SynErr(167);
		return mechanism;
	}

	void conditional_waveforms(ListBuffer<Object> elements) {
		waveform = waveform();
		if (la.kind == 101) {
			Get();
			expr = condition();
			if (la.kind == 29) {
				Get();
				conditional_waveforms(elements);
			}
		}
		elements.prepend(new ConcurrentConditionalSignalAssignment.When(waveform,expr));
	}

	Waveform  waveform() {
		Waveform  waveForm;
		if (StartOf(17)) {
			ListBuffer<Waveform.Element> elements=new ListBuffer<Waveform.Element>();
			e1 = waveform_element();
			elements.append(e1);
			while (la.kind == 115) {
				Get();
				e2 = waveform_element();
				elements.append(e2);
			}
		} else if (la.kind == 95) {
			Get();
			waveForm=new Waveform(toPosition(firstToken),elements.toList());
		} else SynErr(168);
		return waveForm;
	}

	ConcurrentSelectedSignalAssignment.When  selected_waveform() {
		ConcurrentSelectedSignalAssignment.When  whenClause;
		waveform = waveform();
		Expect(101);
		choices = choices();
		whenClause = new ConcurrentSelectedSignalAssignment.When(waveform,choices);
		return whenClause;
	}

	Choices  choices() {
		Choices  choices;
		ListBuffer<Choices.Choice> elements=new ListBuffer<Choices.Choice>(); 
		c1 = choice();
		elements.append(c1);
		while (la.kind == 129) {
			Get();
			c2 = choice();
			elements.append(c2);
		}
		choices =new Choices(elements.toList());
		return choices;
	}

	Aggregate  aggregate() {
		Aggregate  aggregate;
		ListBuffer<Aggregate.ElementAssociation> elements=new ListBuffer<Aggregate.ElementAssociation>(); 
		Expect(117);
		e1 = element_association();
		elements.append(e1);
		while (la.kind == 115) {
			Get();
			e2 = element_association();
			elements.append(e2);
		}
		Expect(118);
		aggregate =new Aggregate(elements.toList());
		return aggregate;
	}

	ForGenerateStatement  for_generate_statement(Identifier label) {
		ForGenerateStatement  forGenerateStmt;
		Position pos=toPosition(la);
		Expect(35);
		loopIdentifier = identifier();
		Expect(43);
		discreteRange = discrete_range();
		Expect(37);
		body = generate_statement_body();
		Expect(31);
		Expect(37);
		if (la.kind == 5 || la.kind == 6) {
			end_generate_label = identifier();
		}
		Expect(114);
		forGenerateStmt=new ForGenerateStatement(pos,label,loopIdentifier,discreteRange,$body.blockItems,$body.statementList,end_generate_label);
		return forGenerateStmt;
	}

	IfGenerateStatement  if_generate_statement(Identifier label) {
		IfGenerateStatement  ifGenerateStmt;
		Position pos=toPosition(la);
		Expect(41);
		expr = condition();
		Expect(37);
		body = generate_statement_body();
		Expect(31);
		Expect(37);
		if (la.kind == 5 || la.kind == 6) {
			end_generate_label = identifier();
		}
		Expect(114);
		ifGenerateStmt=new IfGenerateStatement(pos,label,expr,body._1,body._2,end_generate_label);
		return ifGenerateStmt;
	}

	Tuple2<Seq<DeclarativeItem>,Seq<ConcurrentStatement>>  generate_statement_body() {
		Tuple2<Seq<DeclarativeItem>,Seq<ConcurrentStatement>>  statementList;
		ListBuffer<DeclarativeItem> declarativeItems=new ListBuffer<DeclarativeItem>();
		
		if (StartOf(18)) {
			while (StartOf(3)) {
				item = block_declarative_item();
				declarativeItems.append(item);
			}
			Expect(18);
		}
		statementList = architecture_statement_list();
		statementList=new Tuple2<Seq<DeclarativeItem>,Seq<ConcurrentStatement>>(declarativeItems,statementList);
		
		return statementList;
	}

	SequentialStatement  sequential_statement() {
		SequentialStatement  sequentialStatement;
		sequentialStatement=null;
		if (la.kind == 5 || la.kind == 6) {
			label = label_colon();
		}
		switch (la.kind) {
		case 100: {
			sequentialStatement = wait_statement(label);
			break;
		}
		case 16: {
			sequentialStatement = assertion_statement(label);
			break;
		}
		case 78: {
			sequentialStatement = report_statement(label);
			break;
		}
		case 41: {
			sequentialStatement = if_statement(label);
			break;
		}
		case 23: {
			sequentialStatement = case_statement(label);
			break;
		}
		case 35: case 51: case 102: {
			sequentialStatement = loop_statement(label);
			break;
		}
		case 56: {
			sequentialStatement = next_statement(label);
			break;
		}
		case 33: {
			sequentialStatement = exit_statement(label);
			break;
		}
		case 79: {
			sequentialStatement = return_statement(label);
			break;
		}
		case 59: {
			sequentialStatement = null_statement(label);
			break;
		}
		case 5: case 6: case 7: {
			sequentialStatement = procedure_call_statement(label);
			break;
		}
		default: SynErr(169); break;
		}
		return sequentialStatement;
	}

	WaitStatement  wait_statement(Identifier label) {
		WaitStatement  waitStmt;
		Position pos=toPosition(la);
		Expect(100);
		if (la.kind == 61) {
			Get();
			name_list = name_list();
		}
		if (la.kind == 97) {
			Get();
			untilExpr = condition();
		}
		if (la.kind == 35) {
			Get();
			forExpression = expression();
		}
		Expect(114);
		waitStmt=new WaitStatement(pos,label,name_list,untilExpr,forExpression);
		return waitStmt;
	}

	AssertStatement  assertion_statement(Identifier label) {
		AssertStatement  assertStmt;
		Position pos=toPosition(la);
		Expect(16);
		expr = condition();
		if (la.kind == 78) {
			Get();
			report_expression = expression();
		}
		if (la.kind == 83) {
			Get();
			severity_expression = expression();
		}
		Expect(114);
		assertStmt=new AssertStatement(pos,label,expr,report_expression,severity_expression);
		return assertStmt;
	}

	ReportStatement  report_statement(Identifier label) {
		ReportStatement  reportStmt;
		Position pos=toPosition(la);
		Expect(78);
		report_expression = expression();
		if (la.kind == 83) {
			Get();
			severity_expression = expression();
		}
		Expect(114);
		reportStmt=new ReportStatement(pos,label,report_expression,severity_expression);
		return reportStmt;
	}

	IfStatement  if_statement(Identifier label) {
		IfStatement  ifStmt;
		Position pos=toPosition(la); 
		ListBuffer<IfStatement.IfThenPart> ifList=new ListBuffer<IfStatement.IfThenPart>(); 
		
		Expect(41);
		if_condition = condition();
		Expect(91);
		if_sequential_statement = sequential_statement_list();
		ifList.append(new IfStatement.IfThenPart(if_condition,if_sequential_statement));
		while (la.kind == 30) {
			Get();
			elsif_condition = condition();
			Expect(91);
			elsif_sequential_statement = sequential_statement_list();
			ifList.append(new IfStatement.IfThenPart(elsif_condition,elsif_sequential_statement));
		}
		if (la.kind == 29) {
			Get();
			else_sequential_statement = sequential_statement_list();
		}
		Expect(31);
		Expect(41);
		if (la.kind == 5 || la.kind == 6) {
			end_if_label = identifier();
		}
		Expect(114);
		ifStmt=new IfStatement(pos,label,ifList.toList(),else_sequential_statement,end_if_label);
		return ifStmt;
	}

	CaseStatement  case_statement(Identifier label) {
		CaseStatement  caseStmt;
		Position pos=toPosition(la);
		ListBuffer<CaseStatement.When> alternatives=new ListBuffer<CaseStatement.When>(); 
		
		Expect(23);
		expr = expression();
		Expect(46);
		while (la.kind == 101) {
			Get();
			choices = choices();
			Expect(109);
			sequentialStatements = sequential_statement_list();
			alternatives.append(new CaseStatement.When(choices,sequentialStatements));
		}
		Expect(31);
		Expect(23);
		if (la.kind == 5 || la.kind == 6) {
			end_case_label = identifier();
		}
		Expect(114);
		caseStmt=new CaseStatement(pos,label,expr,alternatives.toList(),end_case_label);
		return caseStmt;
	}

	SequentialStatement  loop_statement(Identifier label) {
		SequentialStatement  loopStmt;
		Position pos=toPosition(la);
		if (la.kind == 35 || la.kind == 102) {
			stmtType = iteration_scheme();
		}
		Expect(51);
		sequentialStatements = sequential_statement_list();
		Expect(31);
		Expect(51);
		if (la.kind == 5 || la.kind == 6) {
			end_loop_label = identifier();
		}
		Expect(114);
		if (stmtType!=null){
		if (stmtType instanceof Left) loopStmt=new WhileStatement(pos,label,((Left)stmtType).a,sequentialStatements,end_loop_label);
		//TODO case Right((identifier,discreteRange)) =>new ForStatement(pos,label,identifier,discreteRange,sequentialStatements,end_loop_label.id);
		}else loopStmt=new LoopStatement(pos,label,sequentialStatements,end_loop_label.id);
		
		return loopStmt;
	}

	NextStatement  next_statement(Identifier label) {
		NextStatement  nextStmt;
		Position pos=toPosition(la);
		Expect(56);
		if (la.kind == 5 || la.kind == 6) {
			identifier = identifier();
		}
		if (la.kind == 101) {
			Get();
			expr = condition();
		}
		Expect(114);
		nextStmt=new NextStatement(pos,label,identifier,expr);
		return nextStmt;
	}

	ExitStatement  exit_statement(Identifier label) {
		ExitStatement  exitStmt;
		Position pos=toPosition(la);
		Expect(33);
		if (la.kind == 5 || la.kind == 6) {
			identifier = identifier();
		}
		if (la.kind == 101) {
			Get();
			expr = condition();
		}
		Expect(114);
		exitStmt=new ExitStatement(pos,label,identifier,expr);
		return exitStmt;
	}

	ReturnStatement  return_statement(Identifier label) {
		ReturnStatement  returnStmt;
		Position pos=toPosition(la);
		Expect(79);
		if (StartOf(19)) {
			expr = expression();
		}
		Expect(114);
		returnStmt=new ReturnStatement(pos,label,expr);
		return returnStmt;
	}

	NullStatement  null_statement(Identifier label) {
		NullStatement  nullStmt;
		Position pos=toPosition(la);
		Expect(59);
		Expect(114);
		nullStmt=new NullStatement(pos,label);
		return nullStmt;
	}

	ProcedureCallStatement  procedure_call_statement(Identifier label) {
		ProcedureCallStatement  procedureCallStmt;
		procedure_name = selected_name();
		if (la.kind == 117) {
			Get();
			associationList = association_list();
			Expect(118);
		}
		Expect(114);
		procedureCallStmt=new ProcedureCallStatement(label,procedure_name,associationList);
		return procedureCallStmt;
	}

	SignalAssignmentStatement  signal_assignment_statement(Identifier label) {
		SignalAssignmentStatement  signalAssignStmt;
		target = target();
		Expect(107);
		if (la.kind == 44 || la.kind == 76 || la.kind == 93) {
			delay = delay_mechanism();
		}
		waveform = waveform();
		Expect(114);
		signalAssignStmt=new SimpleSignalAssignmentStatement(toPosition($LEQ),label,$target,delay,waveform);
		return signalAssignStmt;
	}

	Waveform.Element  waveform_element() {
		Waveform.Element  element;
		value_expression = expression();
		if (la.kind == 10) {
			Get();
			time_expression = expression();
		}
		element= new Waveform.Element(value_expression,time_expression);
		return element;
	}

	VariableAssignmentStatement  variable_assignment_statement(Identifier label) {
		VariableAssignmentStatement  varAssignStmt;
		target = target();
		Expect(111);
		expr = expression();
		Expect(114);
		varAssignStmt=new SimpleVariableAssignmentStatement(toPosition($VAR_ASSIGN),label,target,expr);
		return varAssignStmt;
	}

	Either<Expression,Tuple2<Identifier,DiscreteRange>>  iteration_scheme() {
		Either<Expression,Tuple2<Identifier,DiscreteRange>>  scheme;
		if (la.kind == 102) {
			Get();
			expr = condition();
			scheme=new Left(expr);
		} else if (la.kind == 35) {
			Get();
			identifier = identifier();
			Expect(43);
			discreteRange = discrete_range();
			scheme=new Right(new Tuple2(identifier,discreteRange));
		} else SynErr(170);
		return scheme;
	}

	InterfaceList.AbstractInterfaceElement  interface_element_procedure() {
		InterfaceList.AbstractInterfaceElement  element;
		if (la.kind == 5 || la.kind == 6 || la.kind == 85) {
			signalElement = interface_signal_declaration_procedure();
			element=signalElement;
		} else if (la.kind == 34) {
			fileElement = interface_file_declaration();
			element=fileElement;
		} else SynErr(171);
		return element;
	}

	InterfaceList.InterfaceFileDeclaration  interface_file_declaration() {
		InterfaceList.InterfaceFileDeclaration  fileElement;
		Expect(34);
		list = identifier_list();
		Expect(121);
		subType = subtype_indication();
		fileElement=new InterfaceList.InterfaceFileDeclaration(list,subType);
		return fileElement;
	}

	InterfaceList.AbstractInterfaceElement  interface_element_function() {
		InterfaceList.AbstractInterfaceElement  element;
		if (la.kind == 5 || la.kind == 6 || la.kind == 85) {
			signalElement = interface_signal_declaration_function();
			element=signalElement;
		} else if (la.kind == 34) {
			fileElement = interface_file_declaration();
			element=fileElement;
		} else SynErr(172);
		return element;
	}

	InterfaceList.InterfaceSignalDeclaration  interface_signal_declaration_function() {
		InterfaceList.InterfaceSignalDeclaration  signalElement;
		if (la.kind == 85) {
			Get();
		}
		list = identifier_list();
		Expect(121);
		if (la.kind == 43) {
			Get();
		}
		subType = subtype_indication();
		if (la.kind == 22) {
			Get();
		}
		if (la.kind == 111) {
			Get();
			expr = expression();
		}
		signalElement=new InterfaceList.InterfaceSignalDeclaration(list,InterfaceList.InterfaceMode.IN,subType,$BUS!=null,expr);
		return signalElement;
	}

	InterfaceList.InterfaceMode.Value  interface_mode() {
		InterfaceList.InterfaceMode.Value  mode;
		if (la.kind == 43) {
			Get();
			mode=InterfaceList.InterfaceMode.IN;
		} else if (la.kind == 65) {
			Get();
			mode=InterfaceList.InterfaceMode.OUT;
		} else if (la.kind == 45) {
			Get();
			mode=InterfaceList.InterfaceMode.INOUT;
		} else if (la.kind == 21) {
			Get();
			mode=InterfaceList.InterfaceMode.BUFFER;
		} else if (la.kind == 49) {
			Get();
			mode=InterfaceList.InterfaceMode.LINKAGE;
		} else SynErr(173);
		return mode;
	}

	InterfaceList.InterfaceVariableDeclaration  interface_variable_declaration() {
		InterfaceList.InterfaceVariableDeclaration  varElement;
		if (la.kind == 99) {
			Get();
		}
		list = identifier_list();
		Expect(121);
		if (StartOf(7)) {
			mode = interface_mode();
		}
		subType = subtype_indication();
		if (la.kind == 111) {
			Get();
			expr = expression();
		}
		varElement=new InterfaceList.InterfaceVariableDeclaration(list,mode,subType,expr);
		return varElement;
	}

	AssociationList.Element  association_element() {
		AssociationList.Element  element;
		element=new AssociationList.Element($formal_part.formal_part_,$actual_part.actual_part_);
		return element;
	}

	Name  formal_part() {
		Name  formal_part;
		name = name();
		formal_part = name;
		return formal_part;
	}

	Option<Expression>  actual_part() {
		Option<Expression>  actual_part;
		if (StartOf(20)) {
			expr = expression();
			actual_part = expr;
		} else if (la.kind == 62) {
			Get();
			actual_part=None();
		} else SynErr(174);
		return actual_part;
	}

	Expression  relation() {
		Expression  rel;
		s1 = shift_expression();
		rel=s1;
		if (StartOf(21)) {
			op = relational_operator();
			s2 = shift_expression();
			rel=new Relation(op._1,s1,op._2,s2);
		}
		return rel;
	}

	Tuple2<LogicalExpression.Operator.Value,Position>  logical_operator() {
		Tuple2<LogicalExpression.Operator.Value,Position>  op;
		if (la.kind == 13) {
			Position pos=toPosition(la);
			Get();
			logOp=LogicalExpression.Operator.AND;
		} else if (la.kind == 63) {
			Get();
			logOp=LogicalExpression.Operator.OR;
		} else if (la.kind == 105) {
			Get();
			logOp=LogicalExpression.Operator.XOR;
		} else if (la.kind == 104) {
			Get();
			logOp=LogicalExpression.Operator.XNOR;
		} else SynErr(175);
		return op;
	}

	Expression  shift_expression() {
		Expression  shiftExpr;
		s1 = simple_expression();
		shiftExpr=s1;
		if (StartOf(22)) {
			op = shift_operator();
			s2 = simple_expression();
			shiftExpr=new ShiftExpression(op._1,s1,op._2,s2);
		}
		return shiftExpr;
	}

	Tuple2<Relation.Operator.Value,Position>  relational_operator() {
		Tuple2<Relation.Operator.Value,Position>  op;
		switch (la.kind) {
		case 128: {
			Position pos=toPosition(la);
			Relation.Operator.Value relOp=null;
			
			Get();
			relOp=Relation.Operator.EQ;
			break;
		}
		case 110: {
			Get();
			relOp=Relation.Operator.NEQ;
			break;
		}
		case 126: {
			Get();
			relOp=Relation.Operator.LT;
			break;
		}
		case 107: {
			Get();
			relOp=Relation.Operator.LEQ;
			break;
		}
		case 127: {
			Get();
			relOp=Relation.Operator.GT;
			break;
		}
		case 108: {
			Get();
			relOp=Relation.Operator.GEQ;
			op=new Tuple2<Relation.Operator.Value,Position>(relOp,pos);
			break;
		}
		default: SynErr(176); break;
		}
		return op;
	}

	Expression  simple_expression() {
		Expression  simpleExpr;
		if (la.kind == 124 || la.kind == 125) {
			sign = sign();
		}
		t1 = term();
		if (s!=null) simpleExpr=new SimpleExpression(s._2,s._1,t1,None,None); else simpleExpr=t1;
		while (la.kind == 116 || la.kind == 124 || la.kind == 125) {
			op = adding_operator();
			t2 = term();
			simpleExpr=new SimpleExpression($op.pos,None,$simpleExpr,$op.addOp,t2);
		}
		return simpleExpr;
	}

	Tuple2<ShiftExpression.Operator.Value,Position>  shift_operator() {
		Tuple2<ShiftExpression.Operator.Value,Position>  op;
		switch (la.kind) {
		case 87: {
			ShiftExpression.Operator.Value shiftOp=null;
			Position pos=toPosition(la);
			
			Get();
			shiftOp=ShiftExpression.Operator.SLL;
			break;
		}
		case 89: {
			Get();
			shiftOp=ShiftExpression.Operator.SRL;
			break;
		}
		case 86: {
			Get();
			shiftOp=ShiftExpression.Operator.SLA;
			break;
		}
		case 88: {
			Get();
			shiftOp=ShiftExpression.Operator.SRA;
			break;
		}
		case 80: {
			Get();
			shiftOp=ShiftExpression.Operator.ROL;
			break;
		}
		case 81: {
			Get();
			shiftOp=ShiftExpression.Operator.ROR;
			op=new Tuple2<ShiftExpression.Operator.Value,Position>(shiftOp,pos);
			break;
		}
		default: SynErr(177); break;
		}
		return op;
	}

	Tuple2<SimpleExpression.SignOperator.Value,Position>  sign() {
		Tuple2<SimpleExpression.SignOperator.Value,Position>  op;
		if (la.kind == 124) {
			SimpleExpression.SignOperator.Value signOp=null;
			Position pos=toPosition(la);
			
			Get();
			signOp=SimpleExpression.SignOperator.PLUS;
		} else if (la.kind == 125) {
			Get();
			signOp=SimpleExpression.SignOperator.MINUS;
			op=new Tuple2<SimpleExpression.SignOperator.Value,Position>(signOp,pos);
		} else SynErr(178);
		return op;
	}

	Expression  term() {
		Expression  term;
		f1 = factor();
		term = f1;
		if (StartOf(23)) {
			op = multiplying_operator();
			f2 = factor();
			term = new Term(op._1,term,op._2,f2);
		}
		return term;
	}

	Tuple2<SimpleExpression.AddOperator.Value,Position>  adding_operator() {
		Tuple2<SimpleExpression.AddOperator.Value,Position>  op;
		if (la.kind == 124) {
			SimpleExpression.AddOperator.Value addOp=null;
			Position pos=toPosition(la);
			
			Get();
			addOp=SimpleExpression.AddOperator.PLUS;
		} else if (la.kind == 125) {
			Get();
			addOp=SimpleExpression.AddOperator.MINUS;
		} else if (la.kind == 116) {
			Get();
			addOp=SimpleExpression.AddOperator.AMPERSAND;
			op=new Tuple2<SimpleExpression.AddOperator.Value,Position>(addOp,pos);
		} else SynErr(179);
		return op;
	}

	Tuple2<Term.Operator.Value,Position>  multiplying_operator() {
		Tuple2<Term.Operator.Value,Position>  op;
		if (la.kind == 122) {
			Term.Operator.Value mulOp=null;
			Position pos=toPosition(la);
			
			Get();
			mulOp=Term.Operator.MUL;
		} else if (la.kind == 123) {
			Get();
			mulOp=Term.Operator.DIV;
		} else if (la.kind == 53) {
			Get();
			mulOp=Term.Operator.MOD;
		} else if (la.kind == 77) {
			Get();
			mulOp=Term.Operator.REM;
			op=new Tuple2<SimpleExpression.AddOperator.Value,Position>(mulOp,pos);
		} else SynErr(180);
		return op;
	}

	Expression  factor() {
		Expression  factor;
		if (StartOf(24)) {
			p1 = primary();
			factor = p1;
			if (la.kind == 106) {
				Get();
				p2 = primary();
				factor_ = new Factor(toPosition($DOUBLESTAR),$p1.obj,Factor.Operator.POW,$p2.obj);
			}
		} else if (la.kind == 8) {
			Get();
			p1 = primary();
			factor = new Factor(toPosition($ABS),p1,Factor.Operator.ABS);
		} else if (la.kind == 58) {
			Get();
			p1 = primary();
			factor = new Factor(toPosition($NOT),p2,Factor.Operator.NOT);
		} else SynErr(181);
		return factor;
	}

	Expression  primary() {
		Expression  p;
		if (StartOf(25)) {
		} else if (la.kind == 55) {
			allocator.newExpression = allocator();
			p=allocator.newExpression;
		} else if (la.kind == 117) {
			aggregate = aggregate();
			p=new AggregateExpression(aggregate);
		} else SynErr(182);
		return p;
	}

	Expression  allocator() {
		Expression  newExpression;
		Position pos=toPosition(la);
		Expect(55);
		selectedName = selected_name();
		if (la.kind == 3) {
			expr = qualified_expression(selectedName);
			newExpression=new NewExpression(pos,new Left(expr));
		} else if (StartOf(26)) {
			if (la.kind == 117) {
				constraint = index_constraint();
			}
			newExpression=new NewExpression(pos,new Right(new SubTypeIndication(None,selectedName,new Right(ranges))));
		} else SynErr(183);
		return newExpression;
	}

	QualifiedExpression  qualified_expression(SelectedName typeName) {
		QualifiedExpression  expr;
		Expect(3);
		aggregate = aggregate();
		expr=new QualifiedExpression(typeName,new AggregateExpression($aggregate.aggregate_));
		return expr;
	}

	FunctionCallExpression  function_call() {
		FunctionCallExpression  functionCall;
		function_name = selected_name();
		if (la.kind == 117) {
			Get();
			parameter_association_list = association_list();
			Expect(118);
		}
		functionCall=new FunctionCallExpression(function_name,parameter_association_list);
		return functionCall;
	}

	Identifier  name_prefix() {
		Identifier  id;
		if (la.kind == 5 || la.kind == 6) {
			identifier = identifier();
			id=identifier;
		} else if (la.kind == 7) {
			Get();
			id=toIdentifier($STRING_LITERAL);
		} else SynErr(184);
		return id;
	}

	Name.SelectedPart  name_selected_part() {
		Name.SelectedPart  part;
		Expect(130);
		if (la.kind == 5 || la.kind == 6) {
			identifier = identifier();
			part= new Name.SelectedPart(identifier);
		} else if (la.kind == 4) {
			Get();
			part= new Name.SelectedPart(toIdentifier($CHARACTER_LITERAL));
		} else if (la.kind == 7) {
			Get();
			part= new Name.SelectedPart(toIdentifier($STRING_LITERAL));
		} else if (la.kind == 12) {
			Get();
			part= new Name.SelectedPart(toIdentifier($ALL));
		} else SynErr(185);
		return part;
	}

	Name.Part  name_part() {
		Name.Part  part;
		if (la.kind == 130) {
			selectedPart = name_selected_part();
			part = selectedPart;
		} else if (la.kind == 3 || la.kind == 119) {
			attributePart = name_attribute_part();
			part = attributePart;
		} else if (la.kind == 117) {
			slicePart = name_slice_part();
			part = slicePart;
		} else SynErr(186);
		return part;
	}

	Name.AttributePart  name_attribute_part() {
		Name.AttributePart  part;
		if (la.kind == 119) {
			signature = signature();
		}
		Expect(3);
		return part;
	}

	Name.SlicePart  name_slice_part() {
		Name.SlicePart  part;
		Expect(117);
		discreteRange = discrete_range();
		Expect(118);
		part=new Name.SlicePart(discreteRange);
		return part;
	}

	Name.IndexPart  name_indexed_part() {
		Name.IndexPart  part;
		ListBuffer<Expression> indexes=new ListBuffer<Expression>(); 
		Expect(117);
		e1 = expression();
		indexes.append(e1);
		while (la.kind == 115) {
			Get();
			e2 = expression();
			indexes.append(e2);
		}
		Expect(118);
		part=new Name.IndexPart(indexes.toList());
		return part;
	}

	Object  choice() {
		Object  x;
		Expect(132);
		return x;
	}

	Object  element_association() {
		Object  x;
		Expect(133);
		return x;
	}



	public void Parse() {
		la = new Token();
		la.val = "";		
		Get();
		VHDL();

		Expect(0);
	}

	private static final boolean[][] set = {
		{T,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x},
		{x,x,x,x, x,x,x,x, x,x,x,x, x,x,T,x, x,x,x,x, x,x,x,x, x,T,x,x, x,x,x,x, T,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, T,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,T,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,T,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x},
		{x,x,x,x, x,x,x,x, x,x,x,T, x,x,x,x, x,T,x,x, x,x,x,x, x,x,T,T, x,x,x,x, x,x,T,x, T,x,x,T, x,x,T,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,T,x,x, T,x,x,x, x,x,x,x, x,x,x,x, T,T,x,x, x,x,T,x, x,x,T,x, x,x,T,T, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x},
		{x,x,x,x, x,x,x,x, x,x,x,T, x,x,x,x, x,T,x,x, x,x,x,x, T,x,T,T, x,x,x,x, x,x,T,T, T,x,x,T, x,x,T,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,T,x,x, T,x,x,x, x,x,x,x, x,x,x,x, T,T,x,x, x,x,T,x, x,x,T,x, x,x,T,T, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x},
		{x,x,x,x, x,x,x,x, x,x,x,T, x,x,x,x, x,T,x,x, x,x,x,x, x,x,T,x, x,x,x,x, x,x,T,x, T,x,x,T, x,x,T,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,T,x,x, T,x,x,x, x,x,x,x, x,x,x,x, T,T,x,x, x,x,T,x, x,x,T,x, x,x,T,T, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x},
		{x,x,x,x, x,x,x,x, x,x,x,T, x,x,x,x, x,T,x,x, x,x,x,x, T,x,T,T, x,x,x,x, x,x,T,x, T,x,x,T, x,x,T,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,T,x,x, T,x,x,x, x,x,x,x, x,x,x,x, T,T,x,x, x,x,T,x, x,x,T,x, x,x,T,T, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x},
		{x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,T,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,T, x,x,x,T, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,T,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x},
		{x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,T,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,T, x,T,x,x, x,T,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,T,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x},
		{x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, T,x,x,x, x,x,T,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,T,x,x, T,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x},
		{x,x,x,x, x,T,T,T, x,x,x,x, x,x,x,x, T,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, T,x,T,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x},
		{x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,T,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,T, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,T,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,T,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x},
		{x,x,x,x, x,x,x,x, x,x,x,T, x,x,x,x, x,T,x,x, x,x,x,x, x,x,T,x, x,x,x,x, x,x,T,x, T,x,x,T, x,x,T,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,T,x,x, T,x,x,x, x,x,x,x, x,x,x,x, T,x,x,x, x,x,T,x, x,x,T,x, x,x,T,T, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x},
		{x,x,x,x, x,T,T,T, x,x,x,x, x,x,x,x, T,x,x,x, x,x,x,T, x,x,x,x, x,x,x,x, x,T,x,T, x,x,x,x, x,T,x,x, x,x,x,x, x,x,x,T, x,x,x,x, T,x,x,T, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,T,T, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, T,x,T,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x},
		{x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,T,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, T,x,x,x, x,x,T,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,T,x,x, T,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,T,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x},
		{x,x,x,x, x,x,x,x, x,x,x,x, x,T,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,T, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, T,T,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x},
		{x,x,x,x, T,T,T,T, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x},
		{x,x,x,x, x,T,T,T, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,T,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x},
		{x,x,x,x, x,x,x,x, T,x,T,x, x,T,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,T,x,T, x,x,T,x, x,x,x,T, x,x,x,x, x,x,x,x, x,x,x,x, x,T,x,x, T,T,x,x, x,x,T,T, T,T,x,x, x,x,x,x, x,x,x,x, x,T,x,x, T,T,T,T, T,x,T,x, x,x,T,T, T,T,x,x, x,x,T,T, T,T,T,T, T,x,x,x, x,x,x,x},
		{x,x,x,x, x,x,x,x, x,x,x,T, x,x,x,x, x,T,T,x, x,x,x,x, T,x,T,T, x,x,x,x, x,x,T,T, T,x,x,T, x,x,T,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,T,x,x, T,x,x,x, x,x,x,x, x,x,x,x, T,T,x,x, x,x,T,x, x,x,T,x, x,x,T,T, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x},
		{x,x,x,x, x,x,x,x, T,x,x,x, x,T,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,T,x,T, x,x,T,x, x,x,x,T, x,x,x,x, x,x,x,x, x,x,x,x, x,T,x,x, T,T,x,x, x,x,T,T, T,T,x,x, x,x,x,x, x,x,x,x, x,x,x,x, T,T,T,T, T,x,T,x, x,x,T,x, T,T,x,x, x,x,T,T, T,T,T,T, T,x,x,x, x,x,x,x},
		{x,x,x,x, x,x,x,x, T,x,x,x, x,T,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,T,x,T, x,x,T,x, x,x,x,T, x,x,x,x, x,x,x,x, x,x,x,x, x,T,x,x, T,T,x,x, x,x,T,T, T,T,x,x, x,x,x,x, x,x,x,x, x,x,x,x, T,T,T,T, T,x,T,x, x,x,x,x, T,T,x,x, x,x,T,T, T,T,T,T, T,x,x,x, x,x,x,x},
		{x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,T, T,x,T,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,T,T, T,x,x,x, x,x,x,x},
		{x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, T,T,x,x, x,x,T,T, T,T,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x},
		{x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,T,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,T,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,T,T, x,x,x,x, x,x,x,x, x,x,x,x},
		{x,x,x,x, x,x,x,x, x,x,T,x, x,T,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,T,x,x, x,x,x,T, x,T,x,x, x,x,x,x, T,x,T,x, x,x,x,T, x,T,x,T, x,x,x,x, x,x,x,T, x,x,x,x, x,x,x,x, x,x,x,x, x,T,T,x, T,T,T,T, x,x,T,T, T,T,x,T, x,x,x,x, x,x,x,x, x,T,x,x, T,T,T,T, T,x,T,x, x,x,T,T, T,T,T,x, x,x,T,T, T,T,T,T, T,x,x,x, x,x,x,x},
		{x,x,x,x, x,x,x,x, x,x,T,x, x,T,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,T,x,x, x,x,x,T, x,T,x,x, x,x,x,x, T,x,T,x, x,x,x,T, x,T,x,x, x,x,x,x, x,x,x,T, x,x,x,x, x,x,x,x, x,x,x,x, x,T,T,x, T,T,T,T, x,x,T,T, T,T,x,T, x,x,x,x, x,x,x,x, x,T,x,x, T,T,T,T, T,x,T,x, x,x,T,T, T,x,T,x, x,x,T,T, T,T,T,T, T,x,x,x, x,x,x,x},
		{x,x,x,x, x,x,x,x, x,x,T,x, x,T,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,T,x,x, x,x,x,T, x,T,x,x, x,x,x,x, T,x,T,x, x,x,x,T, x,T,x,x, x,x,x,x, x,x,x,T, x,x,x,x, x,x,x,x, x,x,x,x, x,T,T,x, T,T,T,T, x,x,T,T, T,T,x,T, x,x,x,x, x,x,x,x, x,T,x,x, T,T,T,T, T,x,T,x, x,x,T,T, T,T,T,x, x,x,T,T, T,T,T,T, T,x,x,x, x,x,x,x}

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
			case 1: s = "INTEGER expected"; break;
			case 2: s = "LETTER expected"; break;
			case 3: s = "APOSTROPHE expected"; break;
			case 4: s = "CHARACTER_LITERAL expected"; break;
			case 5: s = "EXTENDED_IDENTIFIER expected"; break;
			case 6: s = "BASIC_IDENTIFIER expected"; break;
			case 7: s = "STRING_LITERAL expected"; break;
			case 8: s = "ABS expected"; break;
			case 9: s = "ACCESS expected"; break;
			case 10: s = "AFTER expected"; break;
			case 11: s = "ALIAS expected"; break;
			case 12: s = "ALL expected"; break;
			case 13: s = "AND expected"; break;
			case 14: s = "ARCHITECTURE expected"; break;
			case 15: s = "ARRAY expected"; break;
			case 16: s = "ASSERT expected"; break;
			case 17: s = "ATTRIBUTE expected"; break;
			case 18: s = "BEGIN expected"; break;
			case 19: s = "BLOCK expected"; break;
			case 20: s = "BODY expected"; break;
			case 21: s = "BUFFER expected"; break;
			case 22: s = "BUS expected"; break;
			case 23: s = "CASE expected"; break;
			case 24: s = "COMPONENT expected"; break;
			case 25: s = "CONFIGURATION expected"; break;
			case 26: s = "CONSTANT expected"; break;
			case 27: s = "DISCONNECT expected"; break;
			case 28: s = "DOWNTO expected"; break;
			case 29: s = "ELSE expected"; break;
			case 30: s = "ELSIF expected"; break;
			case 31: s = "END_TOKEN expected"; break;
			case 32: s = "ENTITY expected"; break;
			case 33: s = "EXIT expected"; break;
			case 34: s = "FILE expected"; break;
			case 35: s = "FOR expected"; break;
			case 36: s = "FUNCTION expected"; break;
			case 37: s = "GENERATE expected"; break;
			case 38: s = "GENERIC expected"; break;
			case 39: s = "GROUP expected"; break;
			case 40: s = "GUARDED expected"; break;
			case 41: s = "IF_TOKEN expected"; break;
			case 42: s = "IMPURE expected"; break;
			case 43: s = "IN expected"; break;
			case 44: s = "INERTIAL expected"; break;
			case 45: s = "INOUT expected"; break;
			case 46: s = "IS expected"; break;
			case 47: s = "LABEL expected"; break;
			case 48: s = "LIBRARY expected"; break;
			case 49: s = "LINKAGE expected"; break;
			case 50: s = "LITERAL expected"; break;
			case 51: s = "LOOP expected"; break;
			case 52: s = "MAP expected"; break;
			case 53: s = "MOD expected"; break;
			case 54: s = "NAND expected"; break;
			case 55: s = "NEW expected"; break;
			case 56: s = "NEXT expected"; break;
			case 57: s = "NOR expected"; break;
			case 58: s = "NOT expected"; break;
			case 59: s = "NULL expected"; break;
			case 60: s = "OF expected"; break;
			case 61: s = "ON expected"; break;
			case 62: s = "OPEN expected"; break;
			case 63: s = "OR expected"; break;
			case 64: s = "OTHERS expected"; break;
			case 65: s = "OUT expected"; break;
			case 66: s = "PACKAGE expected"; break;
			case 67: s = "PORT expected"; break;
			case 68: s = "POSTPONED expected"; break;
			case 69: s = "PROCEDURE expected"; break;
			case 70: s = "PROCESS expected"; break;
			case 71: s = "PROTECTED expected"; break;
			case 72: s = "PURE expected"; break;
			case 73: s = "RANGE expected"; break;
			case 74: s = "RECORD expected"; break;
			case 75: s = "REGISTER expected"; break;
			case 76: s = "REJECT expected"; break;
			case 77: s = "REM expected"; break;
			case 78: s = "REPORT expected"; break;
			case 79: s = "RETURN expected"; break;
			case 80: s = "ROL expected"; break;
			case 81: s = "ROR expected"; break;
			case 82: s = "SELECT expected"; break;
			case 83: s = "SEVERITY expected"; break;
			case 84: s = "SHARED expected"; break;
			case 85: s = "SIGNAL expected"; break;
			case 86: s = "SLA expected"; break;
			case 87: s = "SLL expected"; break;
			case 88: s = "SRA expected"; break;
			case 89: s = "SRL expected"; break;
			case 90: s = "SUBTYPE expected"; break;
			case 91: s = "THEN expected"; break;
			case 92: s = "TO_TOKEN expected"; break;
			case 93: s = "TRANSPORT expected"; break;
			case 94: s = "TYPE expected"; break;
			case 95: s = "UNAFFECTED expected"; break;
			case 96: s = "UNITS expected"; break;
			case 97: s = "UNTIL expected"; break;
			case 98: s = "USE expected"; break;
			case 99: s = "VARIABLE expected"; break;
			case 100: s = "WAIT expected"; break;
			case 101: s = "WHEN expected"; break;
			case 102: s = "WHILE expected"; break;
			case 103: s = "WITH expected"; break;
			case 104: s = "XNOR expected"; break;
			case 105: s = "XOR expected"; break;
			case 106: s = "DOUBLESTAR expected"; break;
			case 107: s = "LEQ expected"; break;
			case 108: s = "GEQ expected"; break;
			case 109: s = "ARROW expected"; break;
			case 110: s = "NEQ expected"; break;
			case 111: s = "VAR_ASSIGN expected"; break;
			case 112: s = "BOX expected"; break;
			case 113: s = "DBLQUOTE expected"; break;
			case 114: s = "SEMICOLON expected"; break;
			case 115: s = "COMMA expected"; break;
			case 116: s = "AMPERSAND expected"; break;
			case 117: s = "LPAREN expected"; break;
			case 118: s = "RPAREN expected"; break;
			case 119: s = "LBRACKET expected"; break;
			case 120: s = "RBRACKET expected"; break;
			case 121: s = "COLON expected"; break;
			case 122: s = "MUL expected"; break;
			case 123: s = "DIV expected"; break;
			case 124: s = "PLUS expected"; break;
			case 125: s = "MINUS expected"; break;
			case 126: s = "LT expected"; break;
			case 127: s = "GT expected"; break;
			case 128: s = "EQ expected"; break;
			case 129: s = "BAR expected"; break;
			case 130: s = "DOT expected"; break;
			case 131: s = "\"jldfkj\u00f6l\" expected"; break;
			case 132: s = "\"\u00f6ksdf\" expected"; break;
			case 133: s = "\"\u00f6ksdsdf\" expected"; break;
			case 134: s = "??? expected"; break;
			case 135: s = "invalid library_unit"; break;
			case 136: s = "invalid identifier"; break;
			case 137: s = "invalid entity_declarative_item"; break;
			case 138: s = "invalid type_declaration"; break;
			case 139: s = "invalid disconnection_specification"; break;
			case 140: s = "invalid block_declarative_item"; break;
			case 141: s = "invalid configuration_declarative_item"; break;
			case 142: s = "invalid package_declarative_item"; break;
			case 143: s = "invalid package_body_declarative_item"; break;
			case 144: s = "invalid designator"; break;
			case 145: s = "invalid subprogram_specification"; break;
			case 146: s = "invalid subprogram_declarative_item"; break;
			case 147: s = "invalid type_definition"; break;
			case 148: s = "invalid alias_designator"; break;
			case 149: s = "invalid entity_name_list"; break;
			case 150: s = "invalid entity_class"; break;
			case 151: s = "invalid entity_designator"; break;
			case 152: s = "invalid instantiation_list"; break;
			case 153: s = "invalid entity_aspect"; break;
			case 154: s = "invalid group_constituent"; break;
			case 155: s = "invalid enumeration_literal"; break;
			case 156: s = "invalid protected_type_declarative_item"; break;
			case 157: s = "invalid protected_type_body_declarative_item"; break;
			case 158: s = "invalid constraint"; break;
			case 159: s = "invalid direction"; break;
			case 160: s = "invalid architecture_statement"; break;
			case 161: s = "invalid architecture_statement_optional_label"; break;
			case 162: s = "invalid architecture_statement_with_label"; break;
			case 163: s = "invalid generate_statement"; break;
			case 164: s = "invalid process_declarative_item"; break;
			case 165: s = "invalid concurrent_signal_assignment_statement"; break;
			case 166: s = "invalid target"; break;
			case 167: s = "invalid delay_mechanism"; break;
			case 168: s = "invalid waveform"; break;
			case 169: s = "invalid sequential_statement"; break;
			case 170: s = "invalid iteration_scheme"; break;
			case 171: s = "invalid interface_element_procedure"; break;
			case 172: s = "invalid interface_element_function"; break;
			case 173: s = "invalid interface_mode"; break;
			case 174: s = "invalid actual_part"; break;
			case 175: s = "invalid logical_operator"; break;
			case 176: s = "invalid relational_operator"; break;
			case 177: s = "invalid shift_operator"; break;
			case 178: s = "invalid sign"; break;
			case 179: s = "invalid adding_operator"; break;
			case 180: s = "invalid multiplying_operator"; break;
			case 181: s = "invalid factor"; break;
			case 182: s = "invalid primary"; break;
			case 183: s = "invalid allocator"; break;
			case 184: s = "invalid name_prefix"; break;
			case 185: s = "invalid name_selected_part"; break;
			case 186: s = "invalid name_part"; break;
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

