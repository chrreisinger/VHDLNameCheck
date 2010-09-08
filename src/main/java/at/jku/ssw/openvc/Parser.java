package at.jku.ssw.openvc;

import at.jku.ssw.openvc.*;
import at.jku.ssw.openvc.ast.*;
import at.jku.ssw.openvc.ast.concurrentStatements.*;
import at.jku.ssw.openvc.ast.declarations.*;
import at.jku.ssw.openvc.ast.expressions.*;
import at.jku.ssw.openvc.ast.sequentialStatements.*;
import at.jku.ssw.openvc.ast.ams.*;
import scala.Either;
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
	public static final int _AMS_ASSIGN = 107;
	public static final int _LEQ = 108;
	public static final int _GEQ = 109;
	public static final int _ARROW = 110;
	public static final int _NEQ = 111;
	public static final int _VAR_ASSIGN = 112;
	public static final int _BOX = 113;
	public static final int _DBLQUOTE = 114;
	public static final int _SEMICOLON = 115;
	public static final int _COMMA = 116;
	public static final int _AMPERSAND = 117;
	public static final int _LPAREN = 118;
	public static final int _RPAREN = 119;
	public static final int _LBRACKET = 120;
	public static final int _RBRACKET = 121;
	public static final int _COLON = 122;
	public static final int _MUL = 123;
	public static final int _DIV = 124;
	public static final int _PLUS = 125;
	public static final int _MINUS = 126;
	public static final int _LT = 127;
	public static final int _GT = 128;
	public static final int _EQ = 129;
	public static final int _BAR = 130;
	public static final int _DOT = 131;
	public static final int maxT = 138;

	static final boolean T = true;
	static final boolean x = false;
	static final int minErrDist = 2;

	public Token t;    // last recognized token
	public Token la;   // lookahead token
	int errDist = minErrDist;
	
	public Scanner scanner;
	public Errors errors;

	

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
	
	void VHDL() {
		Expect(132);
		Expect(133);
		Expect(134);
		System.out.println(t.val); 
	}

	DesignFile  design_file() {
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
		ListBuffer<Identifier> libraries=new Buffer<Identifier>();
		ListBuffer<UseClause> useClauses=new Buffer<UseClause>();
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
		identifierList=Seq(); 
		Expect(48);
		list = identifier_list();
		Expect(115);
		identifierList=list; 
		return identifierList;
	}

	UseClause  use_clause() {
		UseClause  useClause;
		Expect(98);
		list = selected_name_list();
		Expect(115);
		useClause=new UseClause(toPosition($USE),list);
		return useClause;
	}

	LibraryUnit  library_unit() {
		LibraryUnit  libraryUnit;
		if (la.kind == 32) {
			entityDecl = entity_declaration();
			libraryUnit=entityDecl;
		} else if (la.kind == 14) {
			archDecl = architecture_body();
			libraryUnit=archDecl;
		} else if (la.kind == 66) {
			packageDecl = package_declaration();
			libraryUnit=packageDecl;
		} else if (la.kind == 66) {
			packageBody = package_body();
			libraryUnit=packageBody;
		} else if (la.kind == 25) {
			configDecl = configuration_declaration();
			libraryUnit=configDecl;
		} else SynErr(139);
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
		Expect(115);
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
		Expect(115);
		archDecl=new ArchitectureDeclaration(start_identifier,declarativeItems.toList(),entityName,statementList,end_identifier); 
		return archDecl;
	}

	PackageDeclaration  package_declaration() {
		PackageDeclaration  packageDecl;
		ListBuffer<DeclarativeItem> declarativeItems=new ListBuffer<DeclarativeItem>(); 
		Expect(66);
		start_identifier = identifier();
		Expect(46);
		while (StartOf(4)) {
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
		Expect(115);
		packageDecl=new PackageDeclaration(start_identifier,declarativeItems.toList(),end_identifier);
		return packageDecl;
	}

	PackageBodyDeclaration  package_body() {
		PackageBodyDeclaration  packageBody;
		ListBuffer<DeclarativeItem> declarativeItems=new ListBuffer<DeclarativeItem>();
		Expect(66);
		Expect(20);
		start_identifier = identifier();
		Expect(46);
		while (StartOf(5)) {
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
		Expect(115);
		packageBody = new PackageBodyDeclaration(start_identifier,declarativeItems.toList(),end_identifier);
		return packageBody;
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
		Expect(115);
		configDecl=new ConfigurationDeclaration(start_identifier,declarativeItems.toList(),entityName,blockConfig,end_identifier);
		return configDecl;
	}

	Seq<Identifier>  identifier_list() {
		Seq<Identifier>  list;
		ListBuffer<Identifier> tmpList=new ListBuffer<Identifier>();
		list=List();
		
		id1 = identifier();
		tmpList.append(id1);
		while (la.kind == 116) {
			Get();
			id2 = identifier();
			tmpList.append(id2);
		}
		list=tmpList.toList();
		return list;
	}

	InterfaceList  generic_clause() {
		InterfaceList  list;
		Expect(38);
		Expect(118);
		genericList = generic_interface_list();
		Expect(119);
		Expect(115);
		list = genericList; 
		return list;
	}

	InterfaceList  generic_interface_list() {
		InterfaceList  list;
		ListBuffer<InterfaceList.InterfaceConstantDeclaration> elements=new ListBuffer<InterfaceList.InterfaceConstantDeclaration>(); 
		decl = interface_constant_declaration();
		elements.append(decl);
		while (la.kind == 115) {
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
		Expect(122);
		if (la.kind == 43) {
			Get();
		}
		subType = subtype_indication();
		if (la.kind == 112) {
			Get();
			expr = expression();
		}
		constElement=new InterfaceList.InterfaceConstantDeclaration(list,subType,expr);
		return constElement;
	}

	InterfaceList  port_clause() {
		InterfaceList  list;
		Expect(67);
		Expect(118);
		portList = port_interface_list();
		Expect(119);
		Expect(115);
		list = portList;
		return list;
	}

	InterfaceList  port_interface_list() {
		InterfaceList  list;
		ListBuffer<InterfaceList.InterfaceSignalDeclaration> elements=new ListBuffer<InterfaceList.InterfaceSignalDeclaration>(); 
		decl = interface_signal_declaration_procedure();
		elements.append(decl); 
		while (la.kind == 115) {
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
		Expect(122);
		if (StartOf(7)) {
			mode = interface_mode();
		}
		subType = subtype_indication();
		if (la.kind == 22) {
			Get();
		}
		if (la.kind == 112) {
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
			id=toIdentifier(input.LT(-1));
		} else if (la.kind == 5) {
			Get();
			id=toIdentifier(input.LT(-1),false);
		} else SynErr(140);
		return id;
	}

	DeclarativeItem  entity_declarative_item() {
		DeclarativeItem  node;
		if (StartOf(8)) {
			declOrBody = subprogram_declartion_or_body();
			node=declOrBody;
		} else if (la.kind == 94) {
			typeDecl = type_declaration();
			node=typeDecl;
		} else if (la.kind == 90) {
			subTypeDecl = subtype_declaration();
			node=subTypeDecl;
		} else if (la.kind == 26) {
			constantDecl = constant_declaration();
			node=constantDecl;
		} else if (la.kind == 85) {
			signalDecl = signal_declaration();
			node=signalDecl; 
		} else if (la.kind == 84 || la.kind == 99) {
			varDecl = variable_declaration();
			node=varDecl; 
		} else if (la.kind == 34) {
			fileDecl = file_declaration();
			node=fileDecl; 
		} else if (la.kind == 11) {
			aliasDecl = alias_declaration();
			node=aliasDecl; 
		} else if (la.kind == 17) {
			attributeDecl = attribute_declaration();
			node=attributeDecl; 
		} else if (la.kind == 17) {
			attributeSpec = attribute_specification();
			node=attributeSpec.node; 
		} else if (la.kind == 27) {
			disconnectSpec = disconnection_specification();
			node=disconnectSpec;
		} else if (la.kind == 98) {
			useClause = use_clause();
			node=useClause;
		} else if (la.kind == 39) {
			groupTemplateDecl = group_template_declaration();
			node=groupTemplateDecl;
		} else if (la.kind == 39) {
			groupDecl = group_declaration();
			node=groupDecl;
		} else SynErr(141);
		return node;
	}

	DeclarativeItem  subprogram_declartion_or_body() {
		DeclarativeItem  declOrBody;
		decl = subprogram_specification();
		if (la.kind == 46) {
			subProgramDef = subprogram_body(decl);
		}
		Expect(115);
		if (subProgramDef!=null) declOrBody=subProgramDef; else declOrBody=decl;
		return declOrBody;
	}

	AbstractTypeDeclaration  type_declaration() {
		AbstractTypeDeclaration  typeDecl;
		Expect(94);
		id = identifier();
		if (la.kind == 46) {
			Get();
			typeDef = type_definition(id,toPosition($TYPE));
			Expect(115);
			typeDecl=typeDef;
		} else if (la.kind == 115) {
			Get();
			typeDecl=new IncompleteTypeDeclaration(toPosition($TYPE),id);
		} else SynErr(142);
		return typeDecl;
	}

	SubTypeDeclaration  subtype_declaration() {
		SubTypeDeclaration  subTypeDecl;
		Expect(90);
		identifier = identifier();
		Expect(46);
		subType = subtype_indication();
		Expect(115);
		subTypeDecl=new SubTypeDeclaration(toPosition($SUBTYPE),identifier,subType);
		return subTypeDecl;
	}

	ConstantDeclaration  constant_declaration() {
		ConstantDeclaration  constantDecl;
		Expect(26);
		list = identifier_list();
		Expect(122);
		subType = subtype_indication();
		if (la.kind == 112) {
			Get();
			expr = expression();
		}
		Expect(115);
		constantDecl=new ConstantDeclaration(toPosition($CONSTANT),list,subType,expr);
		return constantDecl;
	}

	SignalDeclaration  signal_declaration() {
		SignalDeclaration  signalDecl;
		Expect(85);
		list = identifier_list();
		Expect(122);
		subType = subtype_indication();
		if (la.kind == 22 || la.kind == 75) {
			if (la.kind == 75) {
				Get();
			} else {
				Get();
			}
		}
		if (la.kind == 112) {
			Get();
			expr = expression();
		}
		Expect(115);
		Option<SignalDeclaration.Type> signalType=null;
		if(reg!=null) signalType=new Some(SignalDeclaration.Type.REGISTER);
		else if (bus!=null) signalType=new Some(SignalDeclaration.Type.BUS);
		else signalType=None;
		signalDecl=new SignalDeclaration(toPosition($SIGNAL),list,subType,signalType,expr);
		
		return signalDecl;
	}

	VariableDeclaration  variable_declaration() {
		VariableDeclaration  varDecl;
		if (la.kind == 84) {
			Get();
		}
		Expect(99);
		list = identifier_list();
		Expect(122);
		subType = subtype_indication();
		if (la.kind == 112) {
			Get();
			expr = expression();
		}
		Expect(115);
		varDecl=new VariableDeclaration(toPosition($VARIABLE),$SHARED!=null,list,subType,expr);
		return varDecl;
	}

	FileDeclaration  file_declaration() {
		FileDeclaration  fileDecl;
		Expect(34);
		list = identifier_list();
		Expect(122);
		subType = subtype_indication();
		if (la.kind == 46 || la.kind == 62) {
			if (la.kind == 62) {
				Get();
				file_open_kind_expression = expression();
			}
			Expect(46);
			file_logical_name = expression();
		}
		Expect(115);
		fileDecl=new FileDeclaration(toPosition($FILE),list,subType,file_open_kind_expression,file_logical_name);
		return fileDecl;
	}

	AliasDeclaration  alias_declaration() {
		AliasDeclaration  aliasDecl;
		Expect(11);
		designator = alias_designator();
		if (la.kind == 122) {
			Get();
			subType = subtype_indication();
		}
		Expect(46);
		name = name();
		if (la.kind == 120) {
			signature = signature();
		}
		Expect(115);
		aliasDecl=new AliasDeclaration(toPosition($ALIAS),designator,subType,name,signature);
		return aliasDecl;
	}

	AttributeDeclaration  attribute_declaration() {
		AttributeDeclaration  attributeDecl;
		Expect(17);
		identifier = identifier();
		Expect(122);
		type = type_mark();
		Expect(115);
		attributeDecl=new AttributeDeclaration(toPosition($ATTRIBUTE),identifier,type);
		return attributeDecl;
	}

	AttributeSpecification  attribute_specification() {
		AttributeSpecification  node;
		Expect(17);
		identifier = identifier();
		Expect(60);
		nameList = entity_name_list();
		Expect(122);
		entityClass = entity_class();
		Expect(46);
		expr = expression();
		Expect(115);
		node=new AttributeSpecification(toPosition($ATTRIBUTE),identifier,nameList,entityClass,expr);
		return node;
	}

	DisconnectionSpecification  disconnection_specification() {
		DisconnectionSpecification  disconnectSpec;
		Expect(27);
		if (la.kind == 5 || la.kind == 6 || la.kind == 7) {
			list = selected_name_list();
		} else if (la.kind == 64) {
			Get();
		} else if (la.kind == 12) {
			Get();
		} else SynErr(143);
		Expect(122);
		type = type_mark();
		Expect(10);
		expr = expression();
		Expect(115);
		disconnectSpec= new DisconnectionSpecification(toPosition($DISCONNECT),id==null?new Left(list):new Right(toIdentifier(id)),type,expr);
		return disconnectSpec;
	}

	GroupTemplateDeclaration  group_template_declaration() {
		GroupTemplateDeclaration  groupTemplateDecl;
		ListBuffer<GroupTemplateDeclaration.Element> elements=new ListBuffer<GroupTemplateDeclaration.Element>(); 
		Expect(39);
		identifier = identifier();
		Expect(46);
		Expect(118);
		e1 = entity_class_entry();
		elements.append(e1);
		while (la.kind == 116) {
			Get();
			e2 = entity_class_entry();
			elements.append(e2);
		}
		Expect(119);
		Expect(115);
		groupTemplateDecl=new GroupTemplateDeclaration(toPosition($GROUP),identifier,elements.toList());
		return groupTemplateDecl;
	}

	GroupDeclaration  group_declaration() {
		GroupDeclaration  groupDecl;
		Expect(39);
		identifier = identifier();
		Expect(122);
		selectedName = selected_name();
		Expect(118);
		list = group_constituent_list();
		Expect(119);
		Expect(115);
		groupDecl=new GroupDeclaration(toPosition($GROUP),identifier,selectedName,list);
		return groupDecl;
	}

	SelectedName  selected_name() {
		SelectedName  name;
		ListBuffer<Identifier> parts=new ListBuffer<Identifier>();
		prefix = name_prefix();
		while (la.kind == 131) {
			selectedPart = name_selected_part();
			parts.append(selectedPart);
		}
		prepend.prepend(prefix); name =new SelectedName(parts.toList());
		return name;
	}

	DeclarativeItem  block_declarative_item() {
		DeclarativeItem  node;
		if (StartOf(8)) {
			declOrBody = subprogram_declartion_or_body();
			node=declOrBody;
		} else if (la.kind == 94) {
			typeDecl = type_declaration();
			node=typeDecl;
		} else if (la.kind == 90) {
			subTypeDecl = subtype_declaration();
			node=subTypeDecl;
		} else if (la.kind == 26) {
			constantDecl = constant_declaration();
			node=constantDecl;
		} else if (la.kind == 85) {
			signalDecl = signal_declaration();
			node=signalDecl;
		} else if (la.kind == 84 || la.kind == 99) {
			varDecl = variable_declaration();
			node=varDecl; 
		} else if (la.kind == 34) {
			fileDecl = file_declaration();
			node=fileDecl; 
		} else if (la.kind == 11) {
			aliasDecl = alias_declaration();
			node=aliasDecl; 
		} else if (la.kind == 24) {
			componentDecl = component_declaration();
			node=componentDecl;
		} else if (la.kind == 17) {
			attributeSpec = attribute_specification();
			node=attributeSpec.node; 
		} else if (la.kind == 17) {
			attributeDecl = attribute_declaration();
			node=attributeDecl;
		} else if (la.kind == 35) {
			configSpec = configuration_specification();
			node=configSpec;
		} else if (la.kind == 27) {
			disconnectSpec = disconnection_specification();
			node=disconnectSpec;
		} else if (la.kind == 98) {
			useClause = use_clause();
			node=useClause;
		} else if (la.kind == 39) {
			groupTemplateDecl = group_template_declaration();
			node=groupTemplateDecl;
		} else if (la.kind == 39) {
			groupDecl = group_declaration();
			node=groupDecl;
		} else SynErr(144);
		return node;
	}

	Seq<ConcurrentStatement>  architecture_statement_list() {
		Seq<ConcurrentStatement>  list;
		ListBuffer<ConcurrentStatement> statementList=new ListBuffer<ConcurrentStatement>();
		list=List();
		
		while (StartOf(9)) {
			stmt = architecture_statement();
			statementList.append(stmt);
		}
		list=statementList.toList();
		return list;
	}

	DeclarativeItem  configuration_declarative_item() {
		DeclarativeItem  item;
		if (StartOf(6)) {
		} else if (la.kind == 98) {
			useClause = use_clause();
			node=useClause;
		} else if (la.kind == 17) {
			attributeSpec = attribute_specification();
			node=attributeSpec.node; 
		} else if (la.kind == 39) {
			groupDecl = group_declaration();
			node=groupDecl;
		} else SynErr(145);
		return item;
	}

	BlockConfiguration  block_configuration() {
		BlockConfiguration  blockConfig;
		ListBuffer<UseClause> useClauses=new ListBuffer<UseClause>();
		ListBuffer<AnyRef> configurations=new ListBuffer<AnyRef>();
		
		Expect(35);
		block_specification();
		while (la.kind == 98) {
			useClause = use_clause();
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
		Expect(115);
		blockConfig=new BlockConfiguration(blockConfiguration,useClauses.toList(),configurations.toList());
		return blockConfig;
	}

	void block_specification() {
		Expect(135);
	}

	ComponentConfiguration  component_configuration() {
		ComponentConfiguration  componentConfig;
		Expect(35);
		componentSpec = component_specification();
		if (StartOf(10)) {
			indication = binding_indication();
			Expect(115);
		}
		if (la.kind == 35) {
			blockConfiguration = block_configuration();
		}
		Expect(31);
		Expect(35);
		Expect(115);
		componentConfig=new ComponentConfiguration($component_specification.spec,indication,blockConfiguration);
		return componentConfig;
	}

	Object  component_specification() {
		Object  spec;
		list = instantiation_list();
		Expect(122);
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
		DeclarativeItem  node;
		if (StartOf(8)) {
			declOrBody = subprogram_declartion_or_body();
			node=declOrBody;
		} else if (la.kind == 94) {
			typeDecl = type_declaration();
			node=typeDecl;
		} else if (la.kind == 90) {
			subTypeDecl = subtype_declaration();
			node=subTypeDecl;
		} else if (la.kind == 26) {
			constantDecl = constant_declaration();
			node=constantDecl;
		} else if (la.kind == 85) {
			signalDecl = signal_declaration();
			node=signalDecl; 
		} else if (la.kind == 84 || la.kind == 99) {
			varDecl = variable_declaration();
			node=varDecl; 
		} else if (la.kind == 34) {
			fileDecl = file_declaration();
			node=fileDecl; 
		} else if (la.kind == 11) {
			aliasDecl = alias_declaration();
			node=aliasDecl; 
		} else if (la.kind == 24) {
			componentDecl = component_declaration();
			node=componentDecl;
		} else if (la.kind == 17) {
			attributeDecl = attribute_declaration();
			node=attributeDecl; 
		} else if (la.kind == 17) {
			attributeSpec = attribute_specification();
			node=attributeSpec.node; 
		} else if (la.kind == 27) {
			disconnectSpec = disconnection_specification();
			node=disconnectSpec;
		} else if (la.kind == 98) {
			useClause = use_clause();
			node=useClause;
		} else if (la.kind == 39) {
			groupTemplateDecl = group_template_declaration();
			node=groupTemplateDecl;
		} else if (la.kind == 39) {
			groupDecl = group_declaration();
			node=groupDecl;
		} else SynErr(146);
		return node;
	}

	ComponentDeclaration  component_declaration() {
		ComponentDeclaration  componentDecl;
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
		Expect(115);
		componentDecl=new ComponentDeclaration(toPosition($component),start_identifier,gernicClause,portClause,end_identifier);
		return componentDecl;
	}

	DeclarativeItem  package_body_declarative_item() {
		DeclarativeItem  node;
		if (StartOf(8)) {
			declOrBody = subprogram_declartion_or_body();
			node=declOrBody;
		} else if (la.kind == 94) {
			typeDecl = type_declaration();
			node=typeDecl;
		} else if (la.kind == 90) {
			subTypeDecl = subtype_declaration();
			node=subTypeDecl;
		} else if (la.kind == 26) {
			constantDecl = constant_declaration();
			node=constantDecl;
		} else if (la.kind == 85) {
			signalDecl = signal_declaration();
			node=signalDecl; 
		} else if (la.kind == 84 || la.kind == 99) {
			varDecl = variable_declaration();
			node=varDecl; 
		} else if (la.kind == 34) {
			fileDecl = file_declaration();
			node=fileDecl; 
		} else if (la.kind == 11) {
			aliasDecl = alias_declaration();
			node=aliasDecl; 
		} else if (la.kind == 98) {
			useClause = use_clause();
			node=useClause;
		} else if (la.kind == 17) {
			attributeSpec = attribute_specification();
			node=attributeSpec.node; 
		} else if (la.kind == 39) {
			groupTemplateDecl = group_template_declaration();
			node=groupTemplateDecl;
		} else if (la.kind == 39) {
			groupDecl = group_declaration();
			node=groupDecl;
		} else SynErr(147);
		return node;
	}

	Identifier  designator() {
		Identifier  id;
		if (la.kind == 5 || la.kind == 6) {
			identifier = identifier();
			id=identifier;
		} else if (la.kind == 7) {
			Get();
			id=toIdentifier($STRING_LITERAL);
		} else SynErr(148);
		return id;
	}

	SubProgramDeclaration  subprogram_specification() {
		SubProgramDeclaration  decl;
		if (la.kind == 69) {
			Get();
			designator = designator();
			if (la.kind == 118) {
				Get();
				list = parameter_interface_list_procedure();
				Expect(119);
			}
			decl=new ProcedureDeclaration(toPosition($PROCEDURE),designator,list);
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
			if (la.kind == 118) {
				Get();
				list = parameter_interface_list_function();
				Expect(119);
			}
			Expect(79);
			returnType = type_mark();
			decl=new FunctionDeclaration(toPosition($FUNCTION),i==null,designator,list,returnType);
		} else SynErr(149);
		return decl;
	}

	InterfaceList  parameter_interface_list_procedure() {
		InterfaceList  list;
		ListBuffer<InterfaceList.AbstractInterfaceElement> elements=new ListBuffer<InterfaceList.AbstractInterfaceElement>();
		e1 = interface_element_procedure();
		elements.append(e1);
		while (la.kind == 115) {
			Get();
			e2 = interface_element_procedure();
			elements.append(e2);
		}
		list=new InterfaceList(elements.toList());
		return list;
	}

	InterfaceList  parameter_interface_list_function() {
		InterfaceList  list;
		ListBuffer<InterfaceList.AbstractInterfaceElement> elements=new ListBuffer<InterfaceList.AbstractInterfaceElement>();
		e1 = interface_element_function();
		elements.append(e1);
		while (la.kind == 115) {
			Get();
			e2 = interface_element_function();
			elements.append(e2);
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
		Expect(115);
		subprogramDecl=decl;
		return subprogramDecl;
	}

	DeclarativeItem  subprogram_declarative_item() {
		DeclarativeItem  node;
		if (StartOf(8)) {
			declOrBody = subprogram_declartion_or_body();
			node=declOrBody;
		} else if (la.kind == 94) {
			typeDecl = type_declaration();
			node=typeDecl;
		} else if (la.kind == 90) {
			subTypeDecl = subtype_declaration();
			node=subTypeDecl;
		} else if (la.kind == 26) {
			constantDecl = constant_declaration();
			node=constantDecl;
		} else if (la.kind == 84 || la.kind == 99) {
			varDecl = variable_declaration();
			node=varDecl; 
		} else if (la.kind == 34) {
			fileDecl = file_declaration();
			node=fileDecl; 
		} else if (la.kind == 11) {
			aliasDecl = alias_declaration();
			node=aliasDecl; 
		} else if (la.kind == 17) {
			attributeDecl = attribute_declaration();
			node=attributeDecl;
		} else if (la.kind == 17) {
			attributeSpec = attribute_specification();
			node=attributeSpec.node; 
		} else if (la.kind == 98) {
			useClause = use_clause();
			node=useClause;
		} else if (la.kind == 39) {
			groupTemplateDecl = group_template_declaration();
			node=groupTemplateDecl;
		} else if (la.kind == 39) {
			groupDecl = group_declaration();
			node=groupDecl;
		} else SynErr(150);
		return node;
	}

	Seq<SequentialStatement>  sequential_statement_list() {
		Seq<SequentialStatement>  list;
		ListBuffer<SequentialStatement> tmpList=new ListBuffer<SequentialStatement>();
		list=List();
		
		while (StartOf(12)) {
			stmt = sequential_statement();
			tmpList.append(stmt);
		}
		list=tmpList.toList();
		return list;
	}

	AbstractTypeDeclaration  type_definition(Identifier id,Position pos) {
		AbstractTypeDeclaration  typeDef;
		switch (la.kind) {
		case 118: {
			enumTypeDef = enumeration_type_definition(id,pos);
			typeDef=enumTypeDef; 
			break;
		}
		case 73: {
			intOrFloat = integer_or_floating_point_type_definition(id,pos);
			typeDef=intOrFloat;
			break;
		}
		case 15: {
			arrayTypeDef = array_type_definition(id,pos);
			typeDef=arrayTypeDef;
			break;
		}
		case 74: {
			recordTypeDef = record_type_definition(id,pos);
			typeDef=recordTypeDef;
			break;
		}
		case 9: {
			accessTypeDef = access_type_definition(id,pos);
			typeDef=accessTypeDef;
			break;
		}
		case 34: {
			fileTypeDef = file_type_definition(id,pos);
			typeDef=fileTypeDef;
			break;
		}
		case 71: {
			protectedTypeDecl = protected_type_declaration(id,pos);
			typeDef=protectedTypeDecl;
			break;
		}
		default: SynErr(151); break;
		}
		return typeDef;
	}

	EnumerationTypeDefinition  enumeration_type_definition(Identifier id,Position pos) {
		EnumerationTypeDefinition  enumTypeDef;
		ListBuffer<Identifier> elements=new ListBuffer<Identifier>(); 
		Expect(118);
		e1 = enumeration_literal();
		elements.append(e1);
		while (la.kind == 116) {
			Get();
			e2 = enumeration_literal();
			elements.append(e2);
		}
		Expect(119);
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
		if (unConstraintList.isEmpty) arrayTypeDef=new ConstrainedArrayTypeDefinition($pos,$id,$index_constraint.ranges,$subType.subType);
		else arrayTypeDef=new UnconstrainedArrayTypeDefinition($pos,$id,unConstraintList.toList(),$subType.subType);
		
		return arrayTypeDef;
	}

	RecordTypeDefinition  record_type_definition(Identifier id,Position pos) {
		RecordTypeDefinition  recordTypeDef;
		ListBuffer<RecordTypeDefinition.Element> elements=new ListBuffer<RecordTypeDefinition.Element>(); 
		Expect(74);
		while (la.kind == 5 || la.kind == 6) {
			list = identifier_list();
			Expect(122);
			subType = subtype_indication();
			Expect(115);
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
		subType = subtype_indication();
		accessTypeDef=new AccessTypeDefinition(pos,id,subType);
		return accessTypeDef;
	}

	FileTypeDefinition  file_type_definition(Identifier id,Position pos) {
		FileTypeDefinition  fileTypeDef;
		Expect(34);
		Expect(60);
		type = type_mark();
		fileTypeDef=new FileTypeDefinition(pos,id,type);
		return fileTypeDef;
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
		if (la.kind == 73 || la.kind == 118) {
			constraint = constraint();
		}
		if (n2!=null) subType=new SubTypeIndication(n1,n2,constraint);
		else subType=new SubTypeIndication(None,n1,constraint);
		
		return subType;
	}

	Expression  expression() {
		Expression  expr;
		r2 = relation();
		expr=r1;
		while (StartOf(14)) {
			op = logical_operator();
			r2 = relation();
			expr=new LogicalExpression(op._1,expr,op._2,r2);
		}
		return expr;
	}

	Identifier  alias_designator() {
		Identifier  id;
		if (la.kind == 5 || la.kind == 6) {
			identifier = identifier();
			id=identifier;
		} else if (la.kind == 4) {
			Get();
			id=toIdentifier($CHARACTER_LITERAL);
		} else if (la.kind == 7) {
			Get();
			id=toIdentifier($STRING_LITERAL);
		} else SynErr(152);
		return id;
	}

	Name  name() {
		Name  name;
		ListBuffer<Name.Part> parts=new ListBuffer<Name.Part>();
		prefix = name_prefix();
		name =new Name(prefix,parts.toList());
		return name;
	}

	Signature  signature() {
		Signature  signature;
		Expect(120);
		if (la.kind == 5 || la.kind == 6 || la.kind == 7) {
			list = selected_name_list();
		}
		if (la.kind == 79) {
			Get();
			returnType = type_mark();
		}
		Expect(121);
		signature =new Signature(list,returnType);
		return signature;
	}

	Either<Seq<Tuple2<Identifier,Option<Signature>>>,Identifier>  entity_name_list() {
		Either<Seq<Tuple2<Identifier,Option<Signature>>>,Identifier>  list;
		if (StartOf(15)) {
			elements=new ListBuffer<Tuple2<Identifier,Option<Signature>>>(); 
			designator = entity_designator();
			elements.append(designator); 
			while (la.kind == 116) {
				Get();
				designator = entity_designator();
				elements.append(designator);
			}
			list=Left(elements.toList());
		} else if (la.kind == 64) {
			Get();
			list=new Right<Identifier>(toIdentifier($OTHERS));
		} else if (la.kind == 12) {
			Get();
			list=new Right<Identifier>(toIdentifier($ALL));
		} else SynErr(153);
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
		default: SynErr(154); break;
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
			id=toIdentifier($CHARACTER_LITERAL);
		} else if (la.kind == 7) {
			Get();
			id=toIdentifier($STRING_LITERAL);
		} else SynErr(155);
		if (la.kind == 120) {
			sig = signature();
		}
		designator=new Tuple2<Identifier,Option<Signature>>(id,new Option<Signature>(sig)); 
		return designator;
	}

	ConfigurationSpecification  configuration_specification() {
		ConfigurationSpecification  configSpec;
		Expect(35);
		componentSpec = component_specification();
		indication = binding_indication();
		Expect(115);
		configSpec= new ConfigurationSpecification(toPosition($FOR));
		return configSpec;
	}

	Either<Seq<Identifier>,Identifier>  instantiation_list() {
		Either<Seq<Identifier>,Identifier>  list;
		if (la.kind == 5 || la.kind == 6) {
			list = identifier_list();
			list=Left(list);
		} else if (la.kind == 64) {
			Get();
			list=Right(toIdentifier($OTHERS));
		} else if (la.kind == 12) {
			Get();
			list=Right(toIdentifier($ALL));
		} else SynErr(156);
		return list;
	}

	void entity_aspect() {
		if (la.kind == 32) {
			Get();
			entity_name = selected_name();
			if (la.kind == 118) {
				Get();
				architecture_identifier = identifier();
				Expect(119);
			}
		} else if (la.kind == 25) {
			Get();
			configuration_name = selected_name();
		} else if (la.kind == 62) {
			Get();
		} else SynErr(157);
	}

	AssociationList  generic_map_aspect() {
		AssociationList  list;
		Expect(38);
		Expect(52);
		Expect(118);
		associationList = association_list();
		Expect(119);
		list=associationList; 
		return list;
	}

	AssociationList  port_map_aspect() {
		AssociationList  list;
		Expect(67);
		Expect(52);
		Expect(118);
		associationList = association_list();
		Expect(119);
		list=associationList; 
		return list;
	}

	Seq<SelectedName>  selected_name_list() {
		Seq<SelectedName>  list;
		ListBuffer<SelectedName> tmpList=new ListBuffer<SelectedName>();
		list=List();
		
		n1 = selected_name();
		tmpList.append(n1);
		while (la.kind == 116) {
			Get();
			n2 = selected_name();
			tmpList.append(n2);
		}
		list=tmpList.toList();
		return list;
	}

	GroupTemplateDeclaration.Element  entity_class_entry() {
		GroupTemplateDeclaration.Element  entry;
		entityClass = entity_class();
		if (la.kind == 113) {
			Get();
		}
		entry = new GroupTemplateDeclaration.Element(entityClass,$BOX!=null);
		return entry;
	}

	Seq<Either<Name,Identifier>>  group_constituent_list() {
		Seq<Either<Name,Identifier>>  list;
		ListBuffer<Either<Name,Identifier>> elements=new ListBuffer<Either<Name,Identifier>>(); 
		c1 = group_constituent();
		elements.append(c1); 
		while (la.kind == 116) {
			Get();
			c2 = group_constituent();
			elements.append(c2);
		}
		list=elements.toList();
		return list;
	}

	Either<Name,Identifier>  group_constituent() {
		Either<Name,Identifier>  constituent;
		if (la.kind == 5 || la.kind == 6 || la.kind == 7) {
			name = name();
			constituent=Left(name);
		} else if (la.kind == 4) {
			Get();
			constituent=Right(toIdentifier($CHARACTER_LITERAL));
		} else SynErr(158);
		return constituent;
	}

	Identifier  enumeration_literal() {
		Identifier  id;
		if (la.kind == 5 || la.kind == 6) {
			identifier = identifier();
			id=identifier;
		} else if (la.kind == 4) {
			Get();
			id=toIdentifier($CHARACTER_LITERAL);
		} else SynErr(159);
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
		Expect(115);
		while (la.kind == 5 || la.kind == 6) {
			identifier = identifier();
			Expect(129);
			literal = physical_literal();
			Expect(115);
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
		Expect(137);
		return x;
	}

	SelectedName  index_subtype_definition() {
		SelectedName  typeMark;
		type = type_mark();
		Expect(73);
		Expect(113);
		typeMark=type;
		return typeMark;
	}

	DeclarativeItem  protected_type_declarative_item() {
		DeclarativeItem  node;
		if (StartOf(8)) {
			declOrBody = subprogram_declartion_or_body();
			node=declOrBody;
		} else if (la.kind == 17) {
			attributeSpec = attribute_specification();
			node=attributeSpec.node; 
		} else if (la.kind == 98) {
			useClause = use_clause();
			node=useClause;
		} else SynErr(160);
		return node;
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

	DeclarativeItem  protected_type_body_declarative_item() {
		DeclarativeItem  node;
		if (StartOf(8)) {
			declOrBody = subprogram_declartion_or_body();
			node=declOrBody;
		} else if (la.kind == 94) {
			typeDecl = type_declaration();
			node=typeDecl;
		} else if (la.kind == 90) {
			subTypeDecl = subtype_declaration();
			node=subTypeDecl;
		} else if (la.kind == 26) {
			constantDecl = constant_declaration();
			node=constantDecl;
		} else if (la.kind == 84 || la.kind == 99) {
			varDecl = variable_declaration();
			node=varDecl; 
		} else if (la.kind == 34) {
			fileDecl = file_declaration();
			node=fileDecl; 
		} else if (la.kind == 11) {
			aliasDecl = alias_declaration();
			node=aliasDecl; 
		} else if (la.kind == 17) {
			attributeDecl = attribute_declaration();
			node=attributeDecl;
		} else if (la.kind == 17) {
			attributeSpec = attribute_specification();
			node=attributeSpec.node; 
		} else if (la.kind == 98) {
			useClause = use_clause();
			node=useClause;
		} else if (la.kind == 39) {
			groupTemplateDecl = group_template_declaration();
			node=groupTemplateDecl;
		} else if (la.kind == 39) {
			groupDecl = group_declaration();
			node=groupDecl;
		} else SynErr(161);
		return node;
	}

	Either<Range,Seq<DiscreteRange>>  constraint() {
		Either<Range,Seq<DiscreteRange>>  constraint;
		if (la.kind == 73) {
			rangeContraint = range_constraint();
			constraint =Left(rangeContraint);
		} else if (la.kind == 118) {
			ranges = index_constraint();
			constraint =Right(ranges);
		} else SynErr(162);
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
		} else SynErr(163);
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
		Expect(118);
		d1 = discrete_range();
		list.append(d1);
		while (la.kind == 116) {
			Get();
			d2 = discrete_range();
			list.append(d2);
		}
		Expect(119);
		ranges = list.toList();
		return ranges;
	}

	DiscreteRange  discrete_range() {
		DiscreteRange  discreteRange;
		range = range();
		discreteRange=new DiscreteRange(Left(range));
		return discreteRange;
	}

	ConcurrentStatement  architecture_statement() {
		ConcurrentStatement  concurrentStmt;
		if (la.kind == 5 || la.kind == 6) {
			label = label_colon();
			stmt = architecture_statement_optional_label(label);
			concurrentStmt=stmt;
		} else if (StartOf(9)) {
			stmt = architecture_statement_optional_label(label);
			concurrentStmt=stmt;
		} else SynErr(164);
		return concurrentStmt;
	}

	Identifier  label_colon() {
		Identifier  label;
		id = identifier();
		Expect(122);
		label=id;
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
		} else SynErr(165);
		return concurrentStmt;
	}

	ConcurrentStatement  architecture_statement_with_label(Identifier label) {
		ConcurrentStatement  concurrentStmt;
		if (la.kind == 38 || la.kind == 67 || la.kind == 115) {
			stmt = component_instantiation_statement(label);
			concurrentStmt=stmt;
		} else if (la.kind == 19) {
			stmt = block_statement(label);
			concurrentStmt=stmt;
		} else if (la.kind == 35 || la.kind == 41) {
			stmt = generate_statement(label);
			concurrentStmt=stmt;
		} else SynErr(166);
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
		Expect(115);
		stmt=new ComponentInstantiationStatement(toPosition(firstToken),label,componentType,$n.name_,$architecture_identifier.id,genericMap,portMap);
		return stmt;
	}

	BlockStatement  block_statement(Identifier label) {
		BlockStatement  blockStmt;
		ListBuffer<DeclarativeItem> declItem=new ListBuffer<DeclarativeItem>(); 
		Expect(19);
		if (la.kind == 118) {
			Get();
			guard_expression = expression();
			Expect(119);
		}
		if (la.kind == 46) {
			Get();
		}
		if (la.kind == 38) {
			genericClause = generic_clause();
			if (la.kind == 38) {
				genericMap = generic_map_aspect();
				Expect(115);
			}
		}
		if (la.kind == 67) {
			portClause = port_clause();
			if (la.kind == 67) {
				portMap = port_map_aspect();
				Expect(115);
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
		Expect(115);
		blockStmt=new BlockStatement(toPosition($block),label,guard_expression,genericClause,genericMap,portClause,portMap,declItems.toList(),statementList,end_block_label);
		return blockStmt;
	}

	ConcurrentStatement  generate_statement(Identifier label) {
		ConcurrentStatement  generateStmt;
		if (la.kind == 35) {
			forGenerateStmt = for_generate_statement(label);
			generateStmt=forGenerateStmt;
		} else if (la.kind == 41) {
			ifGenerateStmt = if_generate_statement(label);
			generateStmt=ifGenerateStmt;
		} else SynErr(167);
		return generateStmt;
	}

	ProcessStatement  process_statement(Identifier label,Boolean postponed) {
		ProcessStatement  processStmt;
		ListBuffer<DeclarativeItem> declItem=new ListBuffer<DeclarativeItem>(); 
		Expect(70);
		if (la.kind == 118) {
			Get();
			name_list = name_list();
			Expect(119);
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
		Expect(115);
		processStmt=new ProcessStatement(toPosition($process),label,postponed,name_list,declItem.toList(),sequentialStatements,end_process_label);
		return processStmt;
	}

	ConcurrentAssertionStatement  concurrent_assertion_statement(Identifier label,Boolean postponed) {
		ConcurrentAssertionStatement  assertStmt;
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
		Expect(115);
		assertStmt=new ConcurrentAssertionStatement(toPosition($ASSERT),label,postponed,expr,report_expression,severity_expression);
		return assertStmt;
	}

	ConcurrentProcedureCallStatement  concurrent_procedure_call_statement(Identifier label,Boolean postponed) {
		ConcurrentProcedureCallStatement  procedureCallStmt;
		procedure_name = selected_name();
		if (la.kind == 118) {
			Get();
			associationList = association_list();
			Expect(119);
		}
		Expect(115);
		procedureCallStmt=new ConcurrentProcedureCallStatement(label,postponed,procedure_name,associationList);
		return procedureCallStmt;
	}

	AssociationList  association_list() {
		AssociationList  list;
		ListBuffer<AssociationList.Element> elements=new ListBuffer<AssociationList.Element>();
		e1 = association_element();
		elements.append(e1);
		while (la.kind == 116) {
			Get();
			e2 = association_element();
			elements.append(e2);
		}
		list=new AssociationList(elements.toList());
		return list;
	}

	Seq<Name>  name_list() {
		Seq<Name>  list;
		ListBuffer<Name> tmpList=new ListBuffer<Name>();
		list=List();
		
		n1 = name();
		tmpList.append(n1);
		while (la.kind == 116) {
			Get();
			n2 = name();
			tmpList.append(n2);
		}
		list=tmpList.toList();
		return list;
	}

	DeclarativeItem  process_declarative_item() {
		DeclarativeItem  node;
		if (StartOf(8)) {
			declOrBody = subprogram_declartion_or_body();
			node=declOrBody;
		} else if (la.kind == 94) {
			typeDecl = type_declaration();
			node=typeDecl;
		} else if (la.kind == 90) {
			subTypeDecl = subtype_declaration();
			node=subTypeDecl;
		} else if (la.kind == 26) {
			constantDecl = constant_declaration();
			node=constantDecl;
		} else if (la.kind == 84 || la.kind == 99) {
			varDecl = variable_declaration();
			node=varDecl; 
		} else if (la.kind == 34) {
			fileDecl = file_declaration();
			node=fileDecl; 
		} else if (la.kind == 11) {
			aliasDecl = alias_declaration();
			node=aliasDecl; 
		} else if (la.kind == 98) {
			useClause = use_clause();
			node=useClause;
		} else if (la.kind == 17) {
			attributeSpec = attribute_specification();
			node=attributeSpec.node; 
		} else if (la.kind == 17) {
			attributeDecl = attribute_declaration();
			node=attributeDecl;
		} else if (la.kind == 39) {
			groupTemplateDecl = group_template_declaration();
			node=groupTemplateDecl;
		} else if (la.kind == 39) {
			groupDecl = group_declaration();
			node=groupDecl;
		} else SynErr(168);
		return node;
	}

	Expression  condition() {
		Expression  con;
		expr = expression();
		con=expr;
		return con;
	}

	void concurrent_signal_assignment_statement(ConcurrentSignalAssignmentStatement node, Identifier label,Boolean postponed) {
		if (StartOf(16)) {
			conditional = conditional_signal_assignment(label,postponed);
			node=conditional;
		} else if (la.kind == 103) {
			selected = selected_signal_assignment(label,postponed);
			node=selected;
		} else SynErr(169);
	}

	ConcurrentConditionalSignalAssignment  conditional_signal_assignment(Identifier label,Boolean postponed) {
		ConcurrentConditionalSignalAssignment  signalAssignment;
		ListBuffer<ConcurrentConditionalSignalAssignment.When> elements=new ListBuffer<ConcurrentConditionalSignalAssignment.When>();
		target = target();
		Expect(108);
		if (la.kind == 40) {
			Get();
		}
		if (la.kind == 44 || la.kind == 76 || la.kind == 93) {
			delay = delay_mechanism();
		}
		conditional_waveforms(elements);
		Expect(115);
		signalAssignment=new ConcurrentConditionalSignalAssignment(toPosition($LEQ),label,postponed,target,$GUARDED!=null,delay,elements.toList());
		return signalAssignment;
	}

	ConcurrentSelectedSignalAssignment  selected_signal_assignment(Identifier label,Boolean postponed) {
		ConcurrentSelectedSignalAssignment  signalAssignment;
		ListBuffer<ConcurrentSelectedSignalAssignment.When> elements=new ListBuffer<ConcurrentSelectedSignalAssignment.When>(); 
		Expect(103);
		expr = expression();
		Expect(82);
		target = target();
		Expect(108);
		if (la.kind == 40) {
			Get();
		}
		if (la.kind == 44 || la.kind == 76 || la.kind == 93) {
			delay = delay_mechanism();
		}
		s1 = selected_waveform();
		elements.append(s1);
		while (la.kind == 116) {
			Get();
			s2 = selected_waveform();
			elements.append(s2);
		}
		Expect(115);
		signalAssignment=new ConcurrentSelectedSignalAssignment(toPosition($WITH),label,postponed,expr,target,$GUARDED!=null,delay,elements.toList());
		return signalAssignment;
	}

	Target  target() {
		Target  target;
		if (la.kind == 5 || la.kind == 6 || la.kind == 7) {
			name = name();
			target = new Target(Left(name));
		} else if (la.kind == 118) {
			aggregate = aggregate();
			target = new Target(Right(aggregate));
		} else SynErr(170);
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
			
		} else SynErr(171);
		return mechanism;
	}

	void conditional_waveforms(ListBuffer elements) {
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
			while (la.kind == 116) {
				Get();
				e2 = waveform_element();
				elements.append(e2);
			}
		} else if (la.kind == 95) {
			Get();
			waveForm=new Waveform(toPosition(firstToken),elements.toList());
		} else SynErr(172);
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
		while (la.kind == 130) {
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
		Expect(118);
		e1 = element_association();
		elements.append(e1);
		while (la.kind == 116) {
			Get();
			e2 = element_association();
			elements.append(e2);
		}
		Expect(119);
		aggregate =new Aggregate(elements.toList());
		return aggregate;
	}

	ForGenerateStatement  for_generate_statement(Identifier label) {
		ForGenerateStatement  forGenerateStmt;
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
		Expect(115);
		forGenerateStmt=new ForGenerateStatement(toPosition($FOR),label,loopIdentifier,discreteRange,$body.blockItems,$body.statementList,end_generate_label);
		return forGenerateStmt;
	}

	IfGenerateStatement  if_generate_statement(Identifier label) {
		IfGenerateStatement  ifGenerateStmt;
		Expect(41);
		expr = condition();
		Expect(37);
		body = generate_statement_body();
		Expect(31);
		Expect(37);
		if (la.kind == 5 || la.kind == 6) {
			end_generate_label = identifier();
		}
		Expect(115);
		ifGenerateStmt=new IfGenerateStatement(toPosition($IF),label,expr,body._1,body._2,end_generate_label);
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
		SequentialStatement  stmt;
		if (la.kind == 5 || la.kind == 6) {
			label = label_colon();
		}
		switch (la.kind) {
		case 100: {
			waitStmt = wait_statement(label);
			stmt=waitStmt;
			break;
		}
		case 16: {
			assertStmt = assertion_statement(label);
			stmt=assertStmt;
			break;
		}
		case 78: {
			reportStmt = report_statement(label);
			stmt=reportStmt;
			break;
		}
		case 41: {
			ifStmt = if_statement(label);
			stmt=ifStmt;
			break;
		}
		case 23: {
			caseStmt = case_statement(label);
			stmt=caseStmt;
			break;
		}
		case 35: case 51: case 102: {
			loopStmt = loop_statement(label);
			stmt=loopStmt;
			break;
		}
		case 56: {
			nextStmt = next_statement(label);
			stmt=nextStmt;
			break;
		}
		case 33: {
			exitStmt = exit_statement(label);
			stmt=exitStmt;
			break;
		}
		case 79: {
			returnStmt = return_statement(label);
			stmt=returnStmt;
			break;
		}
		case 59: {
			nullStmt = null_statement(label);
			stmt=nullStmt;
			break;
		}
		case 5: case 6: case 7: {
			procedureCallStmt = procedure_call_statement(label);
			stmt=procedureCallStmt;
			break;
		}
		default: SynErr(173); break;
		}
		return stmt;
	}

	WaitStatement  wait_statement(Identifier label) {
		WaitStatement  waitStmt;
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
		Expect(115);
		waitStmt=new WaitStatement(toPosition($WAIT),label,name_list,untilExpr,forExpression);
		return waitStmt;
	}

	AssertStatement  assertion_statement(Identifier label) {
		AssertStatement  assertStmt;
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
		Expect(115);
		assertStmt=new AssertStatement(toPosition($ASSERT),label,expr,report_expression,severity_expression);
		return assertStmt;
	}

	ReportStatement  report_statement(Identifier label) {
		ReportStatement  reportStmt;
		Expect(78);
		report_expression = expression();
		if (la.kind == 83) {
			Get();
			severity_expression = expression();
		}
		Expect(115);
		reportStmt=new ReportStatement(toPosition($REPORT),label,report_expression,severity_expression);
		return reportStmt;
	}

	IfStatement  if_statement(Identifier label) {
		IfStatement  ifStmt;
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
		Expect(115);
		ifStmt=new IfStatement(toPosition($ifToken),label,ifList.toList(),else_sequential_statement,end_if_label);
		return ifStmt;
	}

	CaseStatement  case_statement(Identifier label) {
		CaseStatement  caseStmt;
		ListBuffer<CaseStatement.When> alternatives=new ListBuffer<CaseStatement.When>(); 
		Expect(23);
		expr = expression();
		Expect(46);
		while (la.kind == 101) {
			Get();
			choices = choices();
			Expect(110);
			sequentialStatements = sequential_statement_list();
			alternatives.append(new CaseStatement.When(choices,sequentialStatements));
		}
		Expect(31);
		Expect(23);
		if (la.kind == 5 || la.kind == 6) {
			end_case_label = identifier();
		}
		Expect(115);
		caseStmt=new CaseStatement(toPosition($caseToken),label,expr,alternatives.toList(),end_case_label);
		return caseStmt;
	}

	SequentialStatement  loop_statement(Identifier label) {
		SequentialStatement  loopStmt;
		Token firstToken=la;
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
		Expect(115);
		Position position=toPosition(firstToken);
		if (stmtType!=null){
			if (stmtType instanceof Left) loopStmt=new WhileStatement(position,label,((Left)stmtType).a,sequentialStatements,end_loop_label);
			//TODO case Right((identifier,discreteRange)) =>new ForStatement(position,label,identifier,discreteRange,sequentialStatements,end_loop_label.id);
		}else loopStmt=new LoopStatement(position,label,sequentialStatements,end_loop_label.id);
		
		return loopStmt;
	}

	NextStatement  next_statement(Identifier label) {
		NextStatement  nextStmt;
		Expect(56);
		if (la.kind == 5 || la.kind == 6) {
			identifier = identifier();
		}
		if (la.kind == 101) {
			Get();
			expr = condition();
		}
		Expect(115);
		nextStmt=new NextStatement(toPosition($NEXT),label,identifier,expr);
		return nextStmt;
	}

	ExitStatement  exit_statement(Identifier label) {
		ExitStatement  exitStmt;
		Expect(33);
		if (la.kind == 5 || la.kind == 6) {
			identifier = identifier();
		}
		if (la.kind == 101) {
			Get();
			expr = condition();
		}
		Expect(115);
		exitStmt=new ExitStatement(toPosition($EXIT),label,identifier,expr);
		return exitStmt;
	}

	ReturnStatement  return_statement(Identifier label) {
		ReturnStatement  returnStmt;
		Expect(79);
		if (StartOf(19)) {
			expr = expression();
		}
		Expect(115);
		returnStmt=new ReturnStatement(toPosition($RETURN),label,expr);
		return returnStmt;
	}

	NullStatement  null_statement(Identifier label) {
		NullStatement  nullStmt;
		Expect(59);
		Expect(115);
		nullStmt=new NullStatement(toPosition($NULL),label);
		return nullStmt;
	}

	ProcedureCallStatement  procedure_call_statement(Identifier label) {
		ProcedureCallStatement  procedureCallStmt;
		procedure_name = selected_name();
		if (la.kind == 118) {
			Get();
			associationList = association_list();
			Expect(119);
		}
		Expect(115);
		procedureCallStmt=new ProcedureCallStatement(label,procedure_name,associationList);
		return procedureCallStmt;
	}

	SignalAssignmentStatement  signal_assignment_statement(Identifier label) {
		SignalAssignmentStatement  signalAssignStmt;
		target = target();
		Expect(108);
		if (la.kind == 44 || la.kind == 76 || la.kind == 93) {
			delay = delay_mechanism();
		}
		waveform = waveform();
		Expect(115);
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
		Expect(112);
		expr = expression();
		Expect(115);
		varAssignStmt=new SimpleVariableAssignmentStatement(toPosition($VAR_ASSIGN),label,target,expr);
		return varAssignStmt;
	}

	Either<Expression,Tuple2<Identifier,DiscreteRange>>  iteration_scheme() {
		Either<Expression,Tuple2<Identifier,DiscreteRange>>  scheme;
		if (la.kind == 102) {
			Get();
			expr = condition();
			scheme=Left(expr);
		} else if (la.kind == 35) {
			Get();
			identifier = identifier();
			Expect(43);
			discreteRange = discrete_range();
			scheme=Right(Tuple2(identifier,discreteRange));
		} else SynErr(174);
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
		} else SynErr(175);
		return element;
	}

	InterfaceList.InterfaceFileDeclaration  interface_file_declaration() {
		InterfaceList.InterfaceFileDeclaration  fileElement;
		Expect(34);
		list = identifier_list();
		Expect(122);
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
		} else SynErr(176);
		return element;
	}

	InterfaceList.InterfaceSignalDeclaration  interface_signal_declaration_function() {
		InterfaceList.InterfaceSignalDeclaration  signalElement;
		if (la.kind == 85) {
			Get();
		}
		list = identifier_list();
		Expect(122);
		if (la.kind == 43) {
			Get();
		}
		subType = subtype_indication();
		if (la.kind == 22) {
			Get();
		}
		if (la.kind == 112) {
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
		} else SynErr(177);
		return mode;
	}

	InterfaceList.InterfaceVariableDeclaration  interface_variable_declaration() {
		InterfaceList.InterfaceVariableDeclaration  varElement;
		if (la.kind == 99) {
			Get();
		}
		list = identifier_list();
		Expect(122);
		if (StartOf(7)) {
			mode = interface_mode();
		}
		subType = subtype_indication();
		if (la.kind == 112) {
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
		} else SynErr(178);
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
		} else SynErr(179);
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
		case 129: {
			Position pos=toPosition(la);
			Relation.Operator.Value relOp=null;
			
			Get();
			relOp=Relation.Operator.EQ;
			break;
		}
		case 111: {
			Get();
			relOp=Relation.Operator.NEQ;
			break;
		}
		case 127: {
			Get();
			relOp=Relation.Operator.LT;
			break;
		}
		case 108: {
			Get();
			relOp=Relation.Operator.LEQ;
			break;
		}
		case 128: {
			Get();
			relOp=Relation.Operator.GT;
			break;
		}
		case 109: {
			Get();
			relOp=Relation.Operator.GEQ;
			op=new Tuple2<Relation.Operator.Value,Position>(relOp,pos);
			break;
		}
		default: SynErr(180); break;
		}
		return op;
	}

	Expression  simple_expression() {
		Expression  simpleExpr;
		if (la.kind == 125 || la.kind == 126) {
			sign = sign();
		}
		t1 = term();
		if (s!=null) simpleExpr=new SimpleExpression(s._2,s._1,t1,None,None); else simpleExpr=t1;
		while (la.kind == 117 || la.kind == 125 || la.kind == 126) {
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
		default: SynErr(181); break;
		}
		return op;
	}

	Tuple2<SimpleExpression.SignOperator.Value,Position>  sign() {
		Tuple2<SimpleExpression.SignOperator.Value,Position>  op;
		if (la.kind == 125) {
			SimpleExpression.SignOperator.Value signOp=null;
			Position pos=toPosition(la);
			
			Get();
			signOp=SimpleExpression.SignOperator.PLUS;
		} else if (la.kind == 126) {
			Get();
			signOp=SimpleExpression.SignOperator.MINUS;
			op=new Tuple2<SimpleExpression.SignOperator.Value,Position>(signOp,pos);
		} else SynErr(182);
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
		if (la.kind == 125) {
			SimpleExpression.AddOperator.Value addOp=null;
			Position pos=toPosition(la);
			
			Get();
			addOp=SimpleExpression.AddOperator.PLUS;
		} else if (la.kind == 126) {
			Get();
			addOp=SimpleExpression.AddOperator.MINUS;
		} else if (la.kind == 117) {
			Get();
			addOp=SimpleExpression.AddOperator.AMPERSAND;
			op=new Tuple2<SimpleExpression.AddOperator.Value,Position>(addOp,pos);
		} else SynErr(183);
		return op;
	}

	Tuple2<Term.Operator.Value,Position>  multiplying_operator() {
		Tuple2<Term.Operator.Value,Position>  op;
		if (la.kind == 123) {
			Term.Operator.Value mulOp=null;
			Position pos=toPosition(la);
			
			Get();
			mulOp=Term.Operator.MUL;
		} else if (la.kind == 124) {
			Get();
			mulOp=Term.Operator.DIV;
		} else if (la.kind == 53) {
			Get();
			mulOp=Term.Operator.MOD;
		} else if (la.kind == 77) {
			Get();
			mulOp=Term.Operator.REM;
			op=new Tuple2<SimpleExpression.AddOperator.Value,Position>(mulOp,pos);
		} else SynErr(184);
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
		} else SynErr(185);
		return factor;
	}

	Expression  primary() {
		Expression  p;
		if (StartOf(25)) {
		} else if (la.kind == 55) {
			allocator.newExpression = allocator();
			p=allocator.newExpression;
		} else if (la.kind == 118) {
			aggregate = aggregate();
			p=new AggregateExpression(aggregate);
		} else SynErr(186);
		return p;
	}

	Expression  allocator() {
		Expression  newExpression;
		Expect(55);
		selectedName = selected_name();
		if (la.kind == 3) {
			expr = qualified_expression(selectedName);
			newExpression=new NewExpression(toPosition($NEW),Left(expr));
		} else if (StartOf(26)) {
			if (la.kind == 118) {
				constraint = index_constraint();
			}
			newExpression=new NewExpression(toPosition($NEW),Right(new SubTypeIndication(None,$selected_name.name_,Right(ranges))));
		} else SynErr(187);
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
		if (la.kind == 118) {
			Get();
			parameter_association_list = association_list();
			Expect(119);
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
		} else SynErr(188);
		return id;
	}

	Name.SelectedPart  name_selected_part() {
		Name.SelectedPart  part;
		Expect(131);
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
		} else SynErr(189);
		return part;
	}

	Name.Part  name_part() {
		Name.Part  part;
		if (la.kind == 131) {
			selectedPart = name_selected_part();
			part = selectedPart;
		} else if (la.kind == 3 || la.kind == 120) {
			attributePart = name_attribute_part();
			part = attributePart;
		} else if (la.kind == 118) {
			slicePart = name_slice_part();
			part = slicePart;
		} else SynErr(190);
		return part;
	}

	Name.AttributePart  name_attribute_part() {
		Name.AttributePart  part;
		if (la.kind == 120) {
			signature = signature();
		}
		Expect(3);
		return part;
	}

	Name.SlicePart  name_slice_part() {
		Name.SlicePart  part;
		Expect(118);
		discreteRange = discrete_range();
		Expect(119);
		part=new Name.SlicePart(discreteRange);
		return part;
	}

	Name.IndexPart  name_indexed_part() {
		Name.IndexPart  part;
		ListBuffer<Expression> indexes=new ListBuffer<Expression>(); 
		Expect(118);
		e1 = expression();
		indexes.append(e1);
		while (la.kind == 116) {
			Get();
			e2 = expression();
			indexes.append(e2);
		}
		Expect(119);
		part=new Name.IndexPart(indexes.toList());
		return part;
	}

	Object  choice() {
		Object  x;
		Expect(136);
		return x;
	}

	Object  element_association() {
		Object  x;
		Expect(137);
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
		{T,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x},
		{x,x,x,x, x,x,x,x, x,x,x,x, x,x,T,x, x,x,x,x, x,x,x,x, x,T,x,x, x,x,x,x, T,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, T,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,T,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,T,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x},
		{x,x,x,x, x,x,x,x, x,x,x,T, x,x,x,x, x,T,x,x, x,x,x,x, x,x,T,T, x,x,x,x, x,x,T,x, T,x,x,T, x,x,T,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,T,x,x, T,x,x,x, x,x,x,x, x,x,x,x, T,T,x,x, x,x,T,x, x,x,T,x, x,x,T,T, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x},
		{x,x,x,x, x,x,x,x, x,x,x,T, x,x,x,x, x,T,x,x, x,x,x,x, T,x,T,T, x,x,x,x, x,x,T,T, T,x,x,T, x,x,T,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,T,x,x, T,x,x,x, x,x,x,x, x,x,x,x, T,T,x,x, x,x,T,x, x,x,T,x, x,x,T,T, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x},
		{x,x,x,x, x,x,x,x, x,x,x,T, x,x,x,x, x,T,x,x, x,x,x,x, T,x,T,T, x,x,x,x, x,x,T,x, T,x,x,T, x,x,T,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,T,x,x, T,x,x,x, x,x,x,x, x,x,x,x, T,T,x,x, x,x,T,x, x,x,T,x, x,x,T,T, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x},
		{x,x,x,x, x,x,x,x, x,x,x,T, x,x,x,x, x,T,x,x, x,x,x,x, x,x,T,x, x,x,x,x, x,x,T,x, T,x,x,T, x,x,T,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,T,x,x, T,x,x,x, x,x,x,x, x,x,x,x, T,T,x,x, x,x,T,x, x,x,T,x, x,x,T,T, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x},
		{x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,T,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,T, x,x,x,T, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,T,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x},
		{x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,T,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,T, x,T,x,x, x,T,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,T,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x},
		{x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, T,x,x,x, x,x,T,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,T,x,x, T,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x},
		{x,x,x,x, x,T,T,T, x,x,x,x, x,x,x,x, T,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, T,x,T,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x},
		{x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,T,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,T, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,T,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,T, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x},
		{x,x,x,x, x,x,x,x, x,x,x,T, x,x,x,x, x,T,x,x, x,x,x,x, x,x,T,x, x,x,x,x, x,x,T,x, T,x,x,T, x,x,T,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,T,x,x, T,x,x,x, x,x,x,x, x,x,x,x, T,x,x,x, x,x,T,x, x,x,T,x, x,x,T,T, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x},
		{x,x,x,x, x,T,T,T, x,x,x,x, x,x,x,x, T,x,x,x, x,x,x,T, x,x,x,x, x,x,x,x, x,T,x,T, x,x,x,x, x,T,x,x, x,x,x,x, x,x,x,T, x,x,x,x, T,x,x,T, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,T,T, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, T,x,T,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x},
		{x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,T,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, T,x,x,x, x,x,T,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,T,x,x, T,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,T,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x},
		{x,x,x,x, x,x,x,x, x,x,x,x, x,T,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,T, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, T,T,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x},
		{x,x,x,x, T,T,T,T, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x},
		{x,x,x,x, x,T,T,T, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,T,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x},
		{x,x,x,x, x,x,x,x, T,x,T,x, x,T,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,T,x,T, x,x,T,x, x,x,x,T, x,x,x,x, x,x,x,x, x,x,x,x, x,T,x,x, T,T,x,x, x,x,T,T, T,T,x,x, x,x,x,x, x,x,x,x, x,T,x,x, T,T,T,x, T,T,x,T, x,x,x,T, T,T,T,x, x,x,x,T, T,T,T,T, T,T,x,x, x,x,x,x, x,x,x,x},
		{x,x,x,x, x,x,x,x, x,x,x,T, x,x,x,x, x,T,T,x, x,x,x,x, T,x,T,T, x,x,x,x, x,x,T,T, T,x,x,T, x,x,T,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,T,x,x, T,x,x,x, x,x,x,x, x,x,x,x, T,T,x,x, x,x,T,x, x,x,T,x, x,x,T,T, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x},
		{x,x,x,x, x,x,x,x, T,x,x,x, x,T,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,T,x,T, x,x,T,x, x,x,x,T, x,x,x,x, x,x,x,x, x,x,x,x, x,T,x,x, T,T,x,x, x,x,T,T, T,T,x,x, x,x,x,x, x,x,x,x, x,x,x,x, T,T,T,x, T,T,x,T, x,x,x,T, x,T,T,x, x,x,x,T, T,T,T,T, T,T,x,x, x,x,x,x, x,x,x,x},
		{x,x,x,x, x,x,x,x, T,x,x,x, x,T,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,T,x,T, x,x,T,x, x,x,x,T, x,x,x,x, x,x,x,x, x,x,x,x, x,T,x,x, T,T,x,x, x,x,T,T, T,T,x,x, x,x,x,x, x,x,x,x, x,x,x,x, T,T,T,x, T,T,x,T, x,x,x,x, x,T,T,x, x,x,x,T, T,T,T,T, T,T,x,x, x,x,x,x, x,x,x,x},
		{x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, T,T,x,T, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,T, T,T,x,x, x,x,x,x, x,x,x,x},
		{x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, T,T,x,x, x,x,T,T, T,T,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x},
		{x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,T,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,T,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,T, T,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x},
		{x,x,x,x, x,x,x,x, x,x,T,x, x,T,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,T,x,x, x,x,x,T, x,T,x,x, x,x,x,x, T,x,T,x, x,x,x,T, x,T,x,T, x,x,x,x, x,x,x,T, x,x,x,x, x,x,x,x, x,x,x,x, x,T,T,x, T,T,T,T, x,x,T,T, T,T,x,T, x,x,x,x, x,x,x,x, x,T,x,x, T,T,T,x, T,T,x,T, x,x,x,T, T,T,T,T, x,x,x,T, T,T,T,T, T,T,x,x, x,x,x,x, x,x,x,x},
		{x,x,x,x, x,x,x,x, x,x,T,x, x,T,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,T,x,x, x,x,x,T, x,T,x,x, x,x,x,x, T,x,T,x, x,x,x,T, x,T,x,x, x,x,x,x, x,x,x,T, x,x,x,x, x,x,x,x, x,x,x,x, x,T,T,x, T,T,T,T, x,x,T,T, T,T,x,T, x,x,x,x, x,x,x,x, x,T,x,x, T,T,T,x, T,T,x,T, x,x,x,T, T,T,x,T, x,x,x,T, T,T,T,T, T,T,x,x, x,x,x,x, x,x,x,x},
		{x,x,x,x, x,x,x,x, x,x,T,x, x,T,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,T,x,x, x,x,x,T, x,T,x,x, x,x,x,x, T,x,T,x, x,x,x,T, x,T,x,x, x,x,x,x, x,x,x,T, x,x,x,x, x,x,x,x, x,x,x,x, x,T,T,x, T,T,T,T, x,x,T,T, T,T,x,T, x,x,x,x, x,x,x,x, x,T,x,x, T,T,T,x, T,T,x,T, x,x,x,T, T,T,T,T, x,x,x,T, T,T,T,T, T,T,x,x, x,x,x,x, x,x,x,x}

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
			case 107: s = "AMS_ASSIGN expected"; break;
			case 108: s = "LEQ expected"; break;
			case 109: s = "GEQ expected"; break;
			case 110: s = "ARROW expected"; break;
			case 111: s = "NEQ expected"; break;
			case 112: s = "VAR_ASSIGN expected"; break;
			case 113: s = "BOX expected"; break;
			case 114: s = "DBLQUOTE expected"; break;
			case 115: s = "SEMICOLON expected"; break;
			case 116: s = "COMMA expected"; break;
			case 117: s = "AMPERSAND expected"; break;
			case 118: s = "LPAREN expected"; break;
			case 119: s = "RPAREN expected"; break;
			case 120: s = "LBRACKET expected"; break;
			case 121: s = "RBRACKET expected"; break;
			case 122: s = "COLON expected"; break;
			case 123: s = "MUL expected"; break;
			case 124: s = "DIV expected"; break;
			case 125: s = "PLUS expected"; break;
			case 126: s = "MINUS expected"; break;
			case 127: s = "LT expected"; break;
			case 128: s = "GT expected"; break;
			case 129: s = "EQ expected"; break;
			case 130: s = "BAR expected"; break;
			case 131: s = "DOT expected"; break;
			case 132: s = "\"Coco/R\" expected"; break;
			case 133: s = "\"Development\" expected"; break;
			case 134: s = "\"Tools\" expected"; break;
			case 135: s = "\"jldfkj\u00f6l\" expected"; break;
			case 136: s = "\"\u00f6ksdf\" expected"; break;
			case 137: s = "\"\u00f6ksdsdf\" expected"; break;
			case 138: s = "??? expected"; break;
			case 139: s = "invalid library_unit"; break;
			case 140: s = "invalid identifier"; break;
			case 141: s = "invalid entity_declarative_item"; break;
			case 142: s = "invalid type_declaration"; break;
			case 143: s = "invalid disconnection_specification"; break;
			case 144: s = "invalid block_declarative_item"; break;
			case 145: s = "invalid configuration_declarative_item"; break;
			case 146: s = "invalid package_declarative_item"; break;
			case 147: s = "invalid package_body_declarative_item"; break;
			case 148: s = "invalid designator"; break;
			case 149: s = "invalid subprogram_specification"; break;
			case 150: s = "invalid subprogram_declarative_item"; break;
			case 151: s = "invalid type_definition"; break;
			case 152: s = "invalid alias_designator"; break;
			case 153: s = "invalid entity_name_list"; break;
			case 154: s = "invalid entity_class"; break;
			case 155: s = "invalid entity_designator"; break;
			case 156: s = "invalid instantiation_list"; break;
			case 157: s = "invalid entity_aspect"; break;
			case 158: s = "invalid group_constituent"; break;
			case 159: s = "invalid enumeration_literal"; break;
			case 160: s = "invalid protected_type_declarative_item"; break;
			case 161: s = "invalid protected_type_body_declarative_item"; break;
			case 162: s = "invalid constraint"; break;
			case 163: s = "invalid direction"; break;
			case 164: s = "invalid architecture_statement"; break;
			case 165: s = "invalid architecture_statement_optional_label"; break;
			case 166: s = "invalid architecture_statement_with_label"; break;
			case 167: s = "invalid generate_statement"; break;
			case 168: s = "invalid process_declarative_item"; break;
			case 169: s = "invalid concurrent_signal_assignment_statement"; break;
			case 170: s = "invalid target"; break;
			case 171: s = "invalid delay_mechanism"; break;
			case 172: s = "invalid waveform"; break;
			case 173: s = "invalid sequential_statement"; break;
			case 174: s = "invalid iteration_scheme"; break;
			case 175: s = "invalid interface_element_procedure"; break;
			case 176: s = "invalid interface_element_function"; break;
			case 177: s = "invalid interface_mode"; break;
			case 178: s = "invalid actual_part"; break;
			case 179: s = "invalid logical_operator"; break;
			case 180: s = "invalid relational_operator"; break;
			case 181: s = "invalid shift_operator"; break;
			case 182: s = "invalid sign"; break;
			case 183: s = "invalid adding_operator"; break;
			case 184: s = "invalid multiplying_operator"; break;
			case 185: s = "invalid factor"; break;
			case 186: s = "invalid primary"; break;
			case 187: s = "invalid allocator"; break;
			case 188: s = "invalid name_prefix"; break;
			case 189: s = "invalid name_selected_part"; break;
			case 190: s = "invalid name_part"; break;
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

