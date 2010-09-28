package at.jku.ssw.openvc;

import java.io.InputStream;
import java.io.IOException;
import java.io.RandomAccessFile;
import java.util.Map;
import java.util.HashMap;

class Token {
	int kind;    // token kind
	int pos;     // token position in the source text (starting at 0)
	int col;     // token column (starting at 1)
	int line;    // token line (starting at 1)
	String val;  // token value
	Token next;  // ML 2005-03-11 Peek tokens are kept in linked list
}

//-----------------------------------------------------------------------------------
// Buffer
//-----------------------------------------------------------------------------------
class Buffer {
	// This Buffer supports the following cases:
	// 1) seekable stream (file)
	//    a) whole stream in buffer
	//    b) part of stream in buffer
	// 2) non seekable stream (network, console)

	public static final int EOF = Character.MAX_VALUE + 1;
	private static final int MIN_BUFFER_LENGTH = 1024; // 1KB
	private static final int MAX_BUFFER_LENGTH = MIN_BUFFER_LENGTH * 64; // 64KB
	private byte[] buf;   // input buffer
	private int bufStart; // position of first byte in buffer relative to input stream
	private int bufLen;   // length of buffer
	private int fileLen;  // length of input stream (may change if stream is no file)
	private int bufPos;      // current position in buffer
	private RandomAccessFile file; // input stream (seekable)
	private InputStream stream; // growing input stream (e.g.: console, network)

	public Buffer(InputStream s) {
		stream = s;
		fileLen = bufLen = bufStart = bufPos = 0;
		buf = new byte[MIN_BUFFER_LENGTH];
	}

	public Buffer(String fileName) {
		try {
			file = new RandomAccessFile(fileName, "r");
			fileLen = (int) file.length();
			bufLen = Math.min(fileLen, MAX_BUFFER_LENGTH);
			buf = new byte[bufLen];
			bufStart = Integer.MAX_VALUE; // nothing in buffer so far
			if (fileLen > 0) setPos(0); // setup buffer to position 0 (start)
			else bufPos = 0; // index 0 is already after the file, thus setPos(0) is invalid
			if (bufLen == fileLen) Close();
		} catch (IOException e) {
			throw new FatalError("Could not open file " + fileName);
		}
	}

	// don't use b after this call anymore
	// called in UTF8Buffer constructor
	protected Buffer(Buffer b) {
		buf = b.buf;
		bufStart = b.bufStart;
		bufLen = b.bufLen;
		fileLen = b.fileLen;
		bufPos = b.bufPos;
		file = b.file;
		stream = b.stream;
		// keep finalize from closing the file
		b.file = null;
	}

	protected void finalize() throws Throwable {
		super.finalize();
		Close();
	}

	protected void Close() {
		if (file != null) {
			try {
				file.close();
				file = null;
			} catch (IOException e) {
				throw new FatalError(e.getMessage());
			}
		}
	}

	public int Read() {
		if (bufPos < bufLen) {
			return buf[bufPos++] & 0xff;  // mask out sign bits
		} else if (getPos() < fileLen) {
			setPos(getPos());         // shift buffer start to pos
			return buf[bufPos++] & 0xff; // mask out sign bits
		} else if (stream != null && ReadNextStreamChunk() > 0) {
			return buf[bufPos++] & 0xff;  // mask out sign bits
		} else {
			return EOF;
		}
	}

	public int Peek() {
		int curPos = getPos();
		int ch = Read();
		setPos(curPos);
		return ch;
	}

	public String GetString(int beg, int end) {
		int len = 0;
		char[] buf = new char[end - beg];
		int oldPos = getPos();
		setPos(beg);
		while (getPos() < end) buf[len++] = (char) Read();
		setPos(oldPos);
		return new String(buf, 0, len);
	}

	public int getPos() {
		return bufPos + bufStart;
	}

	public void setPos(int value) {
		if (value >= fileLen && stream != null) {
			// Wanted position is after buffer and the stream
			// is not seek-able e.g. network or console,
			// thus we have to read the stream manually till
			// the wanted position is in sight.
			while (value >= fileLen && ReadNextStreamChunk() > 0);
		}

		if (value < 0 || value > fileLen) {
			throw new FatalError("buffer out of bounds access, position: " + value);
		}

		if (value >= bufStart && value < bufStart + bufLen) { // already in buffer
			bufPos = value - bufStart;
		} else if (file != null) { // must be swapped in
			try {
				file.seek(value);
				bufLen = file.read(buf);
				bufStart = value; bufPos = 0;
			} catch(IOException e) {
				throw new FatalError(e.getMessage());
			}
		} else {
			// set the position to the end of the file, Pos will return fileLen.
			bufPos = fileLen - bufStart;
		}
	}
	
	// Read the next chunk of bytes from the stream, increases the buffer
	// if needed and updates the fields fileLen and bufLen.
	// Returns the number of bytes read.
	private int ReadNextStreamChunk() {
		int free = buf.length - bufLen;
		if (free == 0) {
			// in the case of a growing input stream
			// we can neither seek in the stream, nor can we
			// foresee the maximum length, thus we must adapt
			// the buffer size on demand.
			byte[] newBuf = new byte[bufLen * 2];
			System.arraycopy(buf, 0, newBuf, 0, bufLen);
			buf = newBuf;
			free = bufLen;
		}
		
		int read;
		try { read = stream.read(buf, bufLen, free); }
		catch (IOException ioex) { throw new FatalError(ioex.getMessage()); }
		
		if (read > 0) {
			fileLen = bufLen = (bufLen + read);
			return read;
		}
		// end of stream reached
		return 0;
	}
}

//-----------------------------------------------------------------------------------
// UTF8Buffer
//-----------------------------------------------------------------------------------
class UTF8Buffer extends Buffer {
	UTF8Buffer(Buffer b) { super(b); }

	public int Read() {
		int ch;
		do {
			ch = super.Read();
			// until we find a utf8 start (0xxxxxxx or 11xxxxxx)
		} while ((ch >= 128) && ((ch & 0xC0) != 0xC0) && (ch != EOF));
		if (ch < 128 || ch == EOF) {
			// nothing to do, first 127 chars are the same in ascii and utf8
			// 0xxxxxxx or end of file character
		} else if ((ch & 0xF0) == 0xF0) {
			// 11110xxx 10xxxxxx 10xxxxxx 10xxxxxx
			int c1 = ch & 0x07; ch = super.Read();
			int c2 = ch & 0x3F; ch = super.Read();
			int c3 = ch & 0x3F; ch = super.Read();
			int c4 = ch & 0x3F;
			ch = (((((c1 << 6) | c2) << 6) | c3) << 6) | c4;
		} else if ((ch & 0xE0) == 0xE0) {
			// 1110xxxx 10xxxxxx 10xxxxxx
			int c1 = ch & 0x0F; ch = super.Read();
			int c2 = ch & 0x3F; ch = super.Read();
			int c3 = ch & 0x3F;
			ch = (((c1 << 6) | c2) << 6) | c3;
		} else if ((ch & 0xC0) == 0xC0) {
			// 110xxxxx 10xxxxxx
			int c1 = ch & 0x1F; ch = super.Read();
			int c2 = ch & 0x3F;
			ch = (c1 << 6) | c2;
		}
		return ch;
	}
}

//-----------------------------------------------------------------------------------
// StartStates  -- maps characters to start states of tokens
//-----------------------------------------------------------------------------------
class StartStates {
	private static class Elem {
		public int key, val;
		public Elem next;
		public Elem(int key, int val) { this.key = key; this.val = val; }
	}

	private Elem[] tab = new Elem[128];

	public void set(int key, int val) {
		Elem e = new Elem(key, val);
		int k = key % 128;
		e.next = tab[k]; tab[k] = e;
	}

	public int state(int key) {
		Elem e = tab[key % 128];
		while (e != null && e.key != key) e = e.next;
		return e == null ? 0: e.val;
	}
}

//-----------------------------------------------------------------------------------
// Scanner
//-----------------------------------------------------------------------------------
public class Scanner {
	static final char EOL = '\n';
	static final int  eofSym = 0;
	static final int maxT = 132;
	static final int noSym = 132;
	char valCh;       // current input character (for token.val)

	public Buffer buffer; // scanner buffer

	Token t;           // current token
	int ch;            // current input character
	int pos;           // byte position of current character
	int col;           // column number of current character
	int line;          // line number of current character
	int oldEols;       // EOLs that appeared in a comment;
	static final StartStates start; // maps initial token character to start state
	static final Map<String,Integer> literals;      // maps literal strings to literal kinds

	Token tokens;      // list of tokens already peeked (first token is a dummy)
	Token pt;          // current peek token
	
	char[] tval = new char[16]; // token text used in NextToken(), dynamically enlarged
	int tlen;          // length of current token


	static {
		start = new StartStates();
		literals = new HashMap<String,Integer>();
		for (int i = 97; i <= 97; ++i) start.set(i, 1);
		for (int i = 99; i <= 110; ++i) start.set(i, 1);
		for (int i = 112; i <= 119; ++i) start.set(i, 1);
		for (int i = 121; i <= 122; ++i) start.set(i, 1);
		for (int i = 192; i <= 214; ++i) start.set(i, 1);
		for (int i = 216; i <= 246; ++i) start.set(i, 1);
		for (int i = 248; i <= 255; ++i) start.set(i, 1);
		for (int i = 48; i <= 57; ++i) start.set(i, 51);
		for (int i = 98; i <= 98; ++i) start.set(i, 52);
		for (int i = 111; i <= 111; ++i) start.set(i, 52);
		for (int i = 120; i <= 120; ++i) start.set(i, 52);
		start.set(92, 2); 
		start.set(34, 25); 
		start.set(39, 53); 
		start.set(42, 54); 
		start.set(60, 55); 
		start.set(62, 56); 
		start.set(61, 57); 
		start.set(47, 58); 
		start.set(58, 59); 
		start.set(59, 40); 
		start.set(44, 41); 
		start.set(38, 42); 
		start.set(40, 43); 
		start.set(41, 44); 
		start.set(91, 45); 
		start.set(93, 46); 
		start.set(43, 47); 
		start.set(45, 48); 
		start.set(124, 49); 
		start.set(46, 50); 
		start.set(Buffer.EOF, -1);
		literals.put("abs", new Integer(10));
		literals.put("access", new Integer(11));
		literals.put("after", new Integer(12));
		literals.put("alias", new Integer(13));
		literals.put("all", new Integer(14));
		literals.put("and", new Integer(15));
		literals.put("architecture", new Integer(16));
		literals.put("array", new Integer(17));
		literals.put("assert", new Integer(18));
		literals.put("attribute", new Integer(19));
		literals.put("begin", new Integer(20));
		literals.put("block", new Integer(21));
		literals.put("body", new Integer(22));
		literals.put("buffer", new Integer(23));
		literals.put("bus", new Integer(24));
		literals.put("case", new Integer(25));
		literals.put("component", new Integer(26));
		literals.put("configuration", new Integer(27));
		literals.put("constant", new Integer(28));
		literals.put("disconnect", new Integer(29));
		literals.put("downto", new Integer(30));
		literals.put("else", new Integer(31));
		literals.put("elsif", new Integer(32));
		literals.put("end", new Integer(33));
		literals.put("entity", new Integer(34));
		literals.put("exit", new Integer(35));
		literals.put("file", new Integer(36));
		literals.put("for", new Integer(37));
		literals.put("function", new Integer(38));
		literals.put("generate", new Integer(39));
		literals.put("generic", new Integer(40));
		literals.put("group", new Integer(41));
		literals.put("guarded", new Integer(42));
		literals.put("if", new Integer(43));
		literals.put("impure", new Integer(44));
		literals.put("in", new Integer(45));
		literals.put("inertial", new Integer(46));
		literals.put("inout", new Integer(47));
		literals.put("is", new Integer(48));
		literals.put("label", new Integer(49));
		literals.put("library", new Integer(50));
		literals.put("linkage", new Integer(51));
		literals.put("literal", new Integer(52));
		literals.put("loop", new Integer(53));
		literals.put("map", new Integer(54));
		literals.put("mod", new Integer(55));
		literals.put("nand", new Integer(56));
		literals.put("new", new Integer(57));
		literals.put("next", new Integer(58));
		literals.put("nor", new Integer(59));
		literals.put("not", new Integer(60));
		literals.put("null", new Integer(61));
		literals.put("of", new Integer(62));
		literals.put("on", new Integer(63));
		literals.put("open", new Integer(64));
		literals.put("or", new Integer(65));
		literals.put("others", new Integer(66));
		literals.put("out", new Integer(67));
		literals.put("package", new Integer(68));
		literals.put("port", new Integer(69));
		literals.put("postponed", new Integer(70));
		literals.put("procedure", new Integer(71));
		literals.put("process", new Integer(72));
		literals.put("protected", new Integer(73));
		literals.put("pure", new Integer(74));
		literals.put("range", new Integer(75));
		literals.put("record", new Integer(76));
		literals.put("register", new Integer(77));
		literals.put("reject", new Integer(78));
		literals.put("rem", new Integer(79));
		literals.put("report", new Integer(80));
		literals.put("return", new Integer(81));
		literals.put("rol", new Integer(82));
		literals.put("ror", new Integer(83));
		literals.put("select", new Integer(84));
		literals.put("severity", new Integer(85));
		literals.put("shared", new Integer(86));
		literals.put("signal", new Integer(87));
		literals.put("sla", new Integer(88));
		literals.put("sll", new Integer(89));
		literals.put("sra", new Integer(90));
		literals.put("srl", new Integer(91));
		literals.put("subtype", new Integer(92));
		literals.put("then", new Integer(93));
		literals.put("to", new Integer(94));
		literals.put("transport", new Integer(95));
		literals.put("type", new Integer(96));
		literals.put("unaffected", new Integer(97));
		literals.put("units", new Integer(98));
		literals.put("until", new Integer(99));
		literals.put("use", new Integer(100));
		literals.put("variable", new Integer(101));
		literals.put("wait", new Integer(102));
		literals.put("when", new Integer(103));
		literals.put("while", new Integer(104));
		literals.put("with", new Integer(105));
		literals.put("xnor", new Integer(106));
		literals.put("xor", new Integer(107));

	}
	
	public Scanner (String fileName) {
		buffer = new Buffer(fileName);
		Init();
	}
	
	public Scanner(InputStream s) {
		buffer = new Buffer(s);
		Init();
	}
	
	void Init () {
		pos = -1; line = 1; col = 0;
		oldEols = 0;
		NextCh();
		if (ch == 0xEF) { // check optional byte order mark for UTF-8
			NextCh(); int ch1 = ch;
			NextCh(); int ch2 = ch;
			if (ch1 != 0xBB || ch2 != 0xBF) {
				throw new FatalError("Illegal byte order mark at start of file");
			}
			buffer = new UTF8Buffer(buffer); col = 0;
			NextCh();
		}
		pt = tokens = new Token();  // first token is a dummy
	}
	
	void NextCh() {
		if (oldEols > 0) { ch = EOL; oldEols--; }
		else {
			pos = buffer.getPos();
			ch = buffer.Read(); col++;
			// replace isolated '\r' by '\n' in order to make
			// eol handling uniform across Windows, Unix and Mac
			if (ch == '\r' && buffer.Peek() != '\n') ch = EOL;
			if (ch == EOL) { line++; col = 0; }
		}
		if (ch != Buffer.EOF) {
			valCh = (char) ch;
			ch = Character.toLowerCase(ch);
		}

	}
	
	void AddCh() {
		if (tlen >= tval.length) {
			char[] newBuf = new char[2 * tval.length];
			System.arraycopy(tval, 0, newBuf, 0, tval.length);
			tval = newBuf;
		}
		if (ch != Buffer.EOF) {
			tval[tlen++] = valCh; 

			NextCh();
		}

	}
	

	boolean Comment0() {
		int level = 1, pos0 = pos, line0 = line, col0 = col;
		NextCh();
		if (ch == '-') {
			NextCh();
			for(;;) {
				if (ch == 10) {
					level--;
					if (level == 0) { oldEols = line - line0; NextCh(); return true; }
					NextCh();
				} else if (ch == Buffer.EOF) return false;
				else NextCh();
			}
		} else {
			buffer.setPos(pos0); NextCh(); line = line0; col = col0;
		}
		return false;
	}

	boolean Comment1() {
		int level = 1, pos0 = pos, line0 = line, col0 = col;
		NextCh();
		if (ch == '*') {
			NextCh();
			for(;;) {
				if (ch == '*') {
					NextCh();
					if (ch == '/') {
						level--;
						if (level == 0) { oldEols = line - line0; NextCh(); return true; }
						NextCh();
					}
				} else if (ch == '/') {
					NextCh();
					if (ch == '*') {
						level++; NextCh();
					}
				} else if (ch == Buffer.EOF) return false;
				else NextCh();
			}
		} else {
			buffer.setPos(pos0); NextCh(); line = line0; col = col0;
		}
		return false;
	}


	void CheckLiteral() {
		String val = t.val;
		val = val.toLowerCase();

		Object kind = literals.get(val);
		if (kind != null) {
			t.kind = ((Integer) kind).intValue();
		}
	}

	Token NextToken() {
		while (ch == ' ' ||
			ch >= 9 && ch <= 10 || ch == 13 || ch == ' '
		) NextCh();
		if (ch == '-' && Comment0() ||ch == '/' && Comment1()) return NextToken();
		int apx = 0;
		t = new Token();
		t.pos = pos; t.col = col; t.line = line; 
		int state = start.state(ch);
		tlen = 0; AddCh();

		loop: for (;;) {
			switch (state) {
				case -1: { t.kind = eofSym; break loop; } // NextCh already done 
				case 0: { t.kind = noSym; break loop; }   // NextCh already done
				case 1:
					if (ch >= '0' && ch <= '9' || ch == '_' || ch >= 'a' && ch <= 'z' || ch >= 192 && ch <= 214 || ch >= 216 && ch <= 246 || ch >= 248 && ch <= 255) {AddCh(); state = 1; break;}
					else {t.kind = 1; t.val = new String(tval, 0, tlen); CheckLiteral(); return t;}
				case 2:
					if (ch >= ' ' && ch <= '@' || ch == '[' || ch >= ']' && ch <= '~' || ch >= 160 && ch <= 255) {AddCh(); state = 2; break;}
					else if (ch == 92) {AddCh(); state = 60; break;}
					else {t.kind = noSym; break loop;}
				case 3:
					if (ch >= '0' && ch <= '9' || ch >= 'a' && ch <= 'z' || ch >= 192 && ch <= 214 || ch >= 216 && ch <= 246 || ch >= 248 && ch <= 255) {AddCh(); state = 4; break;}
					else {t.kind = noSym; break loop;}
				case 4:
					if (ch >= '0' && ch <= '9' || ch >= 'a' && ch <= 'z' || ch >= 192 && ch <= 214 || ch >= 216 && ch <= 246 || ch >= 248 && ch <= 255) {AddCh(); state = 4; break;}
					else if (ch == '#') {AddCh(); state = 5; break;}
					else if (ch == '.') {AddCh(); state = 10; break;}
					else if (ch == '_') {AddCh(); state = 13; break;}
					else {t.kind = noSym; break loop;}
				case 5:
					if (ch == 'e') {AddCh(); state = 6; break;}
					else {t.kind = 3; break loop;}
				case 6:
					if (ch >= '0' && ch <= '9') {AddCh(); state = 8; break;}
					else if (ch == '+' || ch == '-') {AddCh(); state = 7; break;}
					else {t.kind = noSym; break loop;}
				case 7:
					if (ch >= '0' && ch <= '9') {AddCh(); state = 8; break;}
					else {t.kind = noSym; break loop;}
				case 8:
					if (ch >= '0' && ch <= '9') {AddCh(); state = 8; break;}
					else if (ch == '_') {AddCh(); state = 9; break;}
					else {t.kind = 3; break loop;}
				case 9:
					if (ch >= '0' && ch <= '9') {AddCh(); state = 8; break;}
					else {t.kind = noSym; break loop;}
				case 10:
					if (ch >= '0' && ch <= '9' || ch >= 'a' && ch <= 'z' || ch >= 192 && ch <= 214 || ch >= 216 && ch <= 246 || ch >= 248 && ch <= 255) {AddCh(); state = 11; break;}
					else {t.kind = noSym; break loop;}
				case 11:
					if (ch >= '0' && ch <= '9' || ch >= 'a' && ch <= 'z' || ch >= 192 && ch <= 214 || ch >= 216 && ch <= 246 || ch >= 248 && ch <= 255) {AddCh(); state = 11; break;}
					else if (ch == '#') {AddCh(); state = 5; break;}
					else if (ch == '_') {AddCh(); state = 12; break;}
					else {t.kind = noSym; break loop;}
				case 12:
					if (ch >= '0' && ch <= '9' || ch >= 'a' && ch <= 'z' || ch >= 192 && ch <= 214 || ch >= 216 && ch <= 246 || ch >= 248 && ch <= 255) {AddCh(); state = 11; break;}
					else {t.kind = noSym; break loop;}
				case 13:
					if (ch >= '0' && ch <= '9' || ch >= 'a' && ch <= 'z' || ch >= 192 && ch <= 214 || ch >= 216 && ch <= 246 || ch >= 248 && ch <= 255) {AddCh(); state = 4; break;}
					else {t.kind = noSym; break loop;}
				case 14:
					if (ch >= '0' && ch <= '9') {AddCh(); state = 16; break;}
					else if (ch == '+' || ch == '-') {AddCh(); state = 15; break;}
					else {t.kind = noSym; break loop;}
				case 15:
					if (ch >= '0' && ch <= '9') {AddCh(); state = 16; break;}
					else {t.kind = noSym; break loop;}
				case 16:
					if (ch >= '0' && ch <= '9') {AddCh(); state = 16; break;}
					else if (ch == '_') {AddCh(); state = 17; break;}
					else {t.kind = 4; break loop;}
				case 17:
					if (ch >= '0' && ch <= '9') {AddCh(); state = 16; break;}
					else {t.kind = noSym; break loop;}
				case 18:
					if (ch >= '0' && ch <= '9') {AddCh(); state = 19; break;}
					else {t.kind = noSym; break loop;}
				case 19:
					if (ch >= '0' && ch <= '9') {AddCh(); state = 19; break;}
					else if (ch == 'e') {AddCh(); state = 20; break;}
					else if (ch == '_') {AddCh(); state = 24; break;}
					else {t.kind = 5; break loop;}
				case 20:
					if (ch >= '0' && ch <= '9') {AddCh(); state = 22; break;}
					else if (ch == '+' || ch == '-') {AddCh(); state = 21; break;}
					else {t.kind = noSym; break loop;}
				case 21:
					if (ch >= '0' && ch <= '9') {AddCh(); state = 22; break;}
					else {t.kind = noSym; break loop;}
				case 22:
					if (ch >= '0' && ch <= '9') {AddCh(); state = 22; break;}
					else if (ch == '_') {AddCh(); state = 23; break;}
					else {t.kind = 5; break loop;}
				case 23:
					if (ch >= '0' && ch <= '9') {AddCh(); state = 22; break;}
					else {t.kind = noSym; break loop;}
				case 24:
					if (ch >= '0' && ch <= '9') {AddCh(); state = 19; break;}
					else {t.kind = noSym; break loop;}
				case 25:
					if (ch >= ' ' && ch <= '!' || ch >= '#' && ch <= '@' || ch >= '[' && ch <= '~' || ch >= 160 && ch <= 255) {AddCh(); state = 25; break;}
					else if (ch == '"') {AddCh(); state = 61; break;}
					else {t.kind = noSym; break loop;}
				case 26:
					if (ch >= '0' && ch <= '9' || ch >= 'a' && ch <= 'z' || ch >= 192 && ch <= 214 || ch >= 216 && ch <= 246 || ch >= 248 && ch <= 255) {AddCh(); state = 27; break;}
					else if (ch == '"') {AddCh(); state = 29; break;}
					else {t.kind = noSym; break loop;}
				case 27:
					if (ch >= '0' && ch <= '9' || ch >= 'a' && ch <= 'z' || ch >= 192 && ch <= 214 || ch >= 216 && ch <= 246 || ch >= 248 && ch <= 255) {AddCh(); state = 27; break;}
					else if (ch == '"') {AddCh(); state = 29; break;}
					else if (ch == '_') {AddCh(); state = 28; break;}
					else {t.kind = noSym; break loop;}
				case 28:
					if (ch >= '0' && ch <= '9' || ch >= 'a' && ch <= 'z' || ch >= 192 && ch <= 214 || ch >= 216 && ch <= 246 || ch >= 248 && ch <= 255) {AddCh(); state = 27; break;}
					else {t.kind = noSym; break loop;}
				case 29:
					{t.kind = 7; break loop;}
				case 30:
					{
					tlen -= apx;
					buffer.setPos(t.pos); NextCh(); line = t.line; col = t.col; 
					for (int i = 0; i < tlen; i++) NextCh();
					t.kind = 8; break loop;}
				case 31:
					if (ch == 39) {AddCh(); state = 32; break;}
					else {t.kind = noSym; break loop;}
				case 32:
					{t.kind = 9; break loop;}
				case 33:
					{t.kind = 108; break loop;}
				case 34:
					{t.kind = 109; break loop;}
				case 35:
					{t.kind = 110; break loop;}
				case 36:
					{t.kind = 111; break loop;}
				case 37:
					{t.kind = 112; break loop;}
				case 38:
					{t.kind = 113; break loop;}
				case 39:
					{t.kind = 114; break loop;}
				case 40:
					{t.kind = 115; break loop;}
				case 41:
					{t.kind = 116; break loop;}
				case 42:
					{t.kind = 117; break loop;}
				case 43:
					{t.kind = 118; break loop;}
				case 44:
					{t.kind = 119; break loop;}
				case 45:
					{t.kind = 120; break loop;}
				case 46:
					{t.kind = 121; break loop;}
				case 47:
					{t.kind = 125; break loop;}
				case 48:
					{t.kind = 126; break loop;}
				case 49:
					{t.kind = 130; break loop;}
				case 50:
					{t.kind = 131; break loop;}
				case 51:
					if (ch >= '0' && ch <= '9') {AddCh(); state = 51; break;}
					else if (ch == '#') {AddCh(); state = 3; break;}
					else if (ch == '_') {AddCh(); state = 62; break;}
					else if (ch == 'e') {AddCh(); state = 14; break;}
					else if (ch == '.') {AddCh(); state = 18; break;}
					else {t.kind = 4; break loop;}
				case 52:
					if (ch >= '0' && ch <= '9' || ch == '_' || ch >= 'a' && ch <= 'z' || ch >= 192 && ch <= 214 || ch >= 216 && ch <= 246 || ch >= 248 && ch <= 255) {AddCh(); state = 1; break;}
					else if (ch == '"') {AddCh(); state = 26; break;}
					else {t.kind = 1; t.val = new String(tval, 0, tlen); CheckLiteral(); return t;}
				case 53:
					if (ch >= 'a' && ch <= 'z' || ch >= 192 && ch <= 214 || ch >= 216 && ch <= 246 || ch >= 248 && ch <= 255) {apx++; AddCh(); state = 63; break;}
					else if (ch >= ' ' && ch <= '@' || ch >= '[' && ch <= '`' || ch >= '{' && ch <= '~' || ch >= 160 && ch <= 191 || ch == 215 || ch == 247) {AddCh(); state = 31; break;}
					else {t.kind = noSym; break loop;}
				case 54:
					if (ch == '*') {AddCh(); state = 33; break;}
					else {t.kind = 123; break loop;}
				case 55:
					if (ch == '=') {AddCh(); state = 34; break;}
					else if (ch == '>') {AddCh(); state = 39; break;}
					else {t.kind = 127; break loop;}
				case 56:
					if (ch == '=') {AddCh(); state = 35; break;}
					else {t.kind = 128; break loop;}
				case 57:
					if (ch == '>') {AddCh(); state = 36; break;}
					else {t.kind = 129; break loop;}
				case 58:
					if (ch == '=') {AddCh(); state = 37; break;}
					else {t.kind = 124; break loop;}
				case 59:
					if (ch == '=') {AddCh(); state = 38; break;}
					else {t.kind = 122; break loop;}
				case 60:
					if (ch == 92) {AddCh(); state = 2; break;}
					else {t.kind = 2; break loop;}
				case 61:
					if (ch == '"') {AddCh(); state = 25; break;}
					else {t.kind = 6; break loop;}
				case 62:
					if (ch >= '0' && ch <= '9') {AddCh(); state = 51; break;}
					else {t.kind = noSym; break loop;}
				case 63:
					if (ch <= '&' || ch >= '(' && ch <= 65535) {apx++; AddCh(); state = 30; break;}
					else if (ch == 39) {apx = 0; AddCh(); state = 32; break;}
					else {t.kind = noSym; break loop;}

			}
		}
		t.val = new String(tval, 0, tlen);
		return t;
	}
	
	// get the next token (possibly a token already seen during peeking)
	public Token Scan () {
		if (tokens.next == null) {
			return NextToken();
		} else {
			pt = tokens = tokens.next;
			return tokens;
		}
	}

	// get the next token, ignore pragmas
	public Token Peek () {
		do {
			if (pt.next == null) {
				pt.next = NextToken();
			}
			pt = pt.next;
		} while (pt.kind > maxT); // skip pragmas

		return pt;
	}

	// make sure that peeking starts at current scan position
	public void ResetPeek () { pt = tokens; }

} // end Scanner

