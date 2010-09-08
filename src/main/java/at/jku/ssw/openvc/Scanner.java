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
	static final int maxT = 134;
	static final int noSym = 134;


	public Buffer buffer; // scanner buffer

	Token t;           // current token
	int ch;            // current input character
	int pos;           // byte position of current character
	int col;           // column number of current character
	int line;          // line number of current character
	int oldEols;       // EOLs that appeared in a comment;
	static final StartStates start; // maps initial token character to start state
	static final Map literals;      // maps literal strings to literal kinds

	Token tokens;      // list of tokens already peeked (first token is a dummy)
	Token pt;          // current peek token
	
	char[] tval = new char[16]; // token text used in NextToken(), dynamically enlarged
	int tlen;          // length of current token


	static {
		start = new StartStates();
		literals = new HashMap();
		for (int i = 48; i <= 57; ++i) start.set(i, 1);
		for (int i = 65; i <= 90; ++i) start.set(i, 3);
		for (int i = 104; i <= 104; ++i) start.set(i, 3);
		for (int i = 107; i <= 107; ++i) start.set(i, 3);
		for (int i = 113; i <= 113; ++i) start.set(i, 3);
		for (int i = 121; i <= 122; ++i) start.set(i, 3);
		start.set(98, 369); 
		start.set(97, 370); 
		start.set(99, 371); 
		start.set(100, 372); 
		start.set(101, 373); 
		start.set(102, 374); 
		start.set(103, 375); 
		start.set(105, 376); 
		start.set(108, 377); 
		start.set(109, 378); 
		start.set(110, 379); 
		start.set(111, 380); 
		start.set(112, 381); 
		start.set(114, 382); 
		start.set(115, 383); 
		start.set(116, 384); 
		start.set(117, 385); 
		start.set(118, 386); 
		start.set(119, 387); 
		start.set(120, 388); 
		start.set(42, 389); 
		start.set(60, 390); 
		start.set(62, 391); 
		start.set(61, 392); 
		start.set(47, 393); 
		start.set(58, 394); 
		start.set(34, 357); 
		start.set(59, 358); 
		start.set(44, 359); 
		start.set(38, 360); 
		start.set(40, 361); 
		start.set(41, 362); 
		start.set(91, 363); 
		start.set(93, 364); 
		start.set(43, 365); 
		start.set(45, 366); 
		start.set(124, 367); 
		start.set(46, 368); 
		start.set(106, 439); 
		start.set(246, 440); 
		start.set(Buffer.EOF, -1);

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

	}
	
	void AddCh() {
		if (tlen >= tval.length) {
			char[] newBuf = new char[2 * tval.length];
			System.arraycopy(tval, 0, newBuf, 0, tval.length);
			tval = newBuf;
		}
		if (ch != Buffer.EOF) {
			tval[tlen++] = (char)ch; 

			NextCh();
		}

	}
	

	boolean Comment0() {
		int level = 1, pos0 = pos, line0 = line, col0 = col;
		NextCh();
		if (ch == '/') {
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

		Object kind = literals.get(val);
		if (kind != null) {
			t.kind = ((Integer) kind).intValue();
		}
	}

	Token NextToken() {
		while (ch == ' ' ||
			ch >= 9 && ch <= 10 || ch == 13
		) NextCh();
		if (ch == '/' && Comment0() ||ch == '/' && Comment1()) return NextToken();
		t = new Token();
		t.pos = pos; t.col = col; t.line = line; 
		int state = start.state(ch);
		tlen = 0; AddCh();

		loop: for (;;) {
			switch (state) {
				case -1: { t.kind = eofSym; break loop; } // NextCh already done 
				case 0: { t.kind = noSym; break loop; }   // NextCh already done
				case 1:
					if (ch >= '0' && ch <= '9') {AddCh(); state = 1; break;}
					else if (ch == '_') {AddCh(); state = 2; break;}
					else {t.kind = 1; break loop;}
				case 2:
					if (ch >= '0' && ch <= '9') {AddCh(); state = 1; break;}
					else {t.kind = noSym; break loop;}
				case 3:
					{t.kind = 2; break loop;}
				case 4:
					{t.kind = 6; break loop;}
				case 5:
					{t.kind = 7; break loop;}
				case 6:
					if (ch == 's') {AddCh(); state = 7; break;}
					else {t.kind = noSym; break loop;}
				case 7:
					{t.kind = 8; break loop;}
				case 8:
					if (ch == 'c') {AddCh(); state = 9; break;}
					else {t.kind = noSym; break loop;}
				case 9:
					if (ch == 'e') {AddCh(); state = 10; break;}
					else {t.kind = noSym; break loop;}
				case 10:
					if (ch == 's') {AddCh(); state = 11; break;}
					else {t.kind = noSym; break loop;}
				case 11:
					if (ch == 's') {AddCh(); state = 12; break;}
					else {t.kind = noSym; break loop;}
				case 12:
					{t.kind = 9; break loop;}
				case 13:
					if (ch == 't') {AddCh(); state = 14; break;}
					else {t.kind = noSym; break loop;}
				case 14:
					if (ch == 'e') {AddCh(); state = 15; break;}
					else {t.kind = noSym; break loop;}
				case 15:
					if (ch == 'r') {AddCh(); state = 16; break;}
					else {t.kind = noSym; break loop;}
				case 16:
					{t.kind = 10; break loop;}
				case 17:
					if (ch == 'a') {AddCh(); state = 18; break;}
					else {t.kind = noSym; break loop;}
				case 18:
					if (ch == 's') {AddCh(); state = 19; break;}
					else {t.kind = noSym; break loop;}
				case 19:
					{t.kind = 11; break loop;}
				case 20:
					{t.kind = 12; break loop;}
				case 21:
					if (ch == 'd') {AddCh(); state = 22; break;}
					else {t.kind = noSym; break loop;}
				case 22:
					{t.kind = 13; break loop;}
				case 23:
					if (ch == 'h') {AddCh(); state = 24; break;}
					else {t.kind = noSym; break loop;}
				case 24:
					if (ch == 'i') {AddCh(); state = 25; break;}
					else {t.kind = noSym; break loop;}
				case 25:
					if (ch == 't') {AddCh(); state = 26; break;}
					else {t.kind = noSym; break loop;}
				case 26:
					if (ch == 'e') {AddCh(); state = 27; break;}
					else {t.kind = noSym; break loop;}
				case 27:
					if (ch == 'c') {AddCh(); state = 28; break;}
					else {t.kind = noSym; break loop;}
				case 28:
					if (ch == 't') {AddCh(); state = 29; break;}
					else {t.kind = noSym; break loop;}
				case 29:
					if (ch == 'u') {AddCh(); state = 30; break;}
					else {t.kind = noSym; break loop;}
				case 30:
					if (ch == 'r') {AddCh(); state = 31; break;}
					else {t.kind = noSym; break loop;}
				case 31:
					if (ch == 'e') {AddCh(); state = 32; break;}
					else {t.kind = noSym; break loop;}
				case 32:
					{t.kind = 14; break loop;}
				case 33:
					if (ch == 'a') {AddCh(); state = 34; break;}
					else {t.kind = noSym; break loop;}
				case 34:
					if (ch == 'y') {AddCh(); state = 35; break;}
					else {t.kind = noSym; break loop;}
				case 35:
					{t.kind = 15; break loop;}
				case 36:
					if (ch == 's') {AddCh(); state = 37; break;}
					else {t.kind = noSym; break loop;}
				case 37:
					if (ch == 'e') {AddCh(); state = 38; break;}
					else {t.kind = noSym; break loop;}
				case 38:
					if (ch == 'r') {AddCh(); state = 39; break;}
					else {t.kind = noSym; break loop;}
				case 39:
					if (ch == 't') {AddCh(); state = 40; break;}
					else {t.kind = noSym; break loop;}
				case 40:
					{t.kind = 16; break loop;}
				case 41:
					if (ch == 't') {AddCh(); state = 42; break;}
					else {t.kind = noSym; break loop;}
				case 42:
					if (ch == 'r') {AddCh(); state = 43; break;}
					else {t.kind = noSym; break loop;}
				case 43:
					if (ch == 'i') {AddCh(); state = 44; break;}
					else {t.kind = noSym; break loop;}
				case 44:
					if (ch == 'b') {AddCh(); state = 45; break;}
					else {t.kind = noSym; break loop;}
				case 45:
					if (ch == 'u') {AddCh(); state = 46; break;}
					else {t.kind = noSym; break loop;}
				case 46:
					if (ch == 't') {AddCh(); state = 47; break;}
					else {t.kind = noSym; break loop;}
				case 47:
					if (ch == 'e') {AddCh(); state = 48; break;}
					else {t.kind = noSym; break loop;}
				case 48:
					{t.kind = 17; break loop;}
				case 49:
					if (ch == 'g') {AddCh(); state = 50; break;}
					else {t.kind = noSym; break loop;}
				case 50:
					if (ch == 'i') {AddCh(); state = 51; break;}
					else {t.kind = noSym; break loop;}
				case 51:
					if (ch == 'n') {AddCh(); state = 52; break;}
					else {t.kind = noSym; break loop;}
				case 52:
					{t.kind = 18; break loop;}
				case 53:
					if (ch == 'c') {AddCh(); state = 54; break;}
					else {t.kind = noSym; break loop;}
				case 54:
					if (ch == 'k') {AddCh(); state = 55; break;}
					else {t.kind = noSym; break loop;}
				case 55:
					{t.kind = 19; break loop;}
				case 56:
					if (ch == 'd') {AddCh(); state = 57; break;}
					else {t.kind = noSym; break loop;}
				case 57:
					if (ch == 'y') {AddCh(); state = 58; break;}
					else {t.kind = noSym; break loop;}
				case 58:
					{t.kind = 20; break loop;}
				case 59:
					if (ch == 'f') {AddCh(); state = 60; break;}
					else {t.kind = noSym; break loop;}
				case 60:
					if (ch == 'e') {AddCh(); state = 61; break;}
					else {t.kind = noSym; break loop;}
				case 61:
					if (ch == 'r') {AddCh(); state = 62; break;}
					else {t.kind = noSym; break loop;}
				case 62:
					{t.kind = 21; break loop;}
				case 63:
					{t.kind = 22; break loop;}
				case 64:
					if (ch == 's') {AddCh(); state = 65; break;}
					else {t.kind = noSym; break loop;}
				case 65:
					if (ch == 'e') {AddCh(); state = 66; break;}
					else {t.kind = noSym; break loop;}
				case 66:
					{t.kind = 23; break loop;}
				case 67:
					if (ch == 'p') {AddCh(); state = 68; break;}
					else {t.kind = noSym; break loop;}
				case 68:
					if (ch == 'o') {AddCh(); state = 69; break;}
					else {t.kind = noSym; break loop;}
				case 69:
					if (ch == 'n') {AddCh(); state = 70; break;}
					else {t.kind = noSym; break loop;}
				case 70:
					if (ch == 'e') {AddCh(); state = 71; break;}
					else {t.kind = noSym; break loop;}
				case 71:
					if (ch == 'n') {AddCh(); state = 72; break;}
					else {t.kind = noSym; break loop;}
				case 72:
					if (ch == 't') {AddCh(); state = 73; break;}
					else {t.kind = noSym; break loop;}
				case 73:
					{t.kind = 24; break loop;}
				case 74:
					if (ch == 'i') {AddCh(); state = 75; break;}
					else {t.kind = noSym; break loop;}
				case 75:
					if (ch == 'g') {AddCh(); state = 76; break;}
					else {t.kind = noSym; break loop;}
				case 76:
					if (ch == 'u') {AddCh(); state = 77; break;}
					else {t.kind = noSym; break loop;}
				case 77:
					if (ch == 'r') {AddCh(); state = 78; break;}
					else {t.kind = noSym; break loop;}
				case 78:
					if (ch == 'a') {AddCh(); state = 79; break;}
					else {t.kind = noSym; break loop;}
				case 79:
					if (ch == 't') {AddCh(); state = 80; break;}
					else {t.kind = noSym; break loop;}
				case 80:
					if (ch == 'i') {AddCh(); state = 81; break;}
					else {t.kind = noSym; break loop;}
				case 81:
					if (ch == 'o') {AddCh(); state = 82; break;}
					else {t.kind = noSym; break loop;}
				case 82:
					if (ch == 'n') {AddCh(); state = 83; break;}
					else {t.kind = noSym; break loop;}
				case 83:
					{t.kind = 25; break loop;}
				case 84:
					if (ch == 't') {AddCh(); state = 85; break;}
					else {t.kind = noSym; break loop;}
				case 85:
					if (ch == 'a') {AddCh(); state = 86; break;}
					else {t.kind = noSym; break loop;}
				case 86:
					if (ch == 'n') {AddCh(); state = 87; break;}
					else {t.kind = noSym; break loop;}
				case 87:
					if (ch == 't') {AddCh(); state = 88; break;}
					else {t.kind = noSym; break loop;}
				case 88:
					{t.kind = 26; break loop;}
				case 89:
					if (ch == 's') {AddCh(); state = 90; break;}
					else {t.kind = noSym; break loop;}
				case 90:
					if (ch == 'c') {AddCh(); state = 91; break;}
					else {t.kind = noSym; break loop;}
				case 91:
					if (ch == 'o') {AddCh(); state = 92; break;}
					else {t.kind = noSym; break loop;}
				case 92:
					if (ch == 'n') {AddCh(); state = 93; break;}
					else {t.kind = noSym; break loop;}
				case 93:
					if (ch == 'n') {AddCh(); state = 94; break;}
					else {t.kind = noSym; break loop;}
				case 94:
					if (ch == 'e') {AddCh(); state = 95; break;}
					else {t.kind = noSym; break loop;}
				case 95:
					if (ch == 'c') {AddCh(); state = 96; break;}
					else {t.kind = noSym; break loop;}
				case 96:
					if (ch == 't') {AddCh(); state = 97; break;}
					else {t.kind = noSym; break loop;}
				case 97:
					{t.kind = 27; break loop;}
				case 98:
					if (ch == 'w') {AddCh(); state = 99; break;}
					else {t.kind = noSym; break loop;}
				case 99:
					if (ch == 'n') {AddCh(); state = 100; break;}
					else {t.kind = noSym; break loop;}
				case 100:
					if (ch == 't') {AddCh(); state = 101; break;}
					else {t.kind = noSym; break loop;}
				case 101:
					if (ch == 'o') {AddCh(); state = 102; break;}
					else {t.kind = noSym; break loop;}
				case 102:
					{t.kind = 28; break loop;}
				case 103:
					{t.kind = 29; break loop;}
				case 104:
					if (ch == 'f') {AddCh(); state = 105; break;}
					else {t.kind = noSym; break loop;}
				case 105:
					{t.kind = 30; break loop;}
				case 106:
					{t.kind = 31; break loop;}
				case 107:
					if (ch == 'i') {AddCh(); state = 108; break;}
					else {t.kind = noSym; break loop;}
				case 108:
					if (ch == 't') {AddCh(); state = 109; break;}
					else {t.kind = noSym; break loop;}
				case 109:
					if (ch == 'y') {AddCh(); state = 110; break;}
					else {t.kind = noSym; break loop;}
				case 110:
					{t.kind = 32; break loop;}
				case 111:
					if (ch == 'i') {AddCh(); state = 112; break;}
					else {t.kind = noSym; break loop;}
				case 112:
					if (ch == 't') {AddCh(); state = 113; break;}
					else {t.kind = noSym; break loop;}
				case 113:
					{t.kind = 33; break loop;}
				case 114:
					if (ch == 'l') {AddCh(); state = 115; break;}
					else {t.kind = noSym; break loop;}
				case 115:
					if (ch == 'e') {AddCh(); state = 116; break;}
					else {t.kind = noSym; break loop;}
				case 116:
					{t.kind = 34; break loop;}
				case 117:
					if (ch == 'r') {AddCh(); state = 118; break;}
					else {t.kind = noSym; break loop;}
				case 118:
					{t.kind = 35; break loop;}
				case 119:
					if (ch == 'n') {AddCh(); state = 120; break;}
					else {t.kind = noSym; break loop;}
				case 120:
					if (ch == 'c') {AddCh(); state = 121; break;}
					else {t.kind = noSym; break loop;}
				case 121:
					if (ch == 't') {AddCh(); state = 122; break;}
					else {t.kind = noSym; break loop;}
				case 122:
					if (ch == 'i') {AddCh(); state = 123; break;}
					else {t.kind = noSym; break loop;}
				case 123:
					if (ch == 'o') {AddCh(); state = 124; break;}
					else {t.kind = noSym; break loop;}
				case 124:
					if (ch == 'n') {AddCh(); state = 125; break;}
					else {t.kind = noSym; break loop;}
				case 125:
					{t.kind = 36; break loop;}
				case 126:
					if (ch == 't') {AddCh(); state = 127; break;}
					else {t.kind = noSym; break loop;}
				case 127:
					if (ch == 'e') {AddCh(); state = 128; break;}
					else {t.kind = noSym; break loop;}
				case 128:
					{t.kind = 37; break loop;}
				case 129:
					if (ch == 'c') {AddCh(); state = 130; break;}
					else {t.kind = noSym; break loop;}
				case 130:
					{t.kind = 38; break loop;}
				case 131:
					if (ch == 'o') {AddCh(); state = 132; break;}
					else {t.kind = noSym; break loop;}
				case 132:
					if (ch == 'u') {AddCh(); state = 133; break;}
					else {t.kind = noSym; break loop;}
				case 133:
					if (ch == 'p') {AddCh(); state = 134; break;}
					else {t.kind = noSym; break loop;}
				case 134:
					{t.kind = 39; break loop;}
				case 135:
					if (ch == 'a') {AddCh(); state = 136; break;}
					else {t.kind = noSym; break loop;}
				case 136:
					if (ch == 'r') {AddCh(); state = 137; break;}
					else {t.kind = noSym; break loop;}
				case 137:
					if (ch == 'd') {AddCh(); state = 138; break;}
					else {t.kind = noSym; break loop;}
				case 138:
					if (ch == 'e') {AddCh(); state = 139; break;}
					else {t.kind = noSym; break loop;}
				case 139:
					if (ch == 'd') {AddCh(); state = 140; break;}
					else {t.kind = noSym; break loop;}
				case 140:
					{t.kind = 40; break loop;}
				case 141:
					{t.kind = 41; break loop;}
				case 142:
					if (ch == 'p') {AddCh(); state = 143; break;}
					else {t.kind = noSym; break loop;}
				case 143:
					if (ch == 'u') {AddCh(); state = 144; break;}
					else {t.kind = noSym; break loop;}
				case 144:
					if (ch == 'r') {AddCh(); state = 145; break;}
					else {t.kind = noSym; break loop;}
				case 145:
					if (ch == 'e') {AddCh(); state = 146; break;}
					else {t.kind = noSym; break loop;}
				case 146:
					{t.kind = 42; break loop;}
				case 147:
					if (ch == 'r') {AddCh(); state = 148; break;}
					else {t.kind = noSym; break loop;}
				case 148:
					if (ch == 't') {AddCh(); state = 149; break;}
					else {t.kind = noSym; break loop;}
				case 149:
					if (ch == 'i') {AddCh(); state = 150; break;}
					else {t.kind = noSym; break loop;}
				case 150:
					if (ch == 'a') {AddCh(); state = 151; break;}
					else {t.kind = noSym; break loop;}
				case 151:
					if (ch == 'l') {AddCh(); state = 152; break;}
					else {t.kind = noSym; break loop;}
				case 152:
					{t.kind = 44; break loop;}
				case 153:
					if (ch == 'u') {AddCh(); state = 154; break;}
					else {t.kind = noSym; break loop;}
				case 154:
					if (ch == 't') {AddCh(); state = 155; break;}
					else {t.kind = noSym; break loop;}
				case 155:
					{t.kind = 45; break loop;}
				case 156:
					{t.kind = 46; break loop;}
				case 157:
					if (ch == 'b') {AddCh(); state = 158; break;}
					else {t.kind = noSym; break loop;}
				case 158:
					if (ch == 'e') {AddCh(); state = 159; break;}
					else {t.kind = noSym; break loop;}
				case 159:
					if (ch == 'l') {AddCh(); state = 160; break;}
					else {t.kind = noSym; break loop;}
				case 160:
					{t.kind = 47; break loop;}
				case 161:
					if (ch == 'r') {AddCh(); state = 162; break;}
					else {t.kind = noSym; break loop;}
				case 162:
					if (ch == 'a') {AddCh(); state = 163; break;}
					else {t.kind = noSym; break loop;}
				case 163:
					if (ch == 'r') {AddCh(); state = 164; break;}
					else {t.kind = noSym; break loop;}
				case 164:
					if (ch == 'y') {AddCh(); state = 165; break;}
					else {t.kind = noSym; break loop;}
				case 165:
					{t.kind = 48; break loop;}
				case 166:
					if (ch == 'k') {AddCh(); state = 167; break;}
					else {t.kind = noSym; break loop;}
				case 167:
					if (ch == 'a') {AddCh(); state = 168; break;}
					else {t.kind = noSym; break loop;}
				case 168:
					if (ch == 'g') {AddCh(); state = 169; break;}
					else {t.kind = noSym; break loop;}
				case 169:
					if (ch == 'e') {AddCh(); state = 170; break;}
					else {t.kind = noSym; break loop;}
				case 170:
					{t.kind = 49; break loop;}
				case 171:
					if (ch == 'e') {AddCh(); state = 172; break;}
					else {t.kind = noSym; break loop;}
				case 172:
					if (ch == 'r') {AddCh(); state = 173; break;}
					else {t.kind = noSym; break loop;}
				case 173:
					if (ch == 'a') {AddCh(); state = 174; break;}
					else {t.kind = noSym; break loop;}
				case 174:
					if (ch == 'l') {AddCh(); state = 175; break;}
					else {t.kind = noSym; break loop;}
				case 175:
					{t.kind = 50; break loop;}
				case 176:
					if (ch == 'o') {AddCh(); state = 177; break;}
					else {t.kind = noSym; break loop;}
				case 177:
					if (ch == 'p') {AddCh(); state = 178; break;}
					else {t.kind = noSym; break loop;}
				case 178:
					{t.kind = 51; break loop;}
				case 179:
					if (ch == 'p') {AddCh(); state = 180; break;}
					else {t.kind = noSym; break loop;}
				case 180:
					{t.kind = 52; break loop;}
				case 181:
					if (ch == 'd') {AddCh(); state = 182; break;}
					else {t.kind = noSym; break loop;}
				case 182:
					{t.kind = 53; break loop;}
				case 183:
					if (ch == 'n') {AddCh(); state = 184; break;}
					else {t.kind = noSym; break loop;}
				case 184:
					if (ch == 'd') {AddCh(); state = 185; break;}
					else {t.kind = noSym; break loop;}
				case 185:
					{t.kind = 54; break loop;}
				case 186:
					{t.kind = 55; break loop;}
				case 187:
					if (ch == 't') {AddCh(); state = 188; break;}
					else {t.kind = noSym; break loop;}
				case 188:
					{t.kind = 56; break loop;}
				case 189:
					{t.kind = 57; break loop;}
				case 190:
					{t.kind = 58; break loop;}
				case 191:
					if (ch == 'l') {AddCh(); state = 192; break;}
					else {t.kind = noSym; break loop;}
				case 192:
					if (ch == 'l') {AddCh(); state = 193; break;}
					else {t.kind = noSym; break loop;}
				case 193:
					{t.kind = 59; break loop;}
				case 194:
					{t.kind = 60; break loop;}
				case 195:
					{t.kind = 61; break loop;}
				case 196:
					if (ch == 'e') {AddCh(); state = 197; break;}
					else {t.kind = noSym; break loop;}
				case 197:
					if (ch == 'n') {AddCh(); state = 198; break;}
					else {t.kind = noSym; break loop;}
				case 198:
					{t.kind = 62; break loop;}
				case 199:
					{t.kind = 63; break loop;}
				case 200:
					if (ch == 'h') {AddCh(); state = 201; break;}
					else {t.kind = noSym; break loop;}
				case 201:
					if (ch == 'e') {AddCh(); state = 202; break;}
					else {t.kind = noSym; break loop;}
				case 202:
					if (ch == 'r') {AddCh(); state = 203; break;}
					else {t.kind = noSym; break loop;}
				case 203:
					if (ch == 's') {AddCh(); state = 204; break;}
					else {t.kind = noSym; break loop;}
				case 204:
					{t.kind = 64; break loop;}
				case 205:
					if (ch == 't') {AddCh(); state = 206; break;}
					else {t.kind = noSym; break loop;}
				case 206:
					{t.kind = 65; break loop;}
				case 207:
					if (ch == 'c') {AddCh(); state = 208; break;}
					else {t.kind = noSym; break loop;}
				case 208:
					if (ch == 'k') {AddCh(); state = 209; break;}
					else {t.kind = noSym; break loop;}
				case 209:
					if (ch == 'a') {AddCh(); state = 210; break;}
					else {t.kind = noSym; break loop;}
				case 210:
					if (ch == 'g') {AddCh(); state = 211; break;}
					else {t.kind = noSym; break loop;}
				case 211:
					if (ch == 'e') {AddCh(); state = 212; break;}
					else {t.kind = noSym; break loop;}
				case 212:
					{t.kind = 66; break loop;}
				case 213:
					if (ch == 't') {AddCh(); state = 214; break;}
					else {t.kind = noSym; break loop;}
				case 214:
					{t.kind = 67; break loop;}
				case 215:
					if (ch == 't') {AddCh(); state = 216; break;}
					else {t.kind = noSym; break loop;}
				case 216:
					if (ch == 'p') {AddCh(); state = 217; break;}
					else {t.kind = noSym; break loop;}
				case 217:
					if (ch == 'o') {AddCh(); state = 218; break;}
					else {t.kind = noSym; break loop;}
				case 218:
					if (ch == 'n') {AddCh(); state = 219; break;}
					else {t.kind = noSym; break loop;}
				case 219:
					if (ch == 'e') {AddCh(); state = 220; break;}
					else {t.kind = noSym; break loop;}
				case 220:
					if (ch == 'd') {AddCh(); state = 221; break;}
					else {t.kind = noSym; break loop;}
				case 221:
					{t.kind = 68; break loop;}
				case 222:
					if (ch == 'u') {AddCh(); state = 223; break;}
					else {t.kind = noSym; break loop;}
				case 223:
					if (ch == 'r') {AddCh(); state = 224; break;}
					else {t.kind = noSym; break loop;}
				case 224:
					if (ch == 'e') {AddCh(); state = 225; break;}
					else {t.kind = noSym; break loop;}
				case 225:
					{t.kind = 69; break loop;}
				case 226:
					if (ch == 's') {AddCh(); state = 227; break;}
					else {t.kind = noSym; break loop;}
				case 227:
					{t.kind = 70; break loop;}
				case 228:
					if (ch == 'e') {AddCh(); state = 229; break;}
					else {t.kind = noSym; break loop;}
				case 229:
					if (ch == 'c') {AddCh(); state = 230; break;}
					else {t.kind = noSym; break loop;}
				case 230:
					if (ch == 't') {AddCh(); state = 231; break;}
					else {t.kind = noSym; break loop;}
				case 231:
					if (ch == 'e') {AddCh(); state = 232; break;}
					else {t.kind = noSym; break loop;}
				case 232:
					if (ch == 'd') {AddCh(); state = 233; break;}
					else {t.kind = noSym; break loop;}
				case 233:
					{t.kind = 71; break loop;}
				case 234:
					if (ch == 'r') {AddCh(); state = 235; break;}
					else {t.kind = noSym; break loop;}
				case 235:
					if (ch == 'e') {AddCh(); state = 236; break;}
					else {t.kind = noSym; break loop;}
				case 236:
					{t.kind = 72; break loop;}
				case 237:
					if (ch == 'n') {AddCh(); state = 238; break;}
					else {t.kind = noSym; break loop;}
				case 238:
					if (ch == 'g') {AddCh(); state = 239; break;}
					else {t.kind = noSym; break loop;}
				case 239:
					if (ch == 'e') {AddCh(); state = 240; break;}
					else {t.kind = noSym; break loop;}
				case 240:
					{t.kind = 73; break loop;}
				case 241:
					if (ch == 'o') {AddCh(); state = 242; break;}
					else {t.kind = noSym; break loop;}
				case 242:
					if (ch == 'r') {AddCh(); state = 243; break;}
					else {t.kind = noSym; break loop;}
				case 243:
					if (ch == 'd') {AddCh(); state = 244; break;}
					else {t.kind = noSym; break loop;}
				case 244:
					{t.kind = 74; break loop;}
				case 245:
					if (ch == 'i') {AddCh(); state = 246; break;}
					else {t.kind = noSym; break loop;}
				case 246:
					if (ch == 's') {AddCh(); state = 247; break;}
					else {t.kind = noSym; break loop;}
				case 247:
					if (ch == 't') {AddCh(); state = 248; break;}
					else {t.kind = noSym; break loop;}
				case 248:
					if (ch == 'e') {AddCh(); state = 249; break;}
					else {t.kind = noSym; break loop;}
				case 249:
					if (ch == 'r') {AddCh(); state = 250; break;}
					else {t.kind = noSym; break loop;}
				case 250:
					{t.kind = 75; break loop;}
				case 251:
					if (ch == 'e') {AddCh(); state = 252; break;}
					else {t.kind = noSym; break loop;}
				case 252:
					if (ch == 'c') {AddCh(); state = 253; break;}
					else {t.kind = noSym; break loop;}
				case 253:
					if (ch == 't') {AddCh(); state = 254; break;}
					else {t.kind = noSym; break loop;}
				case 254:
					{t.kind = 76; break loop;}
				case 255:
					{t.kind = 77; break loop;}
				case 256:
					if (ch == 'o') {AddCh(); state = 257; break;}
					else {t.kind = noSym; break loop;}
				case 257:
					if (ch == 'r') {AddCh(); state = 258; break;}
					else {t.kind = noSym; break loop;}
				case 258:
					if (ch == 't') {AddCh(); state = 259; break;}
					else {t.kind = noSym; break loop;}
				case 259:
					{t.kind = 78; break loop;}
				case 260:
					if (ch == 'u') {AddCh(); state = 261; break;}
					else {t.kind = noSym; break loop;}
				case 261:
					if (ch == 'r') {AddCh(); state = 262; break;}
					else {t.kind = noSym; break loop;}
				case 262:
					if (ch == 'n') {AddCh(); state = 263; break;}
					else {t.kind = noSym; break loop;}
				case 263:
					{t.kind = 79; break loop;}
				case 264:
					{t.kind = 80; break loop;}
				case 265:
					{t.kind = 81; break loop;}
				case 266:
					if (ch == 'e') {AddCh(); state = 267; break;}
					else {t.kind = noSym; break loop;}
				case 267:
					if (ch == 'c') {AddCh(); state = 268; break;}
					else {t.kind = noSym; break loop;}
				case 268:
					if (ch == 't') {AddCh(); state = 269; break;}
					else {t.kind = noSym; break loop;}
				case 269:
					{t.kind = 82; break loop;}
				case 270:
					if (ch == 'e') {AddCh(); state = 271; break;}
					else {t.kind = noSym; break loop;}
				case 271:
					if (ch == 'r') {AddCh(); state = 272; break;}
					else {t.kind = noSym; break loop;}
				case 272:
					if (ch == 'i') {AddCh(); state = 273; break;}
					else {t.kind = noSym; break loop;}
				case 273:
					if (ch == 't') {AddCh(); state = 274; break;}
					else {t.kind = noSym; break loop;}
				case 274:
					if (ch == 'y') {AddCh(); state = 275; break;}
					else {t.kind = noSym; break loop;}
				case 275:
					{t.kind = 83; break loop;}
				case 276:
					if (ch == 'a') {AddCh(); state = 277; break;}
					else {t.kind = noSym; break loop;}
				case 277:
					if (ch == 'r') {AddCh(); state = 278; break;}
					else {t.kind = noSym; break loop;}
				case 278:
					if (ch == 'e') {AddCh(); state = 279; break;}
					else {t.kind = noSym; break loop;}
				case 279:
					if (ch == 'd') {AddCh(); state = 280; break;}
					else {t.kind = noSym; break loop;}
				case 280:
					{t.kind = 84; break loop;}
				case 281:
					if (ch == 'g') {AddCh(); state = 282; break;}
					else {t.kind = noSym; break loop;}
				case 282:
					if (ch == 'n') {AddCh(); state = 283; break;}
					else {t.kind = noSym; break loop;}
				case 283:
					if (ch == 'a') {AddCh(); state = 284; break;}
					else {t.kind = noSym; break loop;}
				case 284:
					if (ch == 'l') {AddCh(); state = 285; break;}
					else {t.kind = noSym; break loop;}
				case 285:
					{t.kind = 85; break loop;}
				case 286:
					{t.kind = 86; break loop;}
				case 287:
					{t.kind = 87; break loop;}
				case 288:
					{t.kind = 88; break loop;}
				case 289:
					{t.kind = 89; break loop;}
				case 290:
					if (ch == 'b') {AddCh(); state = 291; break;}
					else {t.kind = noSym; break loop;}
				case 291:
					if (ch == 't') {AddCh(); state = 292; break;}
					else {t.kind = noSym; break loop;}
				case 292:
					if (ch == 'y') {AddCh(); state = 293; break;}
					else {t.kind = noSym; break loop;}
				case 293:
					if (ch == 'p') {AddCh(); state = 294; break;}
					else {t.kind = noSym; break loop;}
				case 294:
					if (ch == 'e') {AddCh(); state = 295; break;}
					else {t.kind = noSym; break loop;}
				case 295:
					{t.kind = 90; break loop;}
				case 296:
					if (ch == 'e') {AddCh(); state = 297; break;}
					else {t.kind = noSym; break loop;}
				case 297:
					if (ch == 'n') {AddCh(); state = 298; break;}
					else {t.kind = noSym; break loop;}
				case 298:
					{t.kind = 91; break loop;}
				case 299:
					{t.kind = 92; break loop;}
				case 300:
					if (ch == 'a') {AddCh(); state = 301; break;}
					else {t.kind = noSym; break loop;}
				case 301:
					if (ch == 'n') {AddCh(); state = 302; break;}
					else {t.kind = noSym; break loop;}
				case 302:
					if (ch == 's') {AddCh(); state = 303; break;}
					else {t.kind = noSym; break loop;}
				case 303:
					if (ch == 'p') {AddCh(); state = 304; break;}
					else {t.kind = noSym; break loop;}
				case 304:
					if (ch == 'o') {AddCh(); state = 305; break;}
					else {t.kind = noSym; break loop;}
				case 305:
					if (ch == 'r') {AddCh(); state = 306; break;}
					else {t.kind = noSym; break loop;}
				case 306:
					if (ch == 't') {AddCh(); state = 307; break;}
					else {t.kind = noSym; break loop;}
				case 307:
					{t.kind = 93; break loop;}
				case 308:
					if (ch == 'p') {AddCh(); state = 309; break;}
					else {t.kind = noSym; break loop;}
				case 309:
					if (ch == 'e') {AddCh(); state = 310; break;}
					else {t.kind = noSym; break loop;}
				case 310:
					{t.kind = 94; break loop;}
				case 311:
					if (ch == 'f') {AddCh(); state = 312; break;}
					else {t.kind = noSym; break loop;}
				case 312:
					if (ch == 'f') {AddCh(); state = 313; break;}
					else {t.kind = noSym; break loop;}
				case 313:
					if (ch == 'e') {AddCh(); state = 314; break;}
					else {t.kind = noSym; break loop;}
				case 314:
					if (ch == 'c') {AddCh(); state = 315; break;}
					else {t.kind = noSym; break loop;}
				case 315:
					if (ch == 't') {AddCh(); state = 316; break;}
					else {t.kind = noSym; break loop;}
				case 316:
					if (ch == 'e') {AddCh(); state = 317; break;}
					else {t.kind = noSym; break loop;}
				case 317:
					if (ch == 'd') {AddCh(); state = 318; break;}
					else {t.kind = noSym; break loop;}
				case 318:
					{t.kind = 95; break loop;}
				case 319:
					if (ch == 't') {AddCh(); state = 320; break;}
					else {t.kind = noSym; break loop;}
				case 320:
					if (ch == 's') {AddCh(); state = 321; break;}
					else {t.kind = noSym; break loop;}
				case 321:
					{t.kind = 96; break loop;}
				case 322:
					if (ch == 'i') {AddCh(); state = 323; break;}
					else {t.kind = noSym; break loop;}
				case 323:
					if (ch == 'l') {AddCh(); state = 324; break;}
					else {t.kind = noSym; break loop;}
				case 324:
					{t.kind = 97; break loop;}
				case 325:
					if (ch == 'e') {AddCh(); state = 326; break;}
					else {t.kind = noSym; break loop;}
				case 326:
					{t.kind = 98; break loop;}
				case 327:
					if (ch == 'r') {AddCh(); state = 328; break;}
					else {t.kind = noSym; break loop;}
				case 328:
					if (ch == 'i') {AddCh(); state = 329; break;}
					else {t.kind = noSym; break loop;}
				case 329:
					if (ch == 'a') {AddCh(); state = 330; break;}
					else {t.kind = noSym; break loop;}
				case 330:
					if (ch == 'b') {AddCh(); state = 331; break;}
					else {t.kind = noSym; break loop;}
				case 331:
					if (ch == 'l') {AddCh(); state = 332; break;}
					else {t.kind = noSym; break loop;}
				case 332:
					if (ch == 'e') {AddCh(); state = 333; break;}
					else {t.kind = noSym; break loop;}
				case 333:
					{t.kind = 99; break loop;}
				case 334:
					if (ch == 'i') {AddCh(); state = 335; break;}
					else {t.kind = noSym; break loop;}
				case 335:
					if (ch == 't') {AddCh(); state = 336; break;}
					else {t.kind = noSym; break loop;}
				case 336:
					{t.kind = 100; break loop;}
				case 337:
					if (ch == 'n') {AddCh(); state = 338; break;}
					else {t.kind = noSym; break loop;}
				case 338:
					{t.kind = 101; break loop;}
				case 339:
					if (ch == 'l') {AddCh(); state = 340; break;}
					else {t.kind = noSym; break loop;}
				case 340:
					if (ch == 'e') {AddCh(); state = 341; break;}
					else {t.kind = noSym; break loop;}
				case 341:
					{t.kind = 102; break loop;}
				case 342:
					if (ch == 't') {AddCh(); state = 343; break;}
					else {t.kind = noSym; break loop;}
				case 343:
					if (ch == 'h') {AddCh(); state = 344; break;}
					else {t.kind = noSym; break loop;}
				case 344:
					{t.kind = 103; break loop;}
				case 345:
					if (ch == 'o') {AddCh(); state = 346; break;}
					else {t.kind = noSym; break loop;}
				case 346:
					if (ch == 'r') {AddCh(); state = 347; break;}
					else {t.kind = noSym; break loop;}
				case 347:
					{t.kind = 104; break loop;}
				case 348:
					if (ch == 'r') {AddCh(); state = 349; break;}
					else {t.kind = noSym; break loop;}
				case 349:
					{t.kind = 105; break loop;}
				case 350:
					{t.kind = 106; break loop;}
				case 351:
					{t.kind = 107; break loop;}
				case 352:
					{t.kind = 108; break loop;}
				case 353:
					{t.kind = 109; break loop;}
				case 354:
					{t.kind = 110; break loop;}
				case 355:
					{t.kind = 111; break loop;}
				case 356:
					{t.kind = 112; break loop;}
				case 357:
					{t.kind = 113; break loop;}
				case 358:
					{t.kind = 114; break loop;}
				case 359:
					{t.kind = 115; break loop;}
				case 360:
					{t.kind = 116; break loop;}
				case 361:
					{t.kind = 117; break loop;}
				case 362:
					{t.kind = 118; break loop;}
				case 363:
					{t.kind = 119; break loop;}
				case 364:
					{t.kind = 120; break loop;}
				case 365:
					{t.kind = 124; break loop;}
				case 366:
					{t.kind = 125; break loop;}
				case 367:
					{t.kind = 129; break loop;}
				case 368:
					{t.kind = 130; break loop;}
				case 369:
					if (ch == 'l') {AddCh(); state = 395; break;}
					else if (ch == 'e') {AddCh(); state = 49; break;}
					else if (ch == 'o') {AddCh(); state = 56; break;}
					else if (ch == 'u') {AddCh(); state = 396; break;}
					else {t.kind = 2; break loop;}
				case 370:
					if (ch == 'b') {AddCh(); state = 6; break;}
					else if (ch == 'c') {AddCh(); state = 8; break;}
					else if (ch == 'f') {AddCh(); state = 13; break;}
					else if (ch == 'l') {AddCh(); state = 397; break;}
					else if (ch == 'n') {AddCh(); state = 21; break;}
					else if (ch == 'r') {AddCh(); state = 398; break;}
					else if (ch == 's') {AddCh(); state = 36; break;}
					else if (ch == 't') {AddCh(); state = 41; break;}
					else {t.kind = 2; break loop;}
				case 371:
					if (ch == 'a') {AddCh(); state = 64; break;}
					else if (ch == 'o') {AddCh(); state = 399; break;}
					else {t.kind = 2; break loop;}
				case 372:
					if (ch == 'i') {AddCh(); state = 89; break;}
					else if (ch == 'o') {AddCh(); state = 98; break;}
					else {t.kind = 2; break loop;}
				case 373:
					if (ch == 'l') {AddCh(); state = 400; break;}
					else if (ch == 'n') {AddCh(); state = 401; break;}
					else if (ch == 'x') {AddCh(); state = 111; break;}
					else {t.kind = 2; break loop;}
				case 374:
					if (ch == 'i') {AddCh(); state = 114; break;}
					else if (ch == 'o') {AddCh(); state = 117; break;}
					else if (ch == 'u') {AddCh(); state = 119; break;}
					else {t.kind = 2; break loop;}
				case 375:
					if (ch == 'e') {AddCh(); state = 402; break;}
					else if (ch == 'r') {AddCh(); state = 131; break;}
					else if (ch == 'u') {AddCh(); state = 135; break;}
					else {t.kind = 2; break loop;}
				case 376:
					if (ch == 'f') {AddCh(); state = 141; break;}
					else if (ch == 'm') {AddCh(); state = 142; break;}
					else if (ch == 'n') {AddCh(); state = 403; break;}
					else if (ch == 's') {AddCh(); state = 156; break;}
					else {t.kind = 2; break loop;}
				case 377:
					if (ch == 'a') {AddCh(); state = 157; break;}
					else if (ch == 'i') {AddCh(); state = 404; break;}
					else if (ch == 'o') {AddCh(); state = 176; break;}
					else {t.kind = 2; break loop;}
				case 378:
					if (ch == 'a') {AddCh(); state = 179; break;}
					else if (ch == 'o') {AddCh(); state = 181; break;}
					else {t.kind = 2; break loop;}
				case 379:
					if (ch == 'a') {AddCh(); state = 183; break;}
					else if (ch == 'e') {AddCh(); state = 405; break;}
					else if (ch == 'o') {AddCh(); state = 406; break;}
					else if (ch == 'u') {AddCh(); state = 191; break;}
					else {t.kind = 2; break loop;}
				case 380:
					if (ch == 'f') {AddCh(); state = 194; break;}
					else if (ch == 'n') {AddCh(); state = 195; break;}
					else if (ch == 'p') {AddCh(); state = 196; break;}
					else if (ch == 'r') {AddCh(); state = 199; break;}
					else if (ch == 't') {AddCh(); state = 200; break;}
					else if (ch == 'u') {AddCh(); state = 205; break;}
					else {t.kind = 2; break loop;}
				case 381:
					if (ch == 'a') {AddCh(); state = 207; break;}
					else if (ch == 'o') {AddCh(); state = 407; break;}
					else if (ch == 'r') {AddCh(); state = 408; break;}
					else if (ch == 'u') {AddCh(); state = 234; break;}
					else {t.kind = 2; break loop;}
				case 382:
					if (ch == 'a') {AddCh(); state = 237; break;}
					else if (ch == 'e') {AddCh(); state = 409; break;}
					else if (ch == 'o') {AddCh(); state = 410; break;}
					else {t.kind = 2; break loop;}
				case 383:
					if (ch == 'e') {AddCh(); state = 411; break;}
					else if (ch == 'h') {AddCh(); state = 276; break;}
					else if (ch == 'i') {AddCh(); state = 281; break;}
					else if (ch == 'l') {AddCh(); state = 412; break;}
					else if (ch == 'r') {AddCh(); state = 413; break;}
					else if (ch == 'u') {AddCh(); state = 290; break;}
					else {t.kind = 2; break loop;}
				case 384:
					if (ch == 'h') {AddCh(); state = 296; break;}
					else if (ch == 'o') {AddCh(); state = 299; break;}
					else if (ch == 'r') {AddCh(); state = 300; break;}
					else if (ch == 'y') {AddCh(); state = 308; break;}
					else {t.kind = 2; break loop;}
				case 385:
					if (ch == 'n') {AddCh(); state = 414; break;}
					else if (ch == 's') {AddCh(); state = 325; break;}
					else {t.kind = 2; break loop;}
				case 386:
					if (ch == 'a') {AddCh(); state = 327; break;}
					else {t.kind = 2; break loop;}
				case 387:
					if (ch == 'a') {AddCh(); state = 334; break;}
					else if (ch == 'h') {AddCh(); state = 415; break;}
					else if (ch == 'i') {AddCh(); state = 342; break;}
					else {t.kind = 2; break loop;}
				case 388:
					if (ch == 'n') {AddCh(); state = 345; break;}
					else if (ch == 'o') {AddCh(); state = 348; break;}
					else {t.kind = 2; break loop;}
				case 389:
					if (ch == '*') {AddCh(); state = 350; break;}
					else {t.kind = 122; break loop;}
				case 390:
					if (ch == '=') {AddCh(); state = 351; break;}
					else if (ch == '>') {AddCh(); state = 356; break;}
					else {t.kind = 126; break loop;}
				case 391:
					if (ch == '=') {AddCh(); state = 352; break;}
					else {t.kind = 127; break loop;}
				case 392:
					if (ch == '>') {AddCh(); state = 353; break;}
					else {t.kind = 128; break loop;}
				case 393:
					if (ch == '=') {AddCh(); state = 354; break;}
					else {t.kind = 123; break loop;}
				case 394:
					if (ch == '=') {AddCh(); state = 355; break;}
					else {t.kind = 121; break loop;}
				case 395:
					if (ch == 'u') {AddCh(); state = 416; break;}
					else if (ch == 'o') {AddCh(); state = 53; break;}
					else {t.kind = noSym; break loop;}
				case 396:
					if (ch == 'f') {AddCh(); state = 59; break;}
					else if (ch == 's') {AddCh(); state = 63; break;}
					else {t.kind = noSym; break loop;}
				case 397:
					if (ch == 'i') {AddCh(); state = 17; break;}
					else if (ch == 'l') {AddCh(); state = 20; break;}
					else {t.kind = noSym; break loop;}
				case 398:
					if (ch == 'c') {AddCh(); state = 23; break;}
					else if (ch == 'r') {AddCh(); state = 33; break;}
					else {t.kind = noSym; break loop;}
				case 399:
					if (ch == 'm') {AddCh(); state = 67; break;}
					else if (ch == 'n') {AddCh(); state = 417; break;}
					else {t.kind = noSym; break loop;}
				case 400:
					if (ch == 's') {AddCh(); state = 418; break;}
					else {t.kind = noSym; break loop;}
				case 401:
					if (ch == 'd') {AddCh(); state = 106; break;}
					else if (ch == 't') {AddCh(); state = 107; break;}
					else {t.kind = noSym; break loop;}
				case 402:
					if (ch == 'n') {AddCh(); state = 419; break;}
					else {t.kind = noSym; break loop;}
				case 403:
					if (ch == 'e') {AddCh(); state = 147; break;}
					else if (ch == 'o') {AddCh(); state = 153; break;}
					else {t.kind = 43; break loop;}
				case 404:
					if (ch == 'b') {AddCh(); state = 161; break;}
					else if (ch == 'n') {AddCh(); state = 166; break;}
					else if (ch == 't') {AddCh(); state = 171; break;}
					else {t.kind = noSym; break loop;}
				case 405:
					if (ch == 'w') {AddCh(); state = 186; break;}
					else if (ch == 'x') {AddCh(); state = 187; break;}
					else {t.kind = noSym; break loop;}
				case 406:
					if (ch == 'r') {AddCh(); state = 189; break;}
					else if (ch == 't') {AddCh(); state = 190; break;}
					else {t.kind = noSym; break loop;}
				case 407:
					if (ch == 'r') {AddCh(); state = 213; break;}
					else if (ch == 's') {AddCh(); state = 215; break;}
					else {t.kind = noSym; break loop;}
				case 408:
					if (ch == 'o') {AddCh(); state = 420; break;}
					else {t.kind = noSym; break loop;}
				case 409:
					if (ch == 'c') {AddCh(); state = 241; break;}
					else if (ch == 'g') {AddCh(); state = 245; break;}
					else if (ch == 'j') {AddCh(); state = 251; break;}
					else if (ch == 'm') {AddCh(); state = 255; break;}
					else if (ch == 'p') {AddCh(); state = 256; break;}
					else if (ch == 't') {AddCh(); state = 260; break;}
					else {t.kind = noSym; break loop;}
				case 410:
					if (ch == 'l') {AddCh(); state = 264; break;}
					else if (ch == 'r') {AddCh(); state = 265; break;}
					else {t.kind = noSym; break loop;}
				case 411:
					if (ch == 'l') {AddCh(); state = 266; break;}
					else if (ch == 'v') {AddCh(); state = 270; break;}
					else {t.kind = noSym; break loop;}
				case 412:
					if (ch == 'a') {AddCh(); state = 286; break;}
					else if (ch == 'l') {AddCh(); state = 287; break;}
					else {t.kind = noSym; break loop;}
				case 413:
					if (ch == 'a') {AddCh(); state = 288; break;}
					else if (ch == 'l') {AddCh(); state = 289; break;}
					else {t.kind = noSym; break loop;}
				case 414:
					if (ch == 'a') {AddCh(); state = 311; break;}
					else if (ch == 'i') {AddCh(); state = 319; break;}
					else if (ch == 't') {AddCh(); state = 322; break;}
					else {t.kind = noSym; break loop;}
				case 415:
					if (ch == 'e') {AddCh(); state = 337; break;}
					else if (ch == 'i') {AddCh(); state = 339; break;}
					else {t.kind = noSym; break loop;}
				case 416:
					if (ch == 'b') {AddCh(); state = 421; break;}
					else {t.kind = noSym; break loop;}
				case 417:
					if (ch == 'f') {AddCh(); state = 74; break;}
					else if (ch == 's') {AddCh(); state = 84; break;}
					else {t.kind = noSym; break loop;}
				case 418:
					if (ch == 'e') {AddCh(); state = 103; break;}
					else if (ch == 'i') {AddCh(); state = 104; break;}
					else {t.kind = noSym; break loop;}
				case 419:
					if (ch == 'e') {AddCh(); state = 422; break;}
					else {t.kind = noSym; break loop;}
				case 420:
					if (ch == 'c') {AddCh(); state = 423; break;}
					else if (ch == 't') {AddCh(); state = 228; break;}
					else {t.kind = noSym; break loop;}
				case 421:
					if (ch == 'x') {AddCh(); state = 424; break;}
					else {t.kind = 3; break loop;}
				case 422:
					if (ch == 'r') {AddCh(); state = 425; break;}
					else {t.kind = noSym; break loop;}
				case 423:
					if (ch == 'e') {AddCh(); state = 426; break;}
					else {t.kind = noSym; break loop;}
				case 424:
					if (ch == '2') {AddCh(); state = 427; break;}
					else if (ch == '3') {AddCh(); state = 4; break;}
					else {t.kind = 4; break loop;}
				case 425:
					if (ch == 'a') {AddCh(); state = 126; break;}
					else if (ch == 'i') {AddCh(); state = 129; break;}
					else {t.kind = noSym; break loop;}
				case 426:
					if (ch == 'd') {AddCh(); state = 222; break;}
					else if (ch == 's') {AddCh(); state = 226; break;}
					else {t.kind = noSym; break loop;}
				case 427:
					if (ch == '4') {AddCh(); state = 5; break;}
					else {t.kind = 5; break loop;}
				case 428:
					if (ch == 'd') {AddCh(); state = 429; break;}
					else {t.kind = noSym; break loop;}
				case 429:
					if (ch == 'f') {AddCh(); state = 430; break;}
					else {t.kind = noSym; break loop;}
				case 430:
					if (ch == 'k') {AddCh(); state = 431; break;}
					else {t.kind = noSym; break loop;}
				case 431:
					if (ch == 'j') {AddCh(); state = 432; break;}
					else {t.kind = noSym; break loop;}
				case 432:
					if (ch == 246) {AddCh(); state = 433; break;}
					else {t.kind = noSym; break loop;}
				case 433:
					if (ch == 'l') {AddCh(); state = 434; break;}
					else {t.kind = noSym; break loop;}
				case 434:
					{t.kind = 131; break loop;}
				case 435:
					{t.kind = 132; break loop;}
				case 436:
					if (ch == 'd') {AddCh(); state = 437; break;}
					else {t.kind = noSym; break loop;}
				case 437:
					if (ch == 'f') {AddCh(); state = 438; break;}
					else {t.kind = noSym; break loop;}
				case 438:
					{t.kind = 133; break loop;}
				case 439:
					if (ch == 'l') {AddCh(); state = 428; break;}
					else {t.kind = 2; break loop;}
				case 440:
					if (ch == 'k') {AddCh(); state = 441; break;}
					else {t.kind = noSym; break loop;}
				case 441:
					if (ch == 's') {AddCh(); state = 442; break;}
					else {t.kind = noSym; break loop;}
				case 442:
					if (ch == 'd') {AddCh(); state = 443; break;}
					else {t.kind = noSym; break loop;}
				case 443:
					if (ch == 'f') {AddCh(); state = 435; break;}
					else if (ch == 's') {AddCh(); state = 436; break;}
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

