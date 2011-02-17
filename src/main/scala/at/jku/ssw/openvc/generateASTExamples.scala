package at.jku.ssw.openvc

import java.io.ByteArrayInputStream

object generateASTExamples {
  def main(arguments: Array[String]): Unit = {
    /*
    val code = """
    	if expr1 then
    		statement1;
    	elsif expr2 then
    		statement2;
    	else
    		statment3;
    	end if;
    """
    val code = """
    	while expression loop
    	  statement1;
    	  statement2;
    	end loop;
    """
    val code="a + b sll 1 <= c*d and not e "
    */
    val code = "constant PI : real := 3.14159_26535_89793_23846;"
    val scanner = new Scanner(new ByteArrayInputStream(code.getBytes("utf-8")))
    val parser = new Parser(scanner)
    parser.init
    //println(parser.IfStatement(null))
    //println(parser.LoopStatement(null))
    //println(parser.Expression)
    println(parser.ConstantDeclaration)
  }
}