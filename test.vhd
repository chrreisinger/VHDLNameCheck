package alu_tb is
end alu_tb;

package body alu_tb is
  
  type myEnum is (first, second, third);
  type myAccess is access integer;
  type myFile is file of integer;
  type myArray is array(0 to 5) of bit;
  type myRec is record
    x : integer;
    y : integer;
    z : myArray;
  end record;
  type myEnumArray is array (0 to 5) of myEnum;
  
    --constant x : myEnumArray := (first, first, second, second, third, third);
    --constant y : myEnumArray := (second, second, third, third, first, first);

type SharedCounter is protected
    procedure increment (N : integer := 1);
    procedure decrement (N : integer := 1);
    impure function value return integer;
end protected SharedCounter;

type SharedCounter is protected body
  variable counter : integer := 0;
  procedure increment (N : integer := 1) is
  begin
    counter := counter + N;
  end procedure increment;
  
  procedure decrement (N : integer := 1) is
  begin
    counter := counter - N;
  end procedure decrement;

  impure function value return integer is
  begin
    return counter;
 end function value;

 type myEnum is (first, second, third);
end protected body SharedCounter;

shared variable x : SharedCounter;

procedure main is
  --variable x : integer:=10#2429#e4;
  type     myInt is range 5 to 10;
  variable a : integer range 3 to 11;
  type     integerFile is file of integer;
  file f     : integerFile open read_mode is "output.txt";
  type myEnum is (first, second, third);
  type fooRec is record
                   x : integer;
                   y : real;
                   --z: myEnumArray;
  end record;
begin  -- main
  report myEnum'image(first) severity note;
  --test;
  --file_open(f, "output.txt", read_mode);
  report "start" severity note;
  while not endfile(f) loop
    --read(f, a);
  end loop;
end main;

end alu_tb;
