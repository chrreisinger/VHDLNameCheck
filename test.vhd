package alu_tb is
end alu_tb;

package body alu_tb is
  constant foo_bar : integer;
  variable x       : real;
  type     myInt is range 0 to 10;
  
  function foo_df (
    constant foo_ : integer)
    return integer is
  begin  -- foo_df
    report myInt'left severity note : 
  end foo_df;

end alu_tb;
