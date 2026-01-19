program ex; 

function AUX(a:integer;b:string):integer;
var
begin
  a := 2;
  writeln(b);
  AUX := a;
end;

var
  x:integer;
  y:string;
  r:integer;

begin 
  x:=1;
  y:='string';
  r:=AUX(x,y);
  writeln(r);
end. 

