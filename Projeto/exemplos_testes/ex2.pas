program TesteIfAninhado;
var
  a, b, c: integer;
begin
  readln(a);
  readln(b);
  readln(c);

  if a < b then
    if b < c then
      writeln('a < b < c')
    else
      writeln('a < b mas b >= c');

  writeln('Fim do programa')
end.
