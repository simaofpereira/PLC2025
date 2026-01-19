program TesteWhileExpressoes;
var
  x, y: integer;
  ok: boolean;
begin
  x := 10;
  y := 0;
  ok := true;

  while (x > 0) and ok do
    if (x > 2) or not ok then
      x := x - 1
    else
      ok := false;

  writeln('Resultado final: ', x)
end.
