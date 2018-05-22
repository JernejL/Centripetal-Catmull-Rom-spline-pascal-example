program curves_test;

uses
  Forms,
  u_curvestest in 'u_curvestest.pas' {Form1};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.

