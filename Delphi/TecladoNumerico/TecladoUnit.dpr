program TecladoUnit;

uses
  System.StartUpCopy,
  FMX.Forms,
  UPrincipal in 'UPrincipal.pas' {FrmPrincipal},
  Component.Keyboard.Functions in 'Units\Component.Keyboard.Functions.pas',
  Component.Keyboard.Objects in 'Units\Component.Keyboard.Objects.pas',
  Component.Keyboard in 'Units\Component.Keyboard.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFrmPrincipal, FrmPrincipal);
  Application.Run;
end.
