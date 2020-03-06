program ConversorBitmap;

uses
  System.StartUpCopy,
  FMX.Forms,
  UPrincipal in 'UPrincipal.pas' {Form2},
  BitmapConvert in 'BitmapConvert.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm2, Form2);
  Application.Run;
end.
