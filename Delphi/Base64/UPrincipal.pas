unit UPrincipal;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Objects,
  FMX.Edit, FMX.Controls.Presentation, FMX.ScrollBox, FMX.Memo, FMX.StdCtrls,
  UBase64;

type
  TForm2 = class(TForm)
    Rectangle1: TRectangle;
    Rectangle2: TRectangle;
    Rectangle3: TRectangle;
    Rectangle4: TRectangle;
    Text1: TText;
    Text2: TText;
    Rectangle5: TRectangle;
    Text3: TText;
    memeDecode: TMemo;
    Rectangle6: TRectangle;
    Text4: TText;
    edtDecode: TEdit;
    btnDecode: TButton;
    Rectangle7: TRectangle;
    Text5: TText;
    edtEncode: TEdit;
    btnEncode: TButton;
    Rectangle8: TRectangle;
    Text6: TText;
    memEncode: TMemo;
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure btnDecodeClick(Sender: TObject);
    procedure btnEncodeClick(Sender: TObject);
  private
    b64 : TBase64;
  public
    { Public declarations }
  end;

var
  Form2: TForm2;

implementation

{$R *.fmx}

procedure TForm2.btnDecodeClick(Sender: TObject);
begin
  if (trim(memeDecode.Lines.Text) = '') then
  begin
     Try memeDecode.SetFocus; Except end;
     raise Exception.Create('Informe o código base64 para prosseguir.');
  end;
  if (trim(edtDecode.Text) = '') then
  begin
     Try edtDecode.SetFocus; Except end;
     raise Exception.Create('Informe o caminho de saída para decodificação deste arquivo.');
  end;

  if (assigned(b64)) then
  begin
     if not (DirectoryExists( ExtractFilePath(edtDecode.Text) )) then
        ForceDirectories( ExtractFilePath(edtDecode.Text) );

     Try
       b64.Base64ToFile(memeDecode.Lines.Text, edtDecode.Text);
       showMessage('Arquivo '+ edtDecode.Text +' convertido com sucesso!');
     Except on E : Exception do
       raise Exception.Create('Erro na conversão: '+ E.Message);
     End;
  end;
end;

procedure TForm2.btnEncodeClick(Sender: TObject);
begin
  if (trim(memEncode.Lines.Text) <> '') then
     memEncode.Lines.Clear;

  if (trim(edtEncode.Text) = '') then
  begin
     Try edtEncode.SetFocus; Except end;
     raise Exception.Create('Selecione um arquivo para converter para base64.');
  end;

  if (assigned(b64)) then
  begin
     if not (FileExists( edtEncode.Text )) then
        raise Exception.Create('Desculpe, o arquivo selecionado não foi carregado: '+ edtEncode.Text);

     Try
       memEncode.Lines.Text := b64.FileToBase64(edtEncode.Text);
       showMessage('Arquivo '+ edtEncode.Text +' convertido com sucesso!');
     Except on E : Exception do
       raise Exception.Create('Erro na conversão: '+ E.Message);
     End;
  end;
end;

procedure TForm2.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  if (assigned(b64)) then
  begin
     b64.DisposeOf;
     b64 := nil;
  end;
end;

procedure TForm2.FormCreate(Sender: TObject);
begin
  b64 := TBase64.Create;
end;

end.
