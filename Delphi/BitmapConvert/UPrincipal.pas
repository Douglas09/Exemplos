unit UPrincipal;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Objects,
  FMX.Controls.Presentation, FMX.StdCtrls, FMX.Effects;

type
  TForm2 = class(TForm)
    Rectangle1: TRectangle;
    Button2: TButton;
    Button1: TButton;
    Button3: TButton;
    Button4: TButton;
    Rectangle3: TRectangle;
    ShadowEffect1: TShadowEffect;
    Text1: TText;
    ImageControl1: TImageControl;
    procedure FormCreate(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    ArquivoSalvar : String;
  end;

var
  Form2: TForm2;

implementation

{$R *.fmx}

uses BitmapConvert;

procedure TForm2.Button1Click(Sender: TObject);
begin
  if (TBitmapConvert.toJPG(ImageControl1.Bitmap, ArquivoSalvar+ 'jpg', 100)) then
     showMessage('Bitmap convertido para JPG com sucesso!')
  else
     raise Exception.Create('Não foi possível converter a imagem.');
end;

procedure TForm2.Button2Click(Sender: TObject);
begin
  if (TBitmapConvert.toBMP(ImageControl1.Bitmap, ArquivoSalvar+ 'bmp', 100)) then
     showMessage('Bitmap convertido para JPG com sucesso!')
  else
     raise Exception.Create('Não foi possível converter a imagem.');
end;

procedure TForm2.Button3Click(Sender: TObject);
begin
  if (TBitmapConvert.toPNG(ImageControl1.Bitmap, ArquivoSalvar+ 'png', 100)) then
     showMessage('Bitmap convertido para JPG com sucesso!')
  else
     raise Exception.Create('Não foi possível converter a imagem.');
end;

procedure TForm2.Button4Click(Sender: TObject);
begin
  if (TBitmapConvert.toGIF(ImageControl1.Bitmap, ArquivoSalvar+ 'gif', 100)) then
     showMessage('Bitmap convertido para JPG com sucesso!')
  else
     raise Exception.Create('Não foi possível converter a imagem.');
end;

procedure TForm2.FormCreate(Sender: TObject);
begin
  ArquivoSalvar := GetCurrentDir +PathDelim+ 'imagens' +PathDelim+ 'imagem.';
end;

end.
