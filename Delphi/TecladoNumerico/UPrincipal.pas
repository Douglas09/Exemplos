unit UPrincipal;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Objects,
  FMX.Effects, FMX.Layouts, Component.KeyBoard,
  FMX.Gestures, FMX.Controls.Presentation, FMX.Edit, FMX.StdCtrls;

type
  TTypeFormat = Component.KeyBoard.TTypeFormat;

  TFrmPrincipal = class(TForm)
    edtCPF: TEdit;
    VertScrollBox1: TVertScrollBox;
    Layout1: TLayout;
    Text1: TText;
    Layout2: TLayout;
    Text2: TText;
    edtInteger: TEdit;
    Layout3: TLayout;
    Text3: TText;
    edtDecimal: TEdit;
    Layout4: TLayout;
    Text4: TText;
    edtCelular: TEdit;
    Layout5: TLayout;
    Text5: TText;
    edtTelefone: TEdit;
    Layout6: TLayout;
    Text6: TText;
    edtCNPJ: TEdit;
    Layout7: TLayout;
    Layout8: TLayout;
    Text7: TText;
    edtCasasDecimais: TEdit;
    Layout9: TLayout;
    Text8: TText;
    edtData: TEdit;
    Layout10: TLayout;
    Layout11: TLayout;
    Text9: TText;
    edtCep: TEdit;
    Button1: TButton;
    Button2: TButton;
    procedure edtCPFClick(Sender: TObject);
    procedure edtDecimalClick(Sender: TObject);
    procedure edtCNPJClick(Sender: TObject);
    procedure edtTelefoneClick(Sender: TObject);
    procedure edtCelularClick(Sender: TObject);
    procedure edtDataClick(Sender: TObject);
    procedure edtIntegerClick(Sender: TObject);
    procedure edtCepClick(Sender: TObject);
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    teclado : TTeclado;
    procedure TecladoAbrir(pParent: TCommonCustomForm; pComponent: TObject; pFormat: TTypeFormat; pDescription: String; CasasDecimais : Integer = 0; After : TProc = nil; Before : TProc = nil);
  end;

var
  FrmPrincipal: TFrmPrincipal;

implementation

{$R *.fmx}

procedure TFrmPrincipal.Button1Click(Sender: TObject);
Var Valor : Double;
begin
  Valor := StrToFloatDef(edtCasasDecimais.Text, 1) + TButton(Sender).Tag;

  edtCasasDecimais.Text := FloatToStr(Valor);
end;

procedure TFrmPrincipal.edtCelularClick(Sender: TObject);
begin
  TecladoAbrir(self, Sender, TTypeFormat.ttCelular, 'MEU CELULAR');
end;

procedure TFrmPrincipal.edtCepClick(Sender: TObject);
begin
  TecladoAbrir(self, Sender, TTypeFormat.ttCEP, 'CEP');
end;

procedure TFrmPrincipal.edtCNPJClick(Sender: TObject);
begin
  TecladoAbrir(self, Sender, TTypeFormat.ttCNPJ, 'CNPJ');
end;

procedure TFrmPrincipal.edtCPFClick(Sender: TObject);
begin
  TecladoAbrir(self, Sender, TTypeFormat.ttCPF, 'CPF');
end;

procedure TFrmPrincipal.edtDataClick(Sender: TObject);
Var procedimento : TProc;
begin
  procedimento := procedure
                  begin
                     if (edtData.Text = '') then
                        showMessage('A data informada é INVÁLIDA.')
                     else
                        showMessage('A data informada é VÁLIDA.');
                  end;


  TecladoAbrir(self, Sender, TTypeFormat.ttData, 'DATA', 0, procedimento);
end;

procedure TFrmPrincipal.edtDecimalClick(Sender: TObject);
begin
  TecladoAbrir(self, Sender, TTypeFormat.ttFloat, 'NÚMERO DECIMAL', strToInt(edtCasasDecimais.Text));
end;

procedure TFrmPrincipal.edtIntegerClick(Sender: TObject);
begin
  TecladoAbrir(self, Sender, TTypeFormat.ttInteger, 'RG');
end;

procedure TFrmPrincipal.edtTelefoneClick(Sender: TObject);
begin
  TecladoAbrir(self, Sender, TTypeFormat.ttTelefone, 'MEU TELEFONE');
end;

procedure TFrmPrincipal.TecladoAbrir(pParent: TCommonCustomForm; pComponent: TObject; pFormat: TTypeFormat; pDescription: String; CasasDecimais: Integer = 0; After : TProc = nil; Before: TProc = nil);
begin
  //CONTROLE PARA ABRIR O TECLADO EM QUALQUER FORMULÁRIO A QUALQUER MOMENTO
  if (Teclado <> nil) and (Teclado.Parent <> pParent) then
  begin
     Try
       Teclado.DisposeOf;
       Teclado := nil;
     Except end;
  end;

  if not (assigned(Teclado)) then
     Teclado := TTeclado.Create(pParent);
  Teclado.SetCasasDecimais(CasasDecimais);
  Teclado.Open(pComponent, pFormat, pDescription, After, Before);
end;

end.
