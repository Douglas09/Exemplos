unit AudioPlay;

interface

uses
  {UNIT PARA SER INSERIDA PARA USAR O GRAVADOR DE ÁUDIO}
  uAudioVisualControl,

  System.SysUtils, System.IOUtils, System.Types, System.UITypes, System.Classes,
  System.Variants, FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  FMX.Controls.Presentation, FMX.StdCtrls, FMX.Objects;

type
  TFrmPrincipal = class(TForm)
    btnGravar: TButton;
    btnOuvir2: TButton;
    btnOuvir1: TButton;
    Text1: TText;
    Text2: TText;
    Text3: TText;
    procedure FormCreate(Sender: TObject);
    procedure btnGravarClick(Sender: TObject);
    procedure btnOuvir2Click(Sender: TObject);
    procedure btnOuvir1Click(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    { Private declarations }
  public
    { Public declarations }
    arquivoDeAudio : String;

    Audio : TAudioVisualControl;
  end;

var
  FrmPrincipal: TFrmPrincipal;

implementation

{$R *.fmx}

procedure TFrmPrincipal.btnGravarClick(Sender: TObject);
begin
  Audio.SomenteLeitura := false;
  Audio.abrir( arquivoDeAudio );
end;

procedure TFrmPrincipal.btnOuvir1Click(Sender: TObject);
begin
  Audio.SomenteLeitura := true;
  Audio.abrir( arquivoDeAudio );
end;

procedure TFrmPrincipal.btnOuvir2Click(Sender: TObject);
begin
  Audio.ouvir( arquivoDeAudio );
end;

procedure TFrmPrincipal.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  if (assigned(Audio)) then
     Audio.destroy;
end;

procedure TFrmPrincipal.FormCreate(Sender: TObject);
begin
  Audio := TAudioVisualControl.create(self);

{$IFDEF ANDROID}
     arquivoDeAudio := System.IOUtils.TPath.GetDocumentsPath + PathDelim+ 'audio.caf';
{$ENDIF} {$IFDEF IOS}
     arquivoDeAudio := ExtractFilePath( ParamStr(0) ) + 'audio.caf';
{$ENDIF} {$IFDEF MSWINDOWS}
     arquivoDeAudio := ExtractFilePath( ParamStr(0) ) + 'audio.wav';
{$ENDIF}
end;

end.
