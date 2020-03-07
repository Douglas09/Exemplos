unit UPrincipal;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  FMX.Controls.Presentation, FMX.StdCtrls;

type
  TForm2 = class(TForm)
    btnTop: TButton;
    btnBottom: TButton;
    procedure btnTopClick(Sender: TObject);
    procedure btnBottomClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form2: TForm2;

implementation

{$R *.fmx}

uses ToastMessage;

procedure TForm2.btnBottomClick(Sender: TObject);
begin
  TToastMessage.show('Esta é uma mensagem inferior.', 3, 40, TToastPosition.tpBottom);
end;

procedure TForm2.btnTopClick(Sender: TObject);
begin
  TToastMessage.show('Aprenda a programar e construa seu próprio futuro.');
end;

end.
