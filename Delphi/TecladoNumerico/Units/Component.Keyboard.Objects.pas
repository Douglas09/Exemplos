unit Component.Keyboard.Objects;

interface

uses System.UITypes, System.Classes, System.Types, System.SysUtils,
     FMX.Objects, FMX.Forms, FMX.Types, FMX.Effects, FMX.Layouts, FMX.Gestures;

Type
  TTecFormParent = class
  private
    FOwner: TCommonCustomForm;
    procedure SetOwner(const Value: TCommonCustomForm);
  protected
    FormKeyDown : TKeyEvent;
    FormKeyUp   : TKeyEvent;
    FormResize  : TNotifyEvent;

    procedure KeyBoardKeyUp(Sender: TObject; var Key: Word; var KeyChar: Char; Shift: TShiftState);
    procedure KeyBoardKeyDown(Sender: TObject; var Key: Word; var KeyChar: Char; Shift: TShiftState);
  public
    procedure Active(Activate : Boolean);
  published
    property Owner : TCommonCustomForm read FOwner write SetOwner;
  end;

  TTecTypeFormat = (ttFloat, ttInteger, ttCPF, ttCNPJ, ttCelular, ttTelefone, ttData, ttCEP);

  TTecLayout = class(TObject)
  private //COMPONENTS
    Fundo  : TRectangle;
      Bottom  : TRectangle;
        bFundo : TLayout;
        bCancel : TRectangle;
          bcIcon : TRectangle;
          bcDesc : TText;
        bOk : TRectangle;
          boIcon : TRectangle;
          boDesc : TText;
      Top     : TRectangle;
        tValue : TText;
        tFundoDescricao : TRectangle;
          tfSombra : TShadowEffect;
          tfDescricao : TText;
      Client  : TLayout;
        cLay1 : TLayout;
          cBtnCom : TRectangle;
            clbCom : TText;
          cBtnDel : TRectangle;
            clbIco : TRectangle;
          cBtn0 : TRectangle;
            clb0 : TText;
        cLay2 : TLayout;
          cBtn1 : TRectangle;
            clb1 : TText;
          cBtn2 : TRectangle;
            clb2 : TText;
          cBtn3 : TRectangle;
            clb3 : TText;
        cLay3 : TLayout;
          cBtn4 : TRectangle;
            clb4 : TText;
          cBtn5 : TRectangle;
            clb5 : TText;
          cBtn6 : TRectangle;
            clb6 : TText;
        cLay4 : TLayout;
          cBtn7 : TRectangle;
            clb7 : TText;
          cBtn8 : TRectangle;
            clb8 : TText;
          cBtn9 : TRectangle;
            clb9 : TText;
      Valid   : TRectangle;
        vSombra : TShadowEffect;
        vDescricao : TText;
    FCasasDecimais: Integer;
    FFormat: TTecTypeFormat;
    procedure SetCasasDecimais(const Value: Integer);
    procedure SetFormat(const Value: TTecTypeFormat);
    procedure SetDisplay(const Value : String);
    procedure SetValid(const Value : Boolean);
  protected //FUNCTIONS
    Gesture : TGestureManager;

    procedure CriarEstrutura;
    procedure CriarGenerics;
    procedure CriarValidacao;
    procedure CriarBotoesFundo;
    procedure CriarBotoesDescricao;

    procedure KeyBoardClick(Sender : TObject);
    
    procedure BtnOkClick(Sender : TObject);
    procedure BtnCancelClick(Sender : TObject);

    procedure BackSpaceGesture(Sender: TObject;const EventInfo: TGestureEventInfo; var Handled: Boolean);

    procedure BotoesEnter(Sender : TObject);
    procedure BotoesLeave(Sender : TObject);

    procedure MouseEnter(Sender : TObject);
    procedure MouseLeave(Sender : TObject);

    procedure LayoutConfigure;
  public
    Owner : TTecFormParent;
    Component : TObject;
    ComponentValue : String;
    AfterExecute : TProc;

    procedure Open(pComponent : TObject; pDescription : String = ''; pAfter : TProc = nil);
    procedure Close;
    constructor Create(pOwner: TCommonCustomForm);
    procedure DisposeOf;

  published
    property CasasDecimais : Integer read FCasasDecimais write SetCasasDecimais;
    property Format : TTecTypeFormat read FFormat write SetFormat;
  end;

implementation

uses Component.KeyBoard.Functions,
     FMX.Graphics, FMX.Edit, FMX.StdCtrls;

{ TTecLayout }

procedure TTecLayout.BackSpaceGesture(Sender: TObject; const EventInfo: TGestureEventInfo; var Handled: Boolean);
begin
  if (EventInfo.GestureID = igiLongTap) then
     SetDisplay( '' );
end;

procedure TTecLayout.BotoesEnter(Sender: TObject);
begin
  if (Sender is TRectangle) then
     TRectangle(Sender).Opacity := 0.5;
end;

procedure TTecLayout.BotoesLeave(Sender: TObject);
begin
  if (Sender is TRectangle) then
     TRectangle(Sender).Opacity := 1;
end;

procedure TTecLayout.BtnCancelClick(Sender: TObject);
begin
  Close;
end;

procedure TTecLayout.BtnOkClick(Sender: TObject);
begin
  Try
    if (Component is TLabel) then
       TLabel(Component).Text  := tValue.Text
    else if (Component is TText) then
       TText(Component).Text   := tValue.Text
    else if (Component is TButton) then
       TButton(Component).Text := tValue.Text
    else if (Component is TEdit) then
       TEdit(Component).Text   := tValue.Text;
  Finally
    Close;
  End;
end;

procedure TTecLayout.Close;
begin
  Owner.Active(false);
  Fundo.Visible := false;
  Try Fundo.SendToBack; Except end;
  if (assigned(AfterExecute)) then
     AfterExecute;
end;

constructor TTecLayout.Create(pOwner: TCommonCustomForm);
begin
  CasasDecimais := 2;

  Owner       := TTecFormParent.Create;
  if (assigned(pOwner)) then
     Owner.Owner := pOwner
  else
     Owner.Owner := Application.MainForm;

  CriarEstrutura;
  CriarBotoesFundo;
  CriarBotoesDescricao;
  CriarValidacao;
end;

procedure TTecLayout.CriarBotoesFundo;
begin
   bCancel                                 := TRectangle.Create(bFundo);
   bCancel.Parent                          := bFundo;
   bCancel.XRadius                         := 4;
   bCancel.YRadius                         := 4;
   bCancel.HitTest                         := true;
   bCancel.Locked                          := false;
   bCancel.Fill.Color                      := TAlphaColorRec.Red;
   bCancel.Width                           := 130;
   bCancel.Margins.Top                     := 4;
   bCancel.Margins.Bottom                  := 4;
   bCancel.Align                           := TAlignLayout.Left;
   bCancel.OnMouseEnter                    := BotoesEnter;
   bCancel.OnMouseLeave                    := BotoesLeave;
   bCancel.OnClick                         := BtnCancelClick;

   bcIcon                                  := TRectangle.Create(bCancel);
   bcIcon.Parent                           := bCancel;
   bcIcon.HitTest                          := false;
   bcIcon.Locked                           := true;
   bcIcon.Fill.Kind                        := TBrushKind.Bitmap;
   bcIcon.Fill.Bitmap.WrapMode             := TWrapMode.TileStretch;
   bcIcon.Stroke.Kind                      := TBrushKind.None;
   bcIcon.Align                            := TAlignLayout.Left;
   bcIcon.Width                            := 36;
   bcIcon.Margins.Top                      := 3;
   bcIcon.Margins.Bottom                   := 3;
   bcIcon.Margins.Left                     := 3;
   bcIcon.Fill.Bitmap.Bitmap               := IcoCancel;

   bcDesc                                  := TText.Create(bCancel);
   bcDesc.Parent                           := bCancel;
   bcDesc.HitTest                          := false;
   bcDesc.Locked                           := true;
   bcDesc.Align                            := TAlignLayout.Client;
   bcDesc.TextSettings.FontColor           := TAlphaColorRec.White;
   bcDesc.TextSettings.Font.Size           := 15;
   bcDesc.TextSettings.Font.Style          := [TFontStyle.fsBold];
   bcDesc.Text                             := 'CANCELAR';

   bOk                                     := TRectangle.Create(bFundo);
   bOk.Parent                              := bFundo;
   bOk.XRadius                             := 4;
   bOk.YRadius                             := 4;
   bOk.HitTest                             := true;
   bOk.Locked                              := false;
   bOk.Fill.Color                          := TAlphaColorRec.Seagreen;
   bOk.Width                               := 150;
   bOk.Margins.Top                         := 1;
   bOk.Margins.Bottom                      := 1;
   bOk.Align                               := TAlignLayout.Right;
   bOk.OnMouseEnter                        := BotoesEnter;
   bOk.OnMouseLeave                        := BotoesLeave;
   bOk.OnClick                             := BtnOkClick;

   boIcon                                  := TRectangle.Create(bOk);
   boIcon.Parent                           := bOk;
   boIcon.HitTest                          := false;
   boIcon.Locked                           := true;
   boIcon.Fill.Kind                        := TBrushKind.Bitmap;
   boIcon.Fill.Bitmap.WrapMode             := TWrapMode.TileStretch;
   boIcon.Stroke.Kind                      := TBrushKind.None;
   boIcon.Align                            := TAlignLayout.Left;
   boIcon.Width                            := 36;
   boIcon.Margins.Top                      := 6;
   boIcon.Margins.Bottom                   := 6;
   boIcon.Margins.Left                     := 3;
   boIcon.Fill.Bitmap.Bitmap               := IcoOK;

   boDesc                                  := TText.Create(bOk);
   boDesc.Parent                           := bOk;
   boDesc.HitTest                          := false;
   boDesc.Locked                           := true;
   boDesc.Align                            := TAlignLayout.Client;
   boDesc.TextSettings.FontColor           := TAlphaColorRec.White;
   boDesc.TextSettings.Font.Size           := 15;
   boDesc.TextSettings.Font.Style          := [TFontStyle.fsBold];
   boDesc.Text                             := 'CONFIRMAR';

   cBtnCom                                 := TRectangle.Create(cLay1);
   cBtnCom.Parent                          := cLay1;
   cBtnCom.XRadius                         := 4;
   cBtnCom.YRadius                         := 4;
   cBtnCom.Width                           := Trunc(cLay1.Width / 3) - 2;
   cBtnCom.Margins.Left                    := 1;
   cBtnCom.Margins.Right                   := 1;
   cBtnCom.Align                           := TAlignLayout.MostLeft;
   cBtnCom.Stroke.Kind                     := TBrushKind.None;
   cBtnCom.Fill.Color                      := TAlphaColorRec.Null;
   cBtnCom.OnMouseEnter                    := MouseEnter;
   cBtnCom.OnMouseLeave                    := MouseLeave;
   cBtnCom.OnClick                         := KeyBoardClick;

   cBtnDel                                 := TRectangle.Create(cLay1);
   cBtnDel.Parent                          := cLay1;
   cBtnDel.XRadius                         := 4;
   cBtnDel.YRadius                         := 4;
   cBtnDel.TagString                       := '-';
   cBtnDel.Width                           := Trunc(cLay1.Width / 3) - 2;
   cBtnDel.Margins.Left                    := 1;
   cBtnDel.Margins.Right                   := 1;
   cBtnDel.Align                           := TAlignLayout.Right;
   cBtnDel.Stroke.Kind                     := TBrushKind.None;
   cBtnDel.Fill.Color                      := TAlphaColorRec.Null;
   cBtnDel.Touch.GestureManager            := Gesture;
   cBtnDel.Touch.InteractiveGestures       := [TInteractiveGesture.LongTap];
   cBtnDel.OnMouseEnter                    := MouseEnter;
   cBtnDel.OnMouseLeave                    := MouseLeave;
   cBtnDel.OnGesture                       := BackSpaceGesture;
   cBtnDel.OnClick                         := KeyBoardClick;

   cBtn0                                   := TRectangle.Create(cLay1);
   cBtn0.Parent                            := cLay1;
   cBtn0.XRadius                           := 4;
   cBtn0.YRadius                           := 4;
   cBtn0.Width                             := Trunc(cLay1.Width / 3) - 2;
   cBtn0.Margins.Left                      := 1;
   cBtn0.Margins.Right                     := 1;
   cBtn0.Align                             := TAlignLayout.Left;
   cBtn0.Stroke.Kind                       := TBrushKind.None;
   cBtn0.Fill.Color                        := TAlphaColorRec.Null;
   cBtn0.OnMouseEnter                      := MouseEnter;
   cBtn0.OnMouseLeave                      := MouseLeave;
   cBtn0.OnClick                           := KeyBoardClick;

   cBtn1                                   := TRectangle.Create(cLay2);
   cBtn1.Parent                            := cLay2;
   cBtn1.XRadius                           := 4;
   cBtn1.YRadius                           := 4;
   cBtn1.Width                             := Trunc(cLay2.Width / 3) - 2;
   cBtn1.Margins.Left                      := 1;
   cBtn1.Margins.Right                     := 1;
   cBtn1.Align                             := TAlignLayout.MostLeft;
   cBtn1.Stroke.Kind                       := TBrushKind.None;
   cBtn1.Fill.Color                        := TAlphaColorRec.Null;
   cBtn1.OnMouseEnter                      := MouseEnter;
   cBtn1.OnMouseLeave                      := MouseLeave;
   cBtn1.OnClick                           := KeyBoardClick;

   cBtn2                                   := TRectangle.Create(cLay2);
   cBtn2.Parent                            := cLay2;
   cBtn2.XRadius                           := 4;
   cBtn2.YRadius                           := 4;
   cBtn2.Width                             := Trunc(cLay2.Width / 3) - 2;
   cBtn2.Margins.Left                      := 1;
   cBtn2.Margins.Right                     := 1;
   cBtn2.Align                             := TAlignLayout.Left;
   cBtn2.Stroke.Kind                       := TBrushKind.None;
   cBtn2.Fill.Color                        := TAlphaColorRec.Null;
   cBtn2.OnMouseEnter                      := MouseEnter;
   cBtn2.OnMouseLeave                      := MouseLeave;
   cBtn2.OnClick                           := KeyBoardClick;
   
   cBtn3                                   := TRectangle.Create(cLay2);
   cBtn3.Parent                            := cLay2;
   cBtn3.XRadius                           := 4;
   cBtn3.YRadius                           := 4;
   cBtn3.Width                             := Trunc(cLay2.Width / 3) - 2;
   cBtn3.Margins.Left                      := 1;
   cBtn3.Margins.Right                     := 1;
   cBtn3.Align                             := TAlignLayout.Right;
   cBtn3.Stroke.Kind                       := TBrushKind.None;
   cBtn3.Fill.Color                        := TAlphaColorRec.Null;
   cBtn3.OnMouseEnter                      := MouseEnter;
   cBtn3.OnMouseLeave                      := MouseLeave;
   cBtn3.OnClick                           := KeyBoardClick;

   cBtn4                                   := TRectangle.Create(cLay3);
   cBtn4.Parent                            := cLay3;
   cBtn4.XRadius                           := 4;
   cBtn4.YRadius                           := 4;
   cBtn4.Width                             := Trunc(cLay3.Width / 3) - 2;
   cBtn4.Margins.Left                      := 1;
   cBtn4.Margins.Right                     := 1;
   cBtn4.Align                             := TAlignLayout.MostLeft;
   cBtn4.Stroke.Kind                       := TBrushKind.None;
   cBtn4.Fill.Color                        := TAlphaColorRec.Null;
   cBtn4.OnMouseEnter                      := MouseEnter;
   cBtn4.OnMouseLeave                      := MouseLeave;
   cBtn4.OnClick                           := KeyBoardClick;

   cBtn5                                   := TRectangle.Create(cLay3);
   cBtn5.Parent                            := cLay3;
   cBtn5.XRadius                           := 4;
   cBtn5.YRadius                           := 4;
   cBtn5.Width                             := Trunc(cLay3.Width / 3) - 2;
   cBtn5.Margins.Left                      := 1;
   cBtn5.Margins.Right                     := 1;
   cBtn5.Align                             := TAlignLayout.Left;
   cBtn5.Stroke.Kind                       := TBrushKind.None;
   cBtn5.Fill.Color                        := TAlphaColorRec.Null;
   cBtn5.OnMouseEnter                      := MouseEnter;
   cBtn5.OnMouseLeave                      := MouseLeave;
   cBtn5.OnClick                           := KeyBoardClick;

   cBtn6                                   := TRectangle.Create(cLay3);
   cBtn6.Parent                            := cLay3;
   cBtn6.XRadius                           := 4;
   cBtn6.YRadius                           := 4;
   cBtn6.Width                             := Trunc(cLay3.Width / 3) - 2;
   cBtn6.Margins.Left                      := 1;
   cBtn6.Margins.Right                     := 1;
   cBtn6.Align                             := TAlignLayout.Right;
   cBtn6.Stroke.Kind                       := TBrushKind.None;
   cBtn6.Fill.Color                        := TAlphaColorRec.Null;
   cBtn6.OnMouseEnter                      := MouseEnter;
   cBtn6.OnMouseLeave                      := MouseLeave;
   cBtn6.OnClick                           := KeyBoardClick;

   cBtn7                                   := TRectangle.Create(cLay4);
   cBtn7.Parent                            := cLay4;
   cBtn7.XRadius                           := 4;
   cBtn7.YRadius                           := 4;
   cBtn7.Width                             := Trunc(cLay4.Width / 3) - 2;
   cBtn7.Margins.Left                      := 1;
   cBtn7.Margins.Right                     := 1;
   cBtn7.Align                             := TAlignLayout.mostLeft;
   cBtn7.Stroke.Kind                       := TBrushKind.None;
   cBtn7.Fill.Color                        := TAlphaColorRec.Null;
   cBtn7.OnMouseEnter                      := MouseEnter;
   cBtn7.OnMouseLeave                      := MouseLeave;
   cBtn7.OnClick                           := KeyBoardClick;

   cBtn8                                   := TRectangle.Create(cLay4);
   cBtn8.Parent                            := cLay4;
   cBtn8.XRadius                           := 4;
   cBtn8.YRadius                           := 4;
   cBtn8.Width                             := Trunc(cLay4.Width / 3) - 2;
   cBtn8.Margins.Left                      := 1;
   cBtn8.Margins.Right                     := 1;
   cBtn8.Align                             := TAlignLayout.Left;
   cBtn8.Stroke.Kind                       := TBrushKind.None;
   cBtn8.Fill.Color                        := TAlphaColorRec.Null;
   cBtn8.OnMouseEnter                      := MouseEnter;
   cBtn8.OnMouseLeave                      := MouseLeave;
   cBtn8.OnClick                           := KeyBoardClick;

   cBtn9                                   := TRectangle.Create(cLay4);
   cBtn9.Parent                            := cLay4;
   cBtn9.XRadius                           := 4;
   cBtn9.YRadius                           := 4;
   cBtn9.Width                             := Trunc(cLay4.Width / 3) - 2;
   cBtn9.Margins.Left                      := 1;
   cBtn9.Margins.Right                     := 1;
   cBtn9.Align                             := TAlignLayout.Client;
   cBtn9.Stroke.Kind                       := TBrushKind.None;
   cBtn9.Fill.Color                        := TAlphaColorRec.Null;
   cBtn9.OnMouseEnter                      := MouseEnter;
   cBtn9.OnMouseLeave                      := MouseLeave;
   cBtn9.OnClick                           := KeyBoardClick;
end;

procedure TTecLayout.CriarEstrutura;
begin
   //NÍVEL 01
   Fundo                                   := TRectangle.Create(Owner.Owner);
   Fundo.Parent                            := Owner.Owner;
   Fundo.Visible                           := false;
   Fundo.HitTest                           := true;
   Fundo.Locked                            := false;
   Fundo.Fill.Color                        := TAlphaColorRec.Black;
   Fundo.Stroke.Kind                       := TBrushKind.None;
   Fundo.Position.X                        := 0;
   Fundo.Position.Y                        := 0;
   Fundo.Width                             := Owner.Owner.ClientWidth;
   Fundo.Height                            := Owner.Owner.ClientHeight;

   //NÍVEL 02
   Top                                     := TRectangle.Create(Fundo);
   Top.Parent                              := Fundo;
   Top.Align                               := TAlignLayout.Top;
   Top.Stroke.Kind                         := TBrushKind.None;
   Top.Fill.Color                          := TAlphaColorRec.White;
   Top.Height                              := 100;

   Bottom                                  := TRectangle.Create(Fundo);
   Bottom.Parent                           := Fundo;
   Bottom.HitTest                          := true;
   Bottom.Locked                           := false;
   Bottom.Align                            := TAlignLayout.Bottom;
   Bottom.Height                           := 50;
   Bottom.Stroke.Kind                      := TBrushKind.None;
   Bottom.Fill.Color                       := TAlphaColorRec.Black;

   bFundo                                  := TLayout.Create(Bottom);
   bFundo.Parent                           := Bottom;
   bFundo.HitTest                          := true;
   bFundo.Locked                           := false;
   bFundo.Align                            := TAlignLayout.Center;
   bFundo.Height                           := Bottom.Height;
   bFundo.Width                            := 315;

   Client                                  := TLayout.Create(Fundo);
   Client.Parent                           := Fundo;
   Client.HitTest                          := true;
   Client.Locked                           := false;
   Client.Margins.Left                     := 20;
   Client.Margins.Right                    := 20;
   Client.Margins.Top                      := 20;
   Client.Margins.Bottom                   := 20;
   Client.Align                            := TAlignLayout.Client;
   
   tFundoDescricao                         := TRectangle.Create(Top);
   tFundoDescricao.XRadius                 := 4;
   tFundoDescricao.YRadius                 := 4;
   tFundoDescricao.Parent                  := Top;
   tFundoDescricao.Height                  := 24;
   tFundoDescricao.Align                   := TAlignLayout.Top;
   tFundoDescricao.Margins.Left            := 60;
   tFundoDescricao.Margins.Right           := 60;
   tFundoDescricao.Margins.Top             := 4;
   tFundoDescricao.Stroke.Kind             := TBrushKind.None;
   tFundoDescricao.Fill.Color              := TAlphaColorRec.Red;

   tfSombra                                := TShadowEffect.Create(tFundoDescricao);
   tfSombra.Parent                         := tFundoDescricao;
   tfSombra.Distance                       := 1;
   tfSombra.Opacity                        := 0.9;
   tfSombra.Softness                       := 0.2;

   tfDescricao                             := TText.Create(tFundoDescricao);
   tfDescricao.Parent                      := tFundoDescricao;
   tfDescricao.Align                       := TAlignLayout.Client;
   tfDescricao.TextSettings.FontColor      := TAlphaColorRec.white;
   tfDescricao.TextSettings.Font.Size      := 17;
   tfDescricao.TextSettings.Font.Style     := [TFontStyle.fsBold];

   tValue                                  := TText.Create(Top);
   tValue.Parent                           := Top;
   tValue.Align                            := TAlignLayout.Client;
   tValue.Margins.Top                      := 50;
   tValue.Margins.Left                     := 4;
   tValue.Margins.Top                      := 4;
   tValue.Margins.Bottom                   := 4;
   tValue.TextSettings.Font.Size           := 32;
   tValue.TextSettings.FontColor           := TAlphaColorRec.Red;

   cLay1                                   := TLayout.Create(Client);
   cLay1.Parent                            := Client;
   cLay1.Height                            := Trunc(Client.Height / 4) - 2;
   cLay1.Margins.Top                       := 1;
   cLay1.Margins.Left                      := 1;
   cLay1.Margins.Right                     := 1;
   cLay1.Margins.Bottom                    := 1;
   cLay1.Align                             := TAlignLayout.Bottom;

   cLay2                                   := TLayout.Create(Client);
   cLay2.Parent                            := Client;
   cLay2.Height                            := Trunc(Client.Height / 4) - 2;
   cLay2.Margins.Top                       := 1;
   cLay2.Margins.Left                      := 1;
   cLay2.Margins.Right                     := 1;
   cLay2.Margins.Bottom                    := 1;
   cLay2.Align                             := TAlignLayout.Bottom;

   cLay3                                   := TLayout.Create(Client);
   cLay3.Parent                            := Client;
   cLay3.Height                            := Trunc(Client.Height / 4) - 2;
   cLay3.Margins.Top                       := 1;
   cLay3.Margins.Left                      := 1;
   cLay3.Margins.Right                     := 1;
   cLay3.Margins.Bottom                    := 1;
   cLay3.Align                             := TAlignLayout.Bottom;

   cLay4                                   := TLayout.Create(Client);
   cLay4.Parent                            := Client;
   cLay4.Height                            := Trunc(Client.Height / 4) - 2;
   cLay4.Margins.Top                       := 1;
   cLay4.Margins.Left                      := 1;
   cLay4.Margins.Right                     := 1;
   cLay4.Margins.Bottom                    := 1;
   cLay4.Align                             := TAlignLayout.Bottom;
end;

procedure TTecLayout.CriarGenerics;
begin
  Gesture                                  := TGestureManager.Create(Fundo);
  Gesture.Sensitivity                      := 80;
end;

procedure TTecLayout.CriarValidacao;
begin
   Valid                                   := TRectangle.Create(Fundo);
   Valid.Parent                            := Fundo;
   Valid.HitTest                           := false;
   Valid.Locked                            := true;
   Valid.XRadius                           := 2;
   Valid.YRadius                           := 2;
   Valid.Height                            := 18;
   Valid.Width                             := 60;
   Valid.Position.X                        := 4;
   Valid.Position.Y                        := Trunc((Top.Position.Y + Top.Height) - (Valid.Height / 2));
   Valid.Stroke.Kind                       := TBrushKind.None;
   Valid.Fill.Color                        := TAlphaColorRec.Red;
   Try Valid.BringToFront; Except end;

   vSombra                                 := TShadowEffect.Create(Valid);
   vSombra.Parent                          := Valid;
   vSombra.Distance                        := 1;
   vSombra.Opacity                         := 0.9;
   vSombra.Softness                        := 0.2;

   vDescricao                              := TText.Create(Valid);
   vDescricao.Parent                       := Valid;
   vDescricao.Align                        := TAlignLayout.Client;
   vDescricao.TextSettings.FontColor       := TAlphaColorRec.white;
   vDescricao.TextSettings.Font.Size       := 10;
   vDescricao.TextSettings.Font.Style      := [TFontStyle.fsBold];
end;

procedure TTecLayout.DisposeOf;
begin
  if (Owner <> nil) then
  begin
     Try
       Owner.Owner := nil;
       Owner.DisposeOf;
       Owner := nil;
     Except End;
  end;

  if (assigned(Fundo)) then //FORÇA A REMOÇÃO DA TELA O TECLADO
  begin
     Fundo.Visible := false;
     Fundo.Parent  := nil;
  end;

  if (assigned(vDescricao)) then
  begin
     vDescricao.DisposeOf;
     vDescricao := nil;
  end;
  if (assigned(vSombra)) then
  begin
     vSombra.DisposeOf;
     vSombra := nil;
  end;

  if (assigned(clbCom)) then
  begin
     clbCom.DisposeOf;
     clbCom := nil;
  end;
  if (assigned(clbIco)) then
  begin
     clbIco.DisposeOf;
     clbIco := nil;
  end;
  if (assigned(clb0)) then
  begin
     clb0.DisposeOf;
     clb0 := nil;
  end;
  if (assigned(clb1)) then
  begin
     clb1.DisposeOf;
     clb1 := nil;
  end;
  if (assigned(clb2)) then
  begin
     clb2.DisposeOf;
     clb2 := nil;
  end;
  if (assigned(clb3)) then
  begin
     clb3.DisposeOf;
     clb3 := nil;
  end;
  if (assigned(clb4)) then
  begin
     clb4.DisposeOf;
     clb4 := nil;
  end;
  if (assigned(clb5)) then
  begin
     clb5.DisposeOf;
     clb5 := nil;
  end;
  if (assigned(clb6)) then
  begin
     clb6.DisposeOf;
     clb6 := nil;
  end;
  if (assigned(clb7)) then
  begin
     clb7.DisposeOf;
     clb7 := nil;
  end;
  if (assigned(clb8)) then
  begin
     clb8.DisposeOf;
     clb8 := nil;
  end;
  if (assigned(clb9)) then
  begin
     clb9.DisposeOf;
     clb9 := nil;
  end;
  if (assigned(cBtnCom)) then
  begin
     cBtnCom.DisposeOf;
     cBtnCom := nil;
  end;
  if (assigned(cBtnDel)) then
  begin
     cBtnDel.DisposeOf;
     cBtnDel := nil;
  end;
  if (assigned(cBtn0)) then
  begin
     cBtn0.DisposeOf;
     cBtn0 := nil;
  end;
  if (assigned(cBtn1)) then
  begin
     cBtn1.DisposeOf;
     cBtn1 := nil;
  end;
  if (assigned(cBtn2)) then
  begin
     cBtn2.DisposeOf;
     cBtn2 := nil;
  end;
  if (assigned(cBtn3)) then
  begin
     cBtn3.DisposeOf;
     cBtn3 := nil;
  end;
  if (assigned(cBtn4)) then
  begin
     cBtn4.DisposeOf;
     cBtn4 := nil;
  end;
  if (assigned(cBtn5)) then
  begin
     cBtn5.DisposeOf;
     cBtn5 := nil;
  end;
  if (assigned(cBtn6)) then
  begin
     cBtn6.DisposeOf;
     cBtn6 := nil;
  end;
  if (assigned(cBtn7)) then
  begin
     cBtn7.DisposeOf;
     cBtn7 := nil;
  end;
  if (assigned(cBtn8)) then
  begin
     cBtn8.DisposeOf;
     cBtn8 := nil;
  end;
  if (assigned(cBtn9)) then
  begin
     cBtn9.DisposeOf;
     cBtn9 := nil;
  end;

  if (assigned(cLay1)) then
  begin
     cLay1.DisposeOf;
     cLay1 := nil;
  end;
  if (assigned(cLay2)) then
  begin
     cLay2.DisposeOf;
     cLay2 := nil;
  end;
  if (assigned(cLay3)) then
  begin
     cLay3.DisposeOf;
     cLay3 := nil;
  end;
  if (assigned(cLay4)) then
  begin
     cLay4.DisposeOf;
     cLay4 := nil;
  end;

  if (assigned(tfDescricao)) then
  begin
     tfDescricao.DisposeOf;
     tfDescricao := nil;
  end;
  if (assigned(tfSombra)) then
  begin
     tfSombra.DisposeOf;
     tfSombra := nil;
  end;
  if (assigned(tValue)) then
  begin
     tValue.DisposeOf;
     tValue := nil;
  end;
  if (assigned(tFundoDescricao)) then
  begin
     tFundoDescricao.DisposeOf;
     tFundoDescricao := nil;
  end;

  if (assigned(bcDesc)) then
  begin
     bcDesc.DisposeOf;
     bcDesc := nil;
  end;

  if (assigned(bcIcon)) then
  begin
     bcIcon.DisposeOf;
     bcIcon := nil;
  end;

  if (assigned(bCancel)) then
  begin
     bCancel.DisposeOf;
     bCancel := nil;
  end;

  if (assigned(boDesc)) then
  begin
     boDesc.DisposeOf;
     boDesc := nil;
  end;
  if (assigned(boIcon)) then
  begin
     boIcon.DisposeOf;
     boIcon := nil;
  end;

  if (assigned(bOK)) then
  begin
     bOK.DisposeOf;
     bOK := nil;
  end;

  if (assigned(bFundo)) then
  begin
     bFundo.DisposeOf;
     bFundo := nil;
  end;

  if (assigned(Gesture)) then
  begin
     Gesture.DisposeOf;
     Gesture := nil;
  end;

  //PRINCIPAIS
  if (assigned(Valid)) then
  begin
     Valid.DisposeOf;
     Valid := nil;
  end;
  if (assigned(Top)) then
  begin
     Top.DisposeOf;
     Top := nil;
  end;
  if (assigned(Client)) then
  begin
     Client.DisposeOf;
     Client := nil;
  end;
  if (assigned(Bottom)) then
  begin
     Bottom.DisposeOf;
     Bottom := nil;
  end;

  if (assigned(Fundo)) then
  begin
     Fundo.DisposeOf;
     Fundo := nil;
  end;
end;

procedure TTecLayout.KeyBoardClick(Sender: TObject);
Var Temp : String;
begin
  if (Sender is TRectangle) then
  begin
     if (TRectangle(Sender).TagString = '-') then //DIMINUI 1 CARACTERE
     begin
        Temp := SomenteNumeros(tValue.Text);
        SetDisplay( Copy(Temp, 1, length(Temp) - 1) );
     end else //AUMENTA 1 CARACTERE
        SetDisplay( SomenteNumeros(tValue.Text) + TRectangle(Sender).TagString );
  end;
end;

procedure TTecLayout.LayoutConfigure;
Var i : Integer;
    ValorInicial, ValorInteiro, ValorDecimal, Temp : String;
begin
  Try
     if (Format = ttCEP) then
     begin
        clbCom.Text       := '000';
        tfDescricao.Text  := 'CEP';
     end else if (Format = ttCNPJ) then
     begin
        clbCom.Text       := '000';
        tfDescricao.Text  := 'CNPJ';
     end else if (Format = ttCPF) then
     begin
        clbCom.Text       := '00';
        tfDescricao.Text  := 'CPF';
     end else if (Format = ttInteger) then
     begin
        clbCom.Text       := '00';
        tfDescricao.Text  := 'VALOR INTEIRO';
     end else if (Format = ttCelular) then
     begin
        clbCom.Text       := '00';
        tfDescricao.Text  := 'CELULAR';
     end else if (Format = ttTelefone) then
     begin
        clbCom.Text       := '00';
        tfDescricao.Text  := 'TELEFONE';
     end else if (Format = ttData) then
     begin
        clbCom.Text       := '00';
        tfDescricao.Text  := 'DATA';
     end else if (Format = ttFloat) then
     begin
        tfDescricao.Text := 'VALOR REAL';
        clbCom.Text := '';
        i := 0;
        while (i < CasasDecimais) do
        begin
          clbCom.Text := clbCom.Text + '0';
          i := i + 1;
        end;

        if (CasasDecimais > 5) then
           clbCom.TextSettings.Font.Size := 32
        else
           clbCom.TextSettings.Font.Size := 42;
     end;
     cBtnCom.TagString := clbCom.Text;

     if (Component is TLabel) then
        ValorInicial := TLabel(Component).Text
     else if (Component is TText) then
        ValorInicial := TText(Component).Text
     else if (Component is TButton) then
        ValorInicial := TButton(Component).Text
     else if (Component is TEdit) then
        ValorInicial := TEdit(Component).Text;

     if (Format = ttFloat) and (Trim(ValorInicial) <> '') then //AJUSTA O VALOR RECEBIDO
     begin
        if (pos(FormatSettings.DecimalSeparator, ValorInicial) <> 0) then
        begin
           Try
             ValorInteiro := Copy(ValorInicial, 1, pos(FormatSettings.DecimalSeparator, ValorInicial) - 1);
             ValorDecimal := Copy(ValorInicial, pos(FormatSettings.DecimalSeparator, ValorInicial) + 1, length(ValorInicial));
             ValorDecimal := PreencheValor(ValorDecimal, CasasDecimais);
           Finally
             ValorInicial := ValorInteiro +FormatSettings.DecimalSeparator+ ValorDecimal;
           End;
        end else
           ValorInicial := ValorInicial +FormatSettings.DecimalSeparator+ PreencheValor('', CasasDecimais);
     end;

     SetDisplay(ValorInicial);
  Finally
     Fundo.Visible     := true;
     Try Fundo.BringToFront; Except end;
  End;
end;

procedure TTecLayout.CriarBotoesDescricao;
begin
   clbCom                                := TText.Create(cBtnCom);
   clbCom.Parent                         := cBtnCom;
   clbCom.Align                          := TAlignLayout.Client;
   clbCom.HitTest                        := false;
   clbCom.Locked                         := true;
   clbCom.TextSettings.FontColor         := TAlphaColorRec.White;
   clbCom.TextSettings.Font.Size         := 42;
   clbCom.TextSettings.Font.Style        := [TFontStyle.fsBold];
   clbCom.Text                           := '00';
   cBtnCom.TagString                     := clbCom.Text;

   clbIco                                := TRectangle.Create(cBtnDel);
   clbIco.Parent                         := cBtnDel;
   clbIco.HitTest                        := false;
   clbIco.Locked                         := true;
   clbIco.Stroke.Kind                    := TBrushKind.None;
   clbIco.Fill.Kind                      := TBrushKind.Bitmap;
   clbIco.Fill.Bitmap.WrapMode           := TWrapMode.TileStretch;
   clbIco.Fill.Bitmap.Bitmap             := IcoBackSpace;
   clbIco.Height                         := 38;
   clbIco.Width                          := 38;
   clbIco.Align                          := TAlignLayout.Center;

   clb0                                  := TText.Create(cBtn0);
   clb0.Parent                           := cBtn0;
   clb0.Align                            := TAlignLayout.Client;
   clb0.HitTest                          := false;
   clb0.Locked                           := true;
   clb0.TextSettings.FontColor           := TAlphaColorRec.White;
   clb0.TextSettings.Font.Size           := 48;
   clb0.TextSettings.Font.Style          := [TFontStyle.fsBold];
   clb0.Text                             := '0';
   cBtn0.TagString                       := clb0.Text;

   clb1                                  := TText.Create(cBtn1);
   clb1.Parent                           := cBtn1;
   clb1.Align                            := TAlignLayout.Client;
   clb1.HitTest                          := false;
   clb1.Locked                           := true;
   clb1.TextSettings.FontColor           := TAlphaColorRec.White;
   clb1.TextSettings.Font.Size           := 48;
   clb1.TextSettings.Font.Style          := [TFontStyle.fsBold];
   clb1.Text                             := '1';
   cBtn1.TagString                       := clb1.Text;

   clb2                                  := TText.Create(cBtn2);
   clb2.Parent                           := cBtn2;
   clb2.Align                            := TAlignLayout.Client;
   clb2.HitTest                          := false;
   clb2.Locked                           := true;
   clb2.TextSettings.FontColor           := TAlphaColorRec.White;
   clb2.TextSettings.Font.Size           := 48;
   clb2.TextSettings.Font.Style          := [TFontStyle.fsBold];
   clb2.Text                             := '2';
   cBtn2.TagString                       := clb2.Text;

   clb3                                  := TText.Create(cBtn3);
   clb3.Parent                           := cBtn3;
   clb3.Align                            := TAlignLayout.Client;
   clb3.HitTest                          := false;
   clb3.Locked                           := true;
   clb3.TextSettings.FontColor           := TAlphaColorRec.White;
   clb3.TextSettings.Font.Size           := 48;
   clb3.TextSettings.Font.Style          := [TFontStyle.fsBold];
   clb3.Text                             := '3';
   cBtn3.TagString                       := clb3.Text;

   clb4                                  := TText.Create(cBtn4);
   clb4.Parent                           := cBtn4;
   clb4.Align                            := TAlignLayout.Client;
   clb4.HitTest                          := false;
   clb4.Locked                           := true;
   clb4.TextSettings.FontColor           := TAlphaColorRec.White;
   clb4.TextSettings.Font.Size           := 48;
   clb4.TextSettings.Font.Style          := [TFontStyle.fsBold];
   clb4.Text                             := '4';
   cBtn4.TagString                       := clb4.Text;

   clb5                                  := TText.Create(cBtn5);
   clb5.Parent                           := cBtn5;
   clb5.Align                            := TAlignLayout.Client;
   clb5.HitTest                          := false;
   clb5.Locked                           := true;
   clb5.TextSettings.FontColor           := TAlphaColorRec.White;
   clb5.TextSettings.Font.Size           := 48;
   clb5.TextSettings.Font.Style          := [TFontStyle.fsBold];
   clb5.Text                             := '5';
   cBtn5.TagString                       := clb5.Text;

   clb6                                  := TText.Create(cBtn6);
   clb6.Parent                           := cBtn6;
   clb6.Align                            := TAlignLayout.Client;
   clb6.HitTest                          := false;
   clb6.Locked                           := true;
   clb6.TextSettings.FontColor           := TAlphaColorRec.White;
   clb6.TextSettings.Font.Size           := 48;
   clb6.TextSettings.Font.Style          := [TFontStyle.fsBold];
   clb6.Text                             := '6';
   cBtn6.TagString                       := clb6.Text;

   clb7                                  := TText.Create(cBtn7);
   clb7.Parent                           := cBtn7;
   clb7.Align                            := TAlignLayout.Client;
   clb7.HitTest                          := false;
   clb7.Locked                           := true;
   clb7.TextSettings.FontColor           := TAlphaColorRec.White;
   clb7.TextSettings.Font.Size           := 48;
   clb7.TextSettings.Font.Style          := [TFontStyle.fsBold];
   clb7.Text                             := '7';
   cBtn7.TagString                       := clb7.Text;

   clb8                                  := TText.Create(cBtn8);
   clb8.Parent                           := cBtn8;
   clb8.Align                            := TAlignLayout.Client;
   clb8.HitTest                          := false;
   clb8.Locked                           := true;
   clb8.TextSettings.FontColor           := TAlphaColorRec.White;
   clb8.TextSettings.Font.Size           := 48;
   clb8.TextSettings.Font.Style          := [TFontStyle.fsBold];
   clb8.Text                             := '8';
   cBtn8.TagString                       := clb8.Text;

   clb9                                  := TText.Create(cBtn9);
   clb9.Parent                           := cBtn9;
   clb9.Align                            := TAlignLayout.Client;
   clb9.HitTest                          := false;
   clb9.Locked                           := true;
   clb9.TextSettings.FontColor           := TAlphaColorRec.White;
   clb9.TextSettings.Font.Size           := 48;
   clb9.TextSettings.Font.Style          := [TFontStyle.fsBold];
   clb9.Text                             := '9';
   cBtn9.TagString                       := clb9.Text;
end;

procedure TTecLayout.MouseEnter(Sender: TObject);
begin
  if (Sender is TRectangle) then
     TRectangle(Sender).Fill.Color := TAlphaColorRec.Gold;
end;

procedure TTecLayout.MouseLeave(Sender: TObject);
begin
  if (Sender is TRectangle) then
     TRectangle(Sender).Fill.Color := TAlphaColorRec.Null;
end;

procedure TTecLayout.Open(pComponent: TObject; pDescription : String; pAfter: TProc);
begin
  Try Owner.Owner.Focused := nil; Except end;
  Owner.Active(true);

  Component    := pComponent;
  AfterExecute := pAfter;

  LayoutConfigure;
  if (trim(pDescription) <> '') then
     tfDescricao.Text  := pDescription;
end;

procedure TTecLayout.SetCasasDecimais(const Value: Integer);
begin
  FCasasDecimais := Value;
end;

procedure TTecLayout.SetDisplay(const Value: String);
Var vFormatValue : String;
begin
  ComponentValue := SomenteNumeros(Value);
  vFormatValue   := ComponentValue;

  if (Format = ttFloat) then
  begin
     SetValid( true );
     Valid.Visible                 := false;
     tValue.TextSettings.Font.Size := 36;

     vFormatValue := FormataValor(vFormatValue, CasasDecimais);
  end else if (Format = ttCPF) then
  begin
     Valid.Visible                 := true;
     tValue.TextSettings.Font.Size := 28;

     if (length(vFormatValue) <= 11) then {CONSIDERA SENDO CPF}
     begin
        SetValid( CPFValido(vFormatValue) );
        vFormatValue := FormataCPF(vFormatValue);
     end else
        SetValid( false );
  end else if (Format = ttCNPJ) then
  begin
     Valid.Visible                 := true;
     tValue.TextSettings.Font.Size := 28;

     if (length(vFormatValue) <= 14) then {CONSIDERA SENDO CPF}
     begin
        SetValid( CNPJValido(vFormatValue) );
        vFormatValue := FormataCNPJ(vFormatValue);
     end else
        SetValid( false );
  end else if (Format = ttInteger) then
  begin
     SetValid( true );
     Valid.Visible                 := false;
     tValue.TextSettings.Font.Size := 36;
  end else if (Format = ttCelular) then
  begin
     Valid.Visible                 := true;
     tValue.TextSettings.Font.Size := 36;

     if (length(vFormatValue) <= 11) then {CONSIDERA NÚMERO COM 11 OU 10 DÍGITOS}
     begin
        SetValid( CELULARValido(vFormatValue) );
        vFormatValue := FormataCelular(vFormatValue);
     end else
        SetValid( false );
  end else if (Format = ttTelefone) then
  begin
     Valid.Visible                 := true;
     tValue.TextSettings.Font.Size := 36;

     if (length(vFormatValue) <= 11) then {CONSIDERA NÚMERO COM 11 OU 10 DÍGITOS}
     begin
        SetValid( TELEFONEValido(vFormatValue) );
        vFormatValue := FormataTelefone(vFormatValue);
     end else
        SetValid( false );
  end else if (Format = ttData) then
  begin
     Valid.Visible                 := true;
     tValue.TextSettings.Font.Size := 42;

     if (length(vFormatValue) <= 8) then { DATA 11.11.11 OU 11.11.1111 }
     begin
        SetValid( DATAValida(vFormatValue) );
        vFormatValue := FormataData(vFormatValue);
     end else
        SetValid( false );
  end else if (Format = ttCEP) then
  begin
     Valid.Visible                 := true;
     tValue.TextSettings.Font.Size := 42;

     if (length(vFormatValue) <= 8) then { CEP 95.980-000 }
     begin
        SetValid( CEPValido(vFormatValue) );
        vFormatValue := FormataCEP(vFormatValue);
     end else
        SetValid( false );
  end;

  tValue.Text    := vFormatValue;
end;

procedure TTecLayout.SetFormat(const Value: TTecTypeFormat);
begin
  FFormat := Value;
end;

procedure TTecLayout.SetValid(const Value: Boolean);
begin
  if (Value) then
  begin
     vDescricao.Text               := 'VÁLIDO';
     Valid.Fill.Color              := TAlphaColorRec.Seagreen;
     tFundoDescricao.Fill.Color    := TAlphaColorRec.Seagreen;
     tValue.TextSettings.FontColor := TAlphaColorRec.Seagreen;
     bOk.Enabled                   := true;
  end else begin
     vDescricao.Text               := 'INVÁLIDO';
     Valid.Fill.Color              := TAlphaColorRec.Red;
     tFundoDescricao.Fill.Color    := TAlphaColorRec.Red;
     tValue.TextSettings.FontColor := TAlphaColorRec.Red;
     bOk.Enabled                   := false;
  end;
end;

{ TTecFormParent }

procedure TTecFormParent.Active(Activate: Boolean);
begin
  Try
     if (assigned(Owner)) then
     begin
        if (Activate) then
        begin
           FormKeyDown      := FOwner.OnKeyDown;
           FormKeyUp        := FOwner.OnKeyUp;
           FormResize       := FOwner.OnResize;

           FOwner.OnKeyDown := KeyBoardKeyDown;
           FOwner.OnKeyUp   := KeyBoardKeyUp;
           FOwner.OnResize  := nil;
        end else begin
           Try
              FOwner.OnKeyDown := FormKeyDown;
              FOwner.OnKeyUp   := FormKeyUp;
              FOwner.OnResize  := FormResize;
           Finally
              FormKeyDown      := nil;
              FormKeyUp        := nil;
              FormResize       := nil;
           End;
        end;
     end;
  Except End;
end;

procedure TTecFormParent.KeyBoardKeyDown(Sender: TObject; var Key: Word; var KeyChar: Char; Shift: TShiftState);
begin
  if ((Key = vkHardwareBack) or (Key = vkReturn)) then
  begin
     Key := 0;
     Exit;
  end;
end;

procedure TTecFormParent.KeyBoardKeyUp(Sender: TObject; var Key: Word; var KeyChar: Char; Shift: TShiftState);
begin
  Key := 0;
end;

procedure TTecFormParent.SetOwner(const Value: TCommonCustomForm);
begin
  FOwner := Value;
end;

end.
