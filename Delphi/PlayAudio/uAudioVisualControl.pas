{*
**CLASSE DESENVOLVIDA POR: DOUGLAS COLOMBO
**SINCE 20-12-18
*}

unit uAudioVisualControl;

interface

uses UAudioControl, System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.Media, FMX.StdCtrls,
  FMX.Objects, System.Actions, FMX.ActnList, FMX.Controls.Presentation, FMX.Effects, System.IOUtils,
  FMX.Layouts, FMX.Graphics, FMX.Ani, FMX.ImgList, System.netEncoding, System.Messaging

  {$IFDEF ANDROID}
    , FMX.platform.Android, Androidapi.Jni.Os, Androidapi.Helpers, Androidapi.Jni.Widget, FMX.Helpers.Android,
    Androidapi.JNI.GraphicsContentViewText, Androidapi.JNI.JavaTypes, Androidapi.JNIBridge,
    Androidapi.jni.App
  {$ENDIF}
  ;

Type
  TAudioVisualControl = class
  protected
    Var
      countExibition : Single;
      resizeTemp : TNotifyEvent;
      keyDownTemp : TKeyEvent;

    {$IFDEF ANDROID}
      Const
        RECORD_AUDIO           : String = 'android.permission.RECORD_AUDIO';
        

      procedure HandleActivityMessage(const Sender: TObject; const M: TMessage);
      procedure onRequestPermissionsResult(aData: TPermissionsRequestResultData);
      procedure PedirPermissao(Permissao: array of string);
      Function TemPermissao(Permissao : array of String): Boolean;
    {$ENDIF}

    procedure parentResize(Sender: TObject);
    procedure parentKeyDown(Sender: TObject; var Key: Word; var KeyChar: Char; Shift: TShiftState);
    function Base64ToStream(imagem : String) : TMemoryStream;
  private
    FImgList : TImageList;

    btnFechar : TCircle;
    Audio : TAudioPlayer;
    lyMain : TLayout;
    rcFundo : TRectangle;
    flFundo : TFloatAnimation;

    rcCentro : TRectangle;
    ssCentro : TShadowEffect;

    rcInfo : TRectangle;
    lbInfo : TLabel;
    ssInfo : TShadowEffect;
    flInfo : TFloatAnimation;
    
    rcInfoExc : TRectangle;
    lbInfoExc : TLabel;
    ssInfoExc : TShadowEffect;
    flInfoExc : TFloatAnimation;

    btnExclusao : TRectangle;
    imgExclusao : TRectangle;

    lbExclusao  : TLabel;

    lyGravacao : TLayout;
    btnGravacao : TRectangle;
    imgGravacao : TRectangle;
    lbGravacao :TLabel;

    lyPlay  : TLayout;
    btnPlay : TRectangle;
    imgPlay : TRectangle;

    pgPlay  : TProgressBar;

    FnomeArquivo: String;
    Fparent: TCommonCustomForm;
    FSomenteLeitura: Boolean;

    procedure MouseEnter(Sender: TObject);
    procedure MouseLeave(Sender: TObject);

    procedure info(exibir : Boolean);
    procedure atualizarDimensoes;
    procedure carregarImgList;
    procedure onBackGroundClick(Obj : TObject);
    procedure flFundoOnFinish(Obj : TObject);
    procedure flInfoOnFinish(Obj : TObject);
    procedure flInfoExcOnFinish(Obj : TObject);
    procedure btnExclusaoClick(Obj : TObject);
    procedure btnGravacaoClick(Obj : TObject);
    procedure btnPlayClick(Obj : TObject);
    procedure SetnomeArquivo(const Value: String);
    procedure Setparent(const Value: TCommonCustomForm);
    procedure SetSomenteLeitura(const Value: Boolean);
  public
    property SomenteLeitura : Boolean read FSomenteLeitura write SetSomenteLeitura;
    property parent : TCommonCustomForm read Fparent write Setparent;
    property nomeArquivo : String read FnomeArquivo write SetnomeArquivo;

    procedure abrir(arquivo : String = '');
    procedure ouvir(arquivo : String = '');
    procedure fechar;
    
    Constructor create(pai : TCommonCustomForm = nil); overload;
    Destructor destroy; overload;
  end;


implementation

Const
  fotoFechar : String = 'iVBORw0KGgoAAAANSUhEUgAAAEgAAABICAYAAABV7bNHAAABHElEQVR4AWIYBYB27AADoSAKo3B4' +
                        'gJbULlpICwmzhBbwFhFaUoAwDYGuyhtw+jmHH4DxgXE/ZmZmZmZmZmbWez+OneE3LGOXscM/4jz6' +
                        'qwbirP3VnUeqOO81EKfjSF9wag3EqUgY0LX/rgE4tQsJtB+7EUgbcdaxBaChkHgcHonH4ZF4nHwk' +
                        'HodH4nHykXgcHgnAyUMCcPKQAJw8JAAnDwnAyUICcGKQxKlI4kwgiTPxzyk1cUTahCNSxRFpHmel' +
                        'btwpOAtx447BIW7ccTh5SABOHhKAk4cE4OQhATg8Eo+Tj8Tj8Eg8Tj4SjxOHNI+DIJ1JoNMMDoD0' +
                        'GDvuajxSxUGQKg6PROBUpCkcAKniEEjXrzhmZmZmZmZmZvYEbCYPnlG3fB4AAAAASUVORK5CYII=';

  fotoPlay : String = 'iVBORw0KGgoAAAANSUhEUgAAADAAAAAwCAQAAAD9CzEMAAAAi0lEQVR4Ae2WsRVAYBDGqJRqAxjDDrYwhE0sYQdjGECtVInmWyGVywJ5j/x31/yBoijYGVwB3Cy0pgDgYHQF8LDSaYJwMrkCeNnoNUG4'+
                      'mEVB2BlcQeI1BYnXFSReU5B4XUHiNQWJ1xf4n8j/yX6m/kPzR4U/7Pxx7S8ca2UKS98/W/zDyz8di6L4AR9PTYeLvT6BSgAAAABJRU5ErkJggg==';
  fotoStop : String = 'iVBORw0KGgoAAAANSUhEUgAAADAAAAAwAQAAAAB/ecQqAAAAAnRSTlMAAHaTzTgAAAATSURBVHgBY6AJ4P///8Pgp2gBAPUeR7nHgTImAAAAAElFTkSuQmCC';
  fotoMicrophone : String = 'iVBORw0KGgoAAAANSUhEUgAAADAAAAAwCAQAAAD9CzEMAAABh0lEQVR4Ae3UMwMWXBiA4efzl23tuZ+Tbbstrbkp2zXnJbsl27Z9pbPkXsX3mo7v7UTRz8N/ejkOjuvlv8gvNR2UgINqRv74wxbv2+qP'+
                            'yBcdAEeNMNdjQMfIF9vAFWXfzBp7DHZEvrgPFkViObgX+QKYEompQOEC0wofgBeFC0wHzwsXmAGe/cSBmeBpoQNPCheYVejAbPC40IFHhQvMyTHgHhj43nx5JFaAG5EYlOHn5xgYG4lt4LFmb2YVXAMb'+
                            'IzEOHIuvZS1YFYkOgEcWGuE4oM1759fF19IPPFYmzf+01fs2+TPeUNMLMCy+lnpegL6RqGU/kOxXMxKTAU3j61kB7qgQif/0dByc0st/kWjgGdgWmdAAsN7f8RmqOQ1oFpkxAbBcmfgEdR0ATIhM+dde'+
                            'wAUt4iO0cQuw17+ROZXtItmpvdqRqK29nRJ7VIrs+N9cCbhpr71uSsA8/0cutHXfp9zTNnKngg5WeSJJrhmtSuSPslpIdNHUX5F/kkiKgWIgT4qKXgLKHON17xQ48QAAAABJRU5ErkJggg==';
  fotoDelete : String = 'iVBORw0KGgoAAAANSUhEUgAAADAAAAAwCAQAAAD9CzEMAAAA0ElEQVR4Ae3UgQbCUBiG4UOaAQTRxYwA6KZ2KQVgDAMzdjkbMIAZ8CbGZ6BOPkWdF/Dje4A/pFKfiRKlSv+8i+DNEmAAfioi+zaQgAQs'+
                        'XGhR0HFhcQEz5xDIRNCxD4Ezswco1ktGr/lHFB6gYrfecnrNs6P2ABtiO+8ARCjNuwARmncD9QaozIDmRRgBzZOTi3ABmt/T0ZKJ8ACF5gEQUdheheZFeF6Fnl0HqNbw7OL6R2AgpiEeuBLTLR44MvJq'+
                        'I4cQHycaJp410XAKqZQ9dQddZ5kTV71WWQAAAABJRU5ErkJggg==';


{ TAudioVisualControl }

procedure ClearFileTemp(arquivoNome : String);
Var arquivoOld, oldName : String;
begin
  arquivoOld := Trim(arquivoNome);
  oldName    := ExtractFilePath(arquivoOld) + 'OLD_'+ ExtractFileName(arquivoOld);
  if (arquivoOld <> '') and FileExists(arquivoOld) then
  begin
     if not (DeleteFile(arquivoOld)) then
     begin
        if (FileExists(oldName)) then
           DeleteFile( oldName );
        RenameFile(arquivoOld, oldName);
     end;
  end;
end;

function DestruirComponente(obj : TFmxObject) : TFmxObject;
begin
  if (assigned(obj)) then
  begin
     Try
        {$IFDEF MSWINDOWS}
           obj.Free;
        {$ELSE}
           obj.DisposeOf;
        {$ENDIF}
     Except End;
  end;

  obj    := nil;
  result := obj;
end;

procedure TAudioVisualControl.abrir(arquivo: String);
begin
  {$IFDEF ANDROID}
    if not (TemPermissao([RECORD_AUDIO])) then
       raise Exception.Create('O APLICATIVO NÃO POSSUI PERMISSÃO PARA MANIPULAÇÃO DE AUDIO.');
  {$ENDIF}

  if (Fparent <> nil) then
  begin
     resizeTemp        := Fparent.OnResize;
     Fparent.OnResize  := parentResize;
     keyDownTemp       := Fparent.OnKeyDown;
     Fparent.OnKeyDown := parentKeyDown;
  end;

  if ((arquivo) <> '') then
     SetnomeArquivo(arquivo);

  btnExclusao.Visible         := false;
  lbExclusao.Visible          := false;
  lyPlay.Visible              := false;
  lyGravacao.Visible          := false;
  if FileExists(arquivo) then {SE EXISTE ARQUIVO, ABRE PARA EXIBIÇÃO}
  begin
     countExibition           := countExibition + 1;
     atualizarDimensoes;
     lyPlay.Visible           := true;

     if not(FSomenteLeitura) then
     begin
        btnExclusao.Visible   := true;
        lbExclusao.Visible    := true;
     end;

     pgPlay.Value             := 0;
     btnPlayClick(btnPlay); {LOGO DE CARA INICIA O ÁUDIO}
  end else begin
     if (FSomenteLeitura) then
     begin
        fechar;
        if ((arquivo) = '') or not (FileExists(arquivo)) then
           raise Exception.Create('Arquivo de mídia não encontrado.'+ sLineBreak+ arquivo)
        else
           raise Exception.Create('Desculpe, você só tem permissão de escutar este áudio.');
     end else begin
        lyGravacao.Visible     := True;
        btnGravacao.Fill.Color := TAlphaColorRec.Seagreen;
        imgGravacao.Fill.Bitmap.Bitmap := FImgList.Bitmap(TSizeF.Create(36, 36), 2);

        lbGravacao.Text        := 'Clique no microfone para gravar';
        countExibition         := countExibition + 1;
        atualizarDimensoes;
     end;
  end;
end;

procedure TAudioVisualControl.atualizarDimensoes;
begin
  {PAI DE TODOS}
  lyMain.Position.X                     := 0;
  lyMain.Position.Y                     := 0;
  lyMain.Height                         := Fparent.Height;
  lyMain.Width                          := Fparent.Width;
  lyMain.BringToFront;

  {fundo}
  rcFundo.Parent                        := lyMain;
  rcFundo.Stroke.Thickness              := 0;
  rcFundo.Fill.Kind                     := TBrushKind.Solid;
  rcFundo.Fill.Color                    := TAlphaColorRec.Black;
  rcFundo.Opacity                       := 0.9;
  rcFundo.Position.X                    := 0;
  rcFundo.Position.Y                    := 0;
  rcFundo.Height                        := lyMain.Height;
  rcFundo.Width                         := lyMain.Width;
  rcFundo.OnClick                       := onBackGroundClick;
  if (rcFundo.Visible) then
     rcFundo.BringToFront;

  {ANIMAÇÃO FUNDO - ENTRADA e SAÍDA}
  flFundo.Parent                        := rcFundo;
  flFundo.Enabled                       := false;
  flFundo.AnimationType                 := TAnimationType.InOut;
  flFundo.Delay                         := 0;
  flFundo.AutoReverse                   := false;
  flFundo.Duration                      := 0.4;
  flFundo.Inverse                       := false;
  flFundo.Loop                          := false;
  flFundo.PropertyName                  := 'Position.Y';
  flFundo.StartValue                    := lyMain.Height + 20;
  flFundo.StopValue                     := 0;
  flFundo.OnFinish                      := flFundoOnFinish;  

  {componente - centro fundo}
  rcCentro.Visible                      := false;
  rcCentro.Parent                       := lyMain;
  rcCentro.Align                        := TAlignLayout.Center;
  rcCentro.Height                       := 50;
  rcCentro.Width                        := Trunc((Fparent.Width / 3) * 2) + 60;
  rcCentro.Fill.Kind                    := TBrushKind.Solid;
  rcCentro.Fill.Color                   := TAlphaColorRec.white;
  rcCentro.Stroke.Thickness             := 0;
  rcCentro.Stroke.Color                 := TAlphaColorRec.Black;
  rcCentro.XRadius                      := 8;
  rcCentro.YRadius                      := 8;

  {botão VOLTAR/CONFIRMAR}
  btnFechar.Visible                     := false;
  btnFechar.Name                        := 'AudBtnFechar1';
  btnFechar.Parent                      := lyMain;
  btnFechar.Height                      := 60;
  btnFechar.Width                       := 60;
  btnFechar.Position.X                  := Trunc((lyMain.Width / 2) - (btnFechar.Width / 2));
  btnFechar.Position.Y                  := 30;
  btnFechar.Stroke.Thickness            := 3;
  btnFechar.Cursor                      := crHandPoint;
  btnFechar.Stroke.Color                := TAlphaColorRec.White;
  btnFechar.Fill.Kind                   := TBrushKind.Bitmap;
  btnFechar.Fill.Bitmap.Bitmap          := FImgList.Bitmap(TSizeF.Create(72, 72), 4);
  btnFechar.Fill.Bitmap.WrapMode        := TWrapMode.TileStretch;
  btnFechar.OnClick                     := onBackGroundClick;
  btnFechar.OnMouseEnter                := MouseEnter;
  btnFechar.OnMouseLeave                := MouseLeave;
  btnFechar.BringToFront;

  {SOMBRA COMPONENTE CENTRO}
  ssCentro.Parent                       := rcCentro;
  ssCentro.Distance                     := 3;
  ssCentro.Direction                    := 45;
  ssCentro.Softness                     := 0.3;
  ssCentro.Opacity                      := 0.8;
  ssCentro.ShadowColor                  := TAlphaColorRec.Black;
  ssCentro.Enabled                      := true;

  {INFO - FUNDO}
  rcInfo.Parent                         := rcCentro;
  rcInfo.Stroke.Thickness               := 0;
  rcInfo.Fill.Color                     := TAlphaColorRec.Dodgerblue;
  rcInfo.Height                         := 20;
  rcInfo.Width                          := 220;
  rcInfo.Position.X                     := lyMain.Width + 20;
  rcInfo.Position.Y                     := -40;
  rcInfo.XRadius                        := 8;
  rcInfo.YRadius                        := 8;
  rcInfo.BringToFront;
  
  {INFO - MENSAGEM}
  lbInfo.Parent                         := rcInfo;
  lbInfo.Align                          := TAlignLayout.Client;
  lbInfo.StyledSettings                 := [];
  lbInfo.Text                           := 'CLIQUE NO BOTÃO AZUL PARA SALVAR';
  lbInfo.TextSettings.HorzAlign         := TTextAlign.Center;
  lbInfo.TextSettings.Font.Size         := 10;
  lbInfo.TextSettings.FontColor         := TAlphaColorRec.White;
  lbInfo.TextSettings.Font.Style        := [TFontStyle.fsBold];

  {SOMBRA INFORMATIVO}
  ssInfo.Parent                         := rcInfo;
  ssInfo.Distance                       := 3;
  ssInfo.Direction                      := 45;
  ssInfo.Softness                       := 0.3;
  ssInfo.Opacity                        := 0.8;
  ssInfo.ShadowColor                    := TAlphaColorRec.Black;
  ssInfo.Enabled                        := true;

  {ANIMAÇÃO AVISO DE CONFIRMAÇÃO DE GRAVAÇÃO}
  flInfo.Enabled                        := false;
  flInfo.Parent                         := rcInfo;
  flInfo.AnimationType                  := TAnimationType.InOut;
  flInfo.Delay                          := 0;
  flInfo.AutoReverse                    := false;
  flInfo.Duration                       := 0.5;
  flInfo.Inverse                        := false;
  flInfo.Loop                           := false;
  flInfo.PropertyName                   := 'Position.X';
  flInfo.StartValue                     := lyMain.Width + 20;
  flInfo.StopValue                      := 0;
  flInfo.OnFinish                       := flInfoOnFinish;
  

  {INFO EXCLUSÃO - FUNDO}
  rcInfoExc.Parent                      := rcCentro;
  rcInfoExc.Stroke.Thickness            := 0;
  rcInfoExc.Fill.Color                  := TAlphaColorRec.Red;
  rcInfoExc.Height                      := 20;
  rcInfoExc.Width                       := 220;
  rcInfoExc.Position.X                  := ((lyMain.Width + 30) * -1);
  rcInfoExc.Position.Y                  := ((rcCentro.Height - rcInfoExc.Height) + 40);
  rcInfoExc.XRadius                     := 8;
  rcInfoExc.YRadius                     := 8;
  rcInfoExc.BringToFront;
  
  {INFO EXCLUSÃO - MENSAGEM}
  lbInfoExc.Parent                      := rcInfoExc;
  lbInfoExc.Align                       := TAlignLayout.Client;
  lbInfoExc.StyledSettings              := [];
  lbInfoExc.Text                        := 'CLIQUE NA LIXEIRA PARA CANCELAR';
  lbInfoExc.TextSettings.HorzAlign      := TTextAlign.Center;
  lbInfoExc.TextSettings.Font.Size      := 10;
  lbInfoExc.TextSettings.FontColor      := TAlphaColorRec.White;
  lbInfoExc.TextSettings.Font.Style     := [TFontStyle.fsBold];

  {SOMBRA EXCLUSÃO INFORMATIVO}
  ssInfoExc.Parent                      := rcInfoExc;
  ssInfoExc.Distance                    := 3;
  ssInfoExc.Direction                   := 45;
  ssInfoExc.Softness                    := 0.3;
  ssInfoExc.Opacity                     := 0.8;
  ssInfoExc.ShadowColor                 := TAlphaColorRec.Black;
  ssInfoExc.Enabled                     := true;  

  {ANIMAÇÃO AVISO DE CANCELAMENTO DA GRAVAÇÃO}
  flInfoExc.Enabled                     := false;
  flInfoExc.Parent                      := rcInfoExc;
  flInfoExc.AnimationType               := TAnimationType.InOut;
  flInfoExc.Delay                       := 0;
  flInfoExc.AutoReverse                 := false;
  flInfoExc.Duration                    := 0.5;
  flInfoExc.Inverse                     := false;
  flInfoExc.Loop                        := false;
  flInfoExc.PropertyName                := 'Position.X';
  flInfoExc.StartValue                  := ((lyMain.Width + 20) * -1);
  flInfoExc.StopValue                   := rcCentro.Width - rcInfoExc.Width;
  flInfoExc.OnFinish                    := flInfoExcOnFinish;

  {BOTÃO EXCLUSÃO}
  btnExclusao.Parent                    := rcCentro;
  btnExclusao.Name                      := 'AudBtnExclusao1';
  btnExclusao.Fill.Color                := TAlphaColorRec.Red;
  btnExclusao.Stroke.Kind               := TBrushKind.None;
  btnExclusao.Align                     := TAlignLayout.MostRight;
  btnExclusao.Height                    := 38; {PRECISA}
  btnExclusao.Margins                   := TBounds.Create(tRectF.Create(4, 6, 6, 6));
  btnExclusao.Width                     := btnExclusao.Height;
  btnExclusao.Cursor                    := crHandPoint;
  btnExclusao.OnClick                   := btnExclusaoClick;

  {IMAGEM BTN EXCLUSÃO}
  imgExclusao.Parent                    := btnExclusao;
  imgExclusao.Align                     := TAlignLayout.Client;
  imgExclusao.Locked                    := true;
  imgExclusao.HitTest                   := false;
  imgExclusao.Fill.Kind                 := TBrushKind.Bitmap;
  imgExclusao.Fill.Bitmap.WrapMode      := TWrapMode.TileStretch;
  imgExclusao.Fill.Bitmap.Bitmap        := FImgList.Bitmap(TSizeF.Create(36, 36), 3);
  imgExclusao.Stroke.Kind               := TBrushKind.None;

  {Info Exclusão}
  lbExclusao.Parent                     := rcCentro;
  lbExclusao.Width                      := 56;
  lbExclusao.Align                      := TAlignLayout.Right;
  lbExclusao.StyledSettings             := [];
  lbExclusao.Text                       := 'Excluir';
  lbExclusao.TextSettings.HorzAlign     := TTextAlign.Trailing;
  lbExclusao.TextSettings.Font.Size     := 13;
  lbExclusao.TextSettings.FontColor     := TAlphaColorRec.Red;
  lbExclusao.TextSettings.Font.Style    := [TFontStyle.fsBold];
  lbExclusao.OnClick                    := btnExclusaoClick;
  lbExclusao.Visible                    := false;
                                                
  {FUNDO - DADOS DE GRAVAÇÃO}
  lyGravacao.Parent                     := rcCentro;
  lyGravacao.Align                      := TAlignLayout.Client;
  lyGravacao.HitTest                    := false;
  lyGravacao.Locked                     := true;
  lyGravacao.OnClick                    := btnGravacaoClick;
  lyGravacao.OnMouseEnter               := MouseEnter;
  lyGravacao.OnMouseLeave               := MouseLeave;

  {BUTTON QUE INICIA GRAVAÇÃO E REGISTRA A GRAVAÇÃO}
  btnGravacao.Parent                    := lyGravacao;
  btnGravacao.Name                      := 'AudBtnGravacao1';
  btnGravacao.Height                    := 38; {precisa}
  btnGravacao.Align                     := TAlignLayout.MostLeft;
  btnGravacao.Margins                   := TBounds.Create(tRectF.Create(6, 6, 2, 6));
  btnGravacao.Width                     := btnGravacao.Height;
  btnGravacao.Fill.Color                := TAlphaColorRec.Seagreen;
  btnGravacao.Stroke.Kind               := TBrushKind.None;
  btnGravacao.Cursor                    := crHandPoint;
  btnGravacao.OnClick                   := btnGravacaoClick;
  btnGravacao.OnMouseEnter              := MouseEnter;
  btnGravacao.OnMouseLeave              := MouseLeave;

  {IMAGEM DO BTN DE GRAVACAO}
  imgGravacao.Parent                    := btnGravacao;
  imgGravacao.Locked                    := true;
  imgGravacao.HitTest                   := false;
  imgGravacao.Align                     := TAlignLayout.Client;
  imgGravacao.Fill.Kind                 := TBrushKind.Bitmap;
  imgGravacao.Fill.Bitmap.WrapMode      := TWrapMode.TileStretch;
//  imgGravacao.Fill.Bitmap.Bitmap        := FImgList.Bitmap(TSizeF.Create(36, 36), 0);
  imgGravacao.Stroke.Kind               := TBrushKind.None;


  {INFO GRAVAÇÃO E TIMER CONTROLADO PELO MEDIA PLAYER PARA INFORMAR O TEMPO DE ÁUDIO GRAVADO}
  lbGravacao.Parent                     := lyGravacao;
  lbGravacao.Align                      := TAlignLayout.Client;
  lbGravacao.StyledSettings             := [];
  lbGravacao.Text                       := 'Clique no microfone para gravar';
  lbGravacao.TextSettings.HorzAlign     := TTextAlign.Leading;
  lbGravacao.TextSettings.Font.Size     := 13;
  lbGravacao.TextSettings.FontColor     := TAlphaColorRec.Seagreen;
  lbGravacao.TextSettings.Font.Style    := [TFontStyle.fsBold];
  lbGravacao.OnClick                    := btnGravacaoClick;

  {FUNDO - DADOS DE EXECUÇÃO}
  lyPlay.Parent                         := rcCentro;
  lyPlay.Margins.Left                   := 4;
  lyPlay.Align                          := TAlignLayout.Client;
  lyPlay.Visible                        := false;

  {BUTTON QUE EXECUTA O ÁUDIO}
  btnPlay.Parent                        := lyPlay;
  btnPlay.Name                          := 'AudBtnPlay1';
  btnPlay.Fill.Color                    := TAlphaColorRec.Seagreen;
  btnPlay.Stroke.Kind                   := TBrushKind.None;
  btnPlay.Height                        := 38; {precisa}
  btnPlay.Align                         := TAlignLayout.MostLeft;
  btnPlay.Margins                       := TBounds.Create(tRectF.Create(6, 6, 2, 6));
  btnPlay.Width                         := btnPlay.Height;
  btnPlay.Cursor                        := crHandPoint;
  btnPlay.OnClick                       := btnPlayClick;

  //imagem do botão play
  imgPlay.Parent                        := btnPlay;
  imgPlay.Name                          := 'AudImgPlay1';
  imgPlay.HitTest                       := false;
  imgPlay.Locked                        := true;
  imgPlay.Fill.Kind                     := TBrushKind.Bitmap;
  imgPlay.Fill.Bitmap.WrapMode          := TWrapMode.TileStretch;
  imgPlay.Fill.Bitmap.Bitmap            := FImgList.Bitmap(TSizeF.Create(36, 36), 0); {ICONE DE PLAY}
  imgPlay.Stroke.Kind                   := TBrushKind.None;
  imgPlay.Align                         := TAlignLayout.Client;

  imgGravacao.Locked                    := true;
  imgGravacao.HitTest                   := false;

  {BARRA DE PROGRESSO DO ÁUDIO}
  pgPlay.Parent                         := lyPlay;
  pgPlay.Align                          := TAlignLayout.Left;
  pgPlay.Align                          := TAlignLayout.None;
  pgPlay.Height                         := 10;

  if (FSomenteLeitura) then
     pgPlay.Width                       := ((lyPlay.Width - btnPlay.Width) - 25)
  else
     pgPlay.Width                       := ((lyPlay.Width - btnPlay.Width) - 8);

  pgPlay.Margins                        := TBounds.Create(tRectF.Create(4, 6, 4, 6));
  pgPlay.Value                          := 25;
  pgPlay.Position.Y                     := Trunc((lyPlay.Height / 2) - (pgPlay.Height / 2));
  pgPlay.Anchors                        := [TAnchorKind.akLeft, TAnchorKind.akTop, TAnchorKind.akRight];
                                                                               
  {inicía visualização}
  lyMain.Parent                         := Fparent;
  flFundo.Enabled                       := true;
end;

function TAudioVisualControl.Base64ToStream(imagem: String): TMemoryStream;
var Base64 : TBase64Encoding;
    bytes : tBytes;
begin
  Try
    Base64 := TBase64Encoding.Create;
    bytes  := Base64.DecodeStringToBytes(imagem);
    result := TBytesStream.Create(bytes);
    result.Position := 0;
//    result.Seek(0, 0);
  Finally
    Base64.Free;
    Base64:=nil;
    SetLength(bytes, 0);
  End;
end;

procedure TAudioVisualControl.btnExclusaoClick(Obj: TObject);
  procedure voltarInicio;
  begin
     ClearFileTemp(Audio.NomeArquivo); //LIMPA ARQUIVO TEMPORÁRIO DE ÁUDIO

     lyGravacao.Visible            := true;
     lyPlay.Visible                := false;
     lbGravacao.Text               := 'Clique no microfone para gravar';
     btnGravacao.Fill.Color        := TAlphaColorRec.Seagreen;
     btnExclusao.Visible           := false;
     lbExclusao.Visible            := false;
  end;

begin
  if (lyGravacao.Visible) then {O ÁUDIO FOI CANCELADO}
     audio.StopCapture
  else
     audio.Stop;
  voltarInicio;
  info(false);
end;

procedure TAudioVisualControl.btnGravacaoClick(Obj: TObject);
begin
  if (btnGravacao.Fill.Color = TAlphaColorRec.Seagreen) then {INICIA CAPTAÇÃO DO ÁUDIO}
  begin
     ClearFileTemp(Audio.NomeArquivo); //LIMPA ARQUIVO TEMPORÁRIO DE ÁUDIO
     Audio.StartCapture(lbGravacao);
     if not DirectoryExists(ExtractFilePath(Audio.NomeArquivo)) then
        CreateDir(ExtractFilePath(Audio.NomeArquivo));
     info(true);
  end else if (btnGravacao.Fill.Color = TAlphaColorRec.Dodgerblue) then {PARA A GRAVAÇÃO DO ÁUDIO}
  begin
     pgPlay.Value        := 0;
     lyGravacao.Visible  := false;
     lyPlay.Visible      := true;
     info(false);
     Audio.StopCapture;
     btnPlayClick(btnPlay); {LOGO DE CARA INICIA O ÁUDIO}
  end;

  if not (FSomenteLeitura) then
  begin
     btnExclusao.Visible   := true;
     lbExclusao.Visible    := true;
  end;
end;

procedure TAudioVisualControl.btnPlayClick(Obj: TObject);
begin
  Try
    if (Trim(Audio.NomeArquivo) <> '') and (FileExists(Audio.NomeArquivo)) then
       Audio.Play(pgPlay)
    else begin
       if (FSomenteLeitura) then
          btnExclusaoClick(btnExclusao);

       ShowMessage('Nenhum áudio encontrado.');
    end;
  Except on E : Exception do
    raise Exception.Create('Erro ao dar "Play": '+ E.Message +sLineBreak+sLineBreak+ 'Arquivo: '+ Audio.NomeArquivo);
  End;
end;

procedure TAudioVisualControl.carregarImgList;
var sourceItem : TCustomSourceItem;
    destinationItem : TCustomDestinationItem;
    layer : TLayer;
begin
   Try
    {PLAY}
    sourceItem := FImgList.Source.Add;
    sourceItem.Name := 'play';
    sourceItem.MultiResBitmap.LoadItemFromStream( Base64ToStream(fotoPlay), 3);
    destinationItem := FImgList.Destination.Add;
    layer := destinationItem.Layers.Add;
    layer.SourceRect.Rect := TRectF.Create(0, 0, 16, 16);
    layer.Name := sourceItem.Name;

    {STOP}
    sourceItem := FImgList.Source.Add;
    sourceItem.Name := 'stop';
    sourceItem.MultiResBitmap.LoadItemFromStream( Base64ToStream(fotoStop), 3);
    destinationItem := FImgList.Destination.Add;
    layer := destinationItem.Layers.Add;
    layer.SourceRect.Rect := TRectF.Create(0, 0, 16, 16);
    layer.Name := sourceItem.Name;

    {MICROPHONE}
    sourceItem := FImgList.Source.Add;
    sourceItem.Name := 'microphone';
    sourceItem.MultiResBitmap.LoadItemFromStream( Base64ToStream(fotoMicrophone), 3);
    destinationItem := FImgList.Destination.Add;
    layer := destinationItem.Layers.Add;
    layer.SourceRect.Rect := TRectF.Create(0, 0, 16, 16);
    layer.Name := sourceItem.Name;

    {DELETE}
    sourceItem := FImgList.Source.Add;
    sourceItem.Name := 'delete';
    sourceItem.MultiResBitmap.LoadItemFromStream( Base64ToStream(fotoDelete), 3);
    destinationItem := FImgList.Destination.Add;
    layer := destinationItem.Layers.Add;
    layer.SourceRect.Rect := TRectF.Create(0, 0, 16, 16);
    layer.Name := sourceItem.Name;

    {FECHAR}
    sourceItem := FImgList.Source.Add;
    sourceItem.Name := 'fechar';
    sourceItem.MultiResBitmap.LoadItemFromStream( Base64ToStream(fotoFechar), 1);
    destinationItem := FImgList.Destination.Add;
    layer := destinationItem.Layers.Add;
    layer.SourceRect.Rect := TRectF.Create(0, 0, 72, 72);
    layer.Name := sourceItem.Name;
   Finally
    destinationItem := nil;
    sourceItem := nil;
    layer := nil;
   End;
end;

constructor TAudioVisualControl.create(pai: TCommonCustomForm);
begin
  countExibition := 0; {QTDE DE VEZES QUE FOI EXIBIDO O COMPONENTE VISUAL}
  if (pai = nil) then
     setParent(screen.ActiveForm)
  else
     setParent(pai);

  FSomenteLeitura := false;
  Audio        := TAudioPlayer.create(Fparent);
  lyMain       := TLayout.Create(Fparent);

  rcFundo      := TRectangle.Create(lyMain);
  flFundo      := TFloatAnimation.Create(rcFundo);

  btnFechar    := TCircle.Create(lyMain);

  rcCentro     := TRectangle.Create(lyMain);
  ssCentro     := TShadowEffect.Create(rcCentro);
  
  rcInfo       := TRectangle.Create(lyMain);
  lbInfo       := TLabel.Create(rcInfo);
  ssInfo       := TShadowEffect.Create(rcInfo);
  flInfo       := TFloatAnimation.Create(rcInfo);
  
  rcInfoExc    := TRectangle.Create(lyMain);
  lbInfoExc    := TLabel.Create(rcInfoExc);
  ssInfoExc    := TShadowEffect.Create(rcInfoExc);
  flInfoExc    := TFloatAnimation.Create(rcInfoExc);

  btnExclusao  := TRectangle.Create(rcFundo);
  btnExclusao.OnMouseEnter := MouseEnter;
  btnExclusao.OnMouseLeave := MouseLeave;

  imgExclusao  := TRectangle.Create(btnExclusao);
  lbExclusao   := TLabel.Create(rcFundo);

  lyGravacao   := TLayout.Create(rcCentro);
  btnGravacao  := TRectangle.Create(lyGravacao);
  btnGravacao.OnMouseEnter := MouseEnter;
  btnGravacao.OnMouseLeave := MouseLeave;

  imgGravacao  := TRectangle.Create(btnGravacao);
  lbGravacao   := TLabel.Create(lyGravacao);

  lyPlay       := TLayout.Create(rcCentro);
  btnPlay      := TRectangle.Create(lyPlay);
  btnPlay.OnMouseEnter := MouseEnter;
  btnPlay.OnMouseLeave := MouseLeave;
  imgPlay      := TRectangle.Create(btnPlay);
  pgPlay       := TProgressBar.Create(lyPlay);
  FImgList     := TImageList.Create(lyMain);
  carregarImgList;
end;

destructor TAudioVisualControl.destroy;
begin
  Try Audio.StopCapture; Except End;
  Try Audio.Stop; Except End;
  try
    Audio.Destroy;
    Audio := nil;
  Except end;
  Try
    {$IFDEF MSWINDOWS}
       FImgList.Free;
    {$ELSE}
       FImgList.DisposeOf;
    {$ENDIF}
    FImgList := nil;
  Except end;

  pgPlay      := TProgressBar(DestruirComponente(pgPlay));
  imgPlay     := TRectangle(DestruirComponente(imgPlay));
  btnPlay     := TRectangle(DestruirComponente(btnPlay));
  lyPlay      := TLayout(DestruirComponente(lyPlay));
  lbGravacao  := TLabel(DestruirComponente(lbGravacao));
  imgGravacao := TRectangle(DestruirComponente(imgGravacao));
  btnGravacao := TRectangle(DestruirComponente(btnGravacao));
  lyGravacao  := TLayout(DestruirComponente(lyGravacao));
  lbExclusao  := TLabel(DestruirComponente(lbExclusao));
  imgExclusao := TRectangle(DestruirComponente(imgExclusao));
  btnExclusao := TRectangle(DestruirComponente(btnExclusao));
  flInfoExc   := TFloatAnimation(DestruirComponente(flInfoExc));
  ssInfoExc   := TShadowEffect(DestruirComponente(ssInfoExc));
  lbInfoExc   := TLabel(DestruirComponente(lbInfoExc));
  rcInfoExc   := TRectangle(DestruirComponente(rcInfoExc));
  flInfo      := TFloatAnimation(DestruirComponente(flInfo));
  ssInfo      := TShadowEffect(DestruirComponente(ssInfo));
  lbInfo      := TLabel(DestruirComponente(lbInfo));
  rcInfo      := TRectangle(DestruirComponente(rcInfo));
  btnFechar   := TCircle(DestruirComponente(btnFechar));
  flFundo     := TFloatAnimation(DestruirComponente(flFundo));
  rcFundo     := TRectangle(DestruirComponente(rcFundo));
  ssCentro    := TShadowEffect(DestruirComponente(ssCentro));
  rcCentro    := TRectangle(DestruirComponente(rcCentro));
  lyMain      := TLayout(DestruirComponente(lyMain));
  Fparent     := nil;
end;

procedure TAudioVisualControl.fechar;
begin
  if (Fparent <> nil) then
  begin
     if assigned(resizeTemp) then
        Fparent.OnResize  := resizeTemp
     else
        Fparent.OnResize  := nil;

     if assigned(keyDownTemp) then
        Fparent.OnKeyDown := keyDownTemp
     else
        Fparent.OnKeyDown := nil;
  end;

  rcCentro.Visible  := false;
  btnFechar.Visible := false;
  flFundo.Inverse   := true;
  flFundo.Enabled   := true;
end;

procedure TAudioVisualControl.flFundoOnFinish(Obj: TObject);
begin
  flFundo.Enabled := false;
  if (flFundo.Inverse) then
     lyMain.Parent    := nil
  else begin
     rcCentro.Visible  := true;
     btnFechar.Visible := true;
     rcCentro.BringToFront;
     btnFechar.BringToFront;
  end;
end;

procedure TAudioVisualControl.flInfoExcOnFinish(Obj: TObject);
begin
  flInfoExc.Enabled := false;
end;

procedure TAudioVisualControl.flInfoOnFinish(Obj: TObject);
begin
  flInfo.Enabled := false;
end;

{$IFDEF ANDROID}
  procedure TAudioVisualControl.HandleActivityMessage(const Sender: TObject;const M: TMessage);
  begin
    FMX.Types.Log.d('UAudioVisualControl: HandleActivtyMessage');
    if M is TPermissionsRequestResultMessage then
       onRequestPermissionsResult(TPermissionsRequestResultMessage(M).Value);
  end;
{$ENDIF}

procedure TAudioVisualControl.info(exibir: Boolean);
begin
  if (exibir) then
  begin
     imgGravacao.Fill.Bitmap.Bitmap := FImgList.Bitmap(TSizeF.Create(36, 36), 1);
     btnGravacao.Fill.Color         := TAlphaColorRec.Dodgerblue;
     lbGravacao.TextSettings.FontColor := TAlphaColorRec.Dodgerblue;

     if (rcInfo.Position.X <> 0) then {SE AINDA NÃO ESTIVER VISÍVEL, ENTÃO EXIBE}
     begin
        flInfo.Inverse := false;
        flInfo.Enabled := true;
     end;

     if (rcInfoExc.Position.X < 0) then
     begin
        flInfoExc.Inverse := false;
        flInfoExc.Enabled := true;
     end;
  end else begin
     imgGravacao.Fill.Bitmap.Bitmap    := FImgList.Bitmap(TSizeF.Create(36, 36), 2); {ICONE DE PLAY}
     lbGravacao.TextSettings.FontColor := TAlphaColorRec.Seagreen;

     if (rcInfo.Position.X = 0) then {SE AINDA ESTIVER VISÍVEL, ENTÃO ESCONDE}
     begin
        flInfo.Inverse := true;
        flInfo.Enabled := true;
     end;

     if (rcInfoExc.Position.X >= 0) then
     begin
        flInfoExc.Inverse := true;
        flInfoExc.Enabled := true;
     end;
  end;
end;

procedure TAudioVisualControl.MouseEnter(Sender: TObject);
begin
  if (Sender is TRectangle) then
     TRectangle(Sender).Opacity := 0.5
  else if (Sender is TRoundRect) then
     TRoundRect(Sender).Opacity := 0.5
  else if (Sender is TCircle) then
     TCircle(Sender).Opacity := 0.5;
end;

procedure TAudioVisualControl.MouseLeave(Sender: TObject);
begin
  if (Sender is TRectangle) then
     TRectangle(Sender).Opacity := 1
  else if (Sender is TRoundRect) then
     TRoundRect(Sender).Opacity := 1
  else if (Sender is TCircle) then
     TCircle(Sender).Opacity := 1;
end;

procedure TAudioVisualControl.onBackGroundClick(Obj: TObject);
begin
  if (lyGravacao.Visible) then
  begin
     Audio.StopCapture;
     if (FSomenteLeitura) then
        btnExclusaoClick(btnExclusao);
  end else if (lyPlay.Visible) then
     Audio.Stop;
     
  fechar;
end;

{$IFDEF ANDROID}
  procedure TAudioVisualControl.onRequestPermissionsResult(aData: TPermissionsRequestResultData);
  var i: Integer;
  begin
    FMX.Types.Log.d('UAudioVisualControl: OnRequstPermissionsResult');
    if (aData.GrantResults.Length > 0) then
    begin
      for i := 0 to aData.GrantResults.Length -1 do
      begin
        if(aData.GrantResults[i] <> TJPackageManager.JavaClass.PERMISSION_GRANTED) then
          Exit;
      end;
    end;
  end;
{$ENDIF}

procedure TAudioVisualControl.ouvir(arquivo: String);
begin
  {$IFDEF ANDROID}
    if not (TemPermissao([RECORD_AUDIO])) then
       raise Exception.Create('O APLICATIVO NÃO POSSUI PERMISSÃO PARA MANIPULAÇÃO DE AUDIO.');
  {$ENDIF}

  if ((arquivo) <> '') then
     SetnomeArquivo(arquivo);

  if (FileExists(arquivo)) then {SE EXISTE ARQUIVO, ABRE PARA EXIBIÇÃO}
  begin
     countExibition           := countExibition + 1;
     pgPlay.Value             := 0;
     btnPlayClick(btnPlay); {LOGO DE CARA INICIA O ÁUDIO}
  end else begin
     if ((arquivo) = '') or not (FileExists(arquivo)) then
        raise Exception.Create('Arquivo de mídia não encontrado.'+ sLineBreak+ arquivo)
     else
        raise Exception.Create('Desculpe, você só tem permissão de escutar este áudio.');
  end;
end;

procedure TAudioVisualControl.parentKeyDown(Sender: TObject; var Key: Word; var KeyChar: Char; Shift: TShiftState);
begin
    if (Key = vkHardwareBack) then
    begin
       key := 0;
       fechar;
    end;
end;

procedure TAudioVisualControl.parentResize(Sender: TObject);
begin
  lyMain.Height              := Fparent.Height;
  lyMain.Width               := Fparent.Width;
  rcFundo.Height             := Fparent.Height;
  rcFundo.Width              := Fparent.Width;
  rcCentro.Width             := Trunc((Fparent.Width / 3) * 2) + 60;
  flInfo.StartValue          := lyMain.Width + 20;
  flInfoExc.StopValue        := rcCentro.Width - rcInfoExc.Width;
  btnFechar.Position.X       := Trunc((lyMain.Width / 2) - (btnFechar.Width / 2));

  if (rcInfo.Position.X <> 0) then {NÃO ESTÁ VISÍVEL}
     rcInfo.Position.X          := lyMain.Width + 20;

  if (rcInfoExc.Position.X > 0) then
     rcInfoExc.Position.X    := rcCentro.Width - rcInfoExc.Width;
end;

{$IFDEF ANDROID}
  procedure TAudioVisualControl.PedirPermissao(Permissao: array of string);
  Var Permissions : TJavaObjectArray<JString>;
      i: Integer;
  begin
    Permissions := TJavaObjectArray<JString>.Create(length(Permissao));
    for i := 0 to length(Permissao) -1 do
      Permissions.Items[i] := StringToJString(Permissao[i]);
    TAndroidHelper.Activity.requestPermissions(Permissions, 555);
  end;
{$ENDIF}

procedure TAudioVisualControl.SetnomeArquivo(const Value: String);
begin
  FnomeArquivo := Value;
  Audio.NomeArquivo := FnomeArquivo;

  if not DirectoryExists(ExtractFilePath(Audio.NomeArquivo)) then
     CreateDir(ExtractFilePath(Audio.NomeArquivo));
end;

procedure TAudioVisualControl.Setparent(const Value: TCommonCustomForm);
begin
  if (countExibition > 0) then
     if (lyMain.Parent <> nil) then
        fechar;

  if (Value = nil) then
     Fparent := Screen.ActiveForm
  else
     Fparent := Value;
end;

procedure TAudioVisualControl.SetSomenteLeitura(const Value: Boolean);
begin
  FSomenteLeitura := Value;
end;

{$IFDEF ANDROID}
  function TAudioVisualControl.TemPermissao(Permissao: array of String): Boolean;
  Var Permissions : TJavaObjectArray<JString>;
      i: Integer;
      VersaoAndroid : String;
  begin
    VersaoAndroid := JStringToString(TJBuild_VERSION.JavaClass.RELEASE);
    if (StrToInt(Copy(VersaoAndroid, 0, 1)) >= 6) then {SE FOR ANDROID 6 OU SUPERIOR, SOLICITA AS PERMISSÕES, SENÃO ACESSA DIRETAMENTE}
    begin
       Permissions := TJavaObjectArray<JString>.Create(length(Permissao));
       for i := 0 to length(Permissao) -1 do
       begin
           Result := TAndroidHelper.Activity.checkSelfPermission(StringToJString(Permissao[i])) = TJPackageManager.JavaClass.PERMISSION_GRANTED;
           if not (Result) then {SE UMA DAS PERMISSÕES DO ARRAY JÁ NÃO TEM MAIS PERMISSÃO, CAI FORA E RETORNA FALSE}
              break;
       end;
    end else
       Result := true;
  end;
{$ENDIF}

end.

