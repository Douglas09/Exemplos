unit UAudioControl;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.Media, FMX.StdCtrls,
  FMX.Objects, System.Actions, FMX.ActnList, FMX.Controls.Presentation, FMX.Effects, System.IOUtils


  {$IFDEF ANDROID}
    , Androidapi.Helpers, FMX.Helpers.Android, Androidapi.Jni.Widget, Androidapi.IOUtils
  {$ENDIF}
  ;


{$IFDEF ANDROID}
     const nomeArquivoTemporario : String = 'AudioTemp.caf';
{$ENDIF} {$IFDEF IOS}
     const nomeArquivoTemporario : String = 'AudioTemp.caf';
{$ENDIF} {$IFDEF MSWINDOWS}
     const nomeArquivoTemporario : String = 'AudioTemp.wav';
{$ENDIF}

Type
   TAudioPlayer = class
   private
     FPai : TCommonCustomForm;
     lbTimeTemp    : TLabel;

     fProgressBar : TProgressBar;
     
     FMicrophone: TAudioCaptureDevice;
     FMediaPlayer : TMediaPlayer;
     FNomeArquivo: String;

     procedure changedExecuteControls;
     procedure changedCaptureLabel;
     function IsMicrophoneRecording: Boolean;
     function HasMicrophone: Boolean;
     procedure SetNomeArquivo(const Value: String);
   public
     property NomeArquivo : String read FNomeArquivo write SetNomeArquivo;
     procedure StartCapture(tempoGravacao : TLabel);
     function StopCapture : Boolean;


     procedure Play(progresso : TProgressBar);
     procedure Stop;

     Constructor create(pai : TCommonCustomForm = nil); overload;
     Destructor Destroy; overload;
   end;

implementation

function GetAudioFileName(const AFileName: string): string;
begin
  {$IFDEF ANDROID}
     result := GetSharedDownloadsDir +PathDelim+ AFileName;
  {$ENDIF} {$IFDEF IOS}
     result := TPath.GetDocumentsPath + PathDelim+ AFileName;
  {$ENDIF} {$IFDEF MSWINDOWS}
     result := 'C:\' + AFileName;
  {$ENDIF}
end;

{ TAudioPlayer }
procedure TAudioPlayer.changedExecuteControls;
Var thr, thr2 : TThread;
begin
   thr := TThread.CreateAnonymousThread(procedure
       var pMax, pAtu : TMediaTime;
       begin
          thr.FreeOnTerminate := true;
          if (fProgressBar <> nil) then
          begin
             pMax               := FMediaPlayer.Media.Duration;
             fProgressBar.Min   := 0;
             fProgressBar.Max   := pMax;
             while (FMediaPlayer.Media.State = TMediaState.Playing) do
             begin
                Try
                  Sleep(10);
                  pAtu := FMediaPlayer.Media.CurrentTime;
                  if (assigned(fProgressBar)) then
                     TThread.Queue(nil, procedure begin fProgressBar.Value := pAtu; end);
                Except end;

                Sleep(10);
             end;
          end;
       end);
   thr.Start;
end;

constructor TAudioPlayer.create(pai : TCommonCustomForm = nil);
begin
  if (pai <> nil) then
     FPai := pai
  else
     FPai := Screen.ActiveForm;

  FMediaPlayer := TMediaPlayer.Create(FPai);
  FMicrophone := TCaptureDeviceManager.Current.DefaultAudioCaptureDevice;
end;

destructor TAudioPlayer.Destroy;
begin
  if (assigned(FMediaPlayer.Media)) then
     if (FMediaPlayer.Media.State = TMediaState.Playing) then
        FMediaPlayer.Media.stop;
  if (assigned(FMicrophone)) then
     if (FMicrophone.State = TCaptureDeviceState.Capturing) then
        FMicrophone.StopCapture;

  Try
    {$IFDEF MSWINDOWS}
       FMediaPlayer.free;
    {$ELSE}
       FMediaPlayer.DisposeOf;
    {$ENDIF}
    FMicrophone := nil;
  Except End;

  Try
    {$IFDEF MSWINDOWS}
       FMicrophone.free;
    {$ELSE}
       FMicrophone.DisposeOf;
    {$ENDIF}
    FMicrophone := nil;
  Except End;

  FPai          := nil;
  lbTimeTemp    := nil;
  fProgressBar  := nil;
end;

procedure TAudioPlayer.SetNomeArquivo(const Value: String);
begin
  FNomeArquivo := Value;
end;

procedure TAudioPlayer.changedCaptureLabel;
Var thr : TThread;
begin
  thr := TThread.CreateAnonymousThread(procedure
      begin
         thr.FreeOnTerminate := true;
         if (lbTimeTemp <> nil) then
         begin
            lbTimeTemp.Text := '00:00:00';
            Sleep(850);
         end;

         while (FMicrophone.State = TCaptureDeviceState.Capturing) do
         begin
            Try
              if (lbTimeTemp <> nil) then
                 TThread.Queue(nil, procedure
                         begin
                            Try
                               lbTimeTemp.Text := timeToStr(strToTime(lbTimeTemp.Text) + strToTime('00:00:01'));
                            Except end;
                         end)
              else
                 break;
            Except
              break;
            end;
            Sleep(995);
         end;
      end);
  thr.Start;
end;

function TAudioPlayer.HasMicrophone: Boolean;
begin
  Result := Assigned(FMicrophone);
end;

function TAudioPlayer.IsMicrophoneRecording: Boolean;
begin
  Result := HasMicrophone and (FMicrophone.State = TCaptureDeviceState.Capturing);
end;

procedure TAudioPlayer.Play(progresso: TProgressBar);
begin
  if (Trim(FNomeArquivo) = '') or not(FileExists(FNomeArquivo)) then
  begin
     fProgressBar  := nil;
     raise Exception.Create('Arquivo não encontrado...'+sLineBreak+FNomeArquivo);
  end else begin
     fProgressBar  := progresso;
     if IsMicrophoneRecording then
        StopCapture;

     FMediaPlayer.FileName := FNomeArquivo;
     FMediaPlayer.Media.Play;
     changedExecuteControls;
  end;
end;

procedure TAudioPlayer.StartCapture(tempoGravacao: TLabel);
begin
  if assigned(FMediaPlayer.Media) then
     if (FMediaPlayer.Media.State = TMediaState.Playing) then
        FMediaPlayer.Media.stop;
  if (FMicrophone.State = TCaptureDeviceState.Capturing) then
     FMicrophone.StopCapture;

  lbTimeTemp    := tempoGravacao;

  FMicrophone := TCaptureDeviceManager.Current.DefaultAudioCaptureDevice;
  if (HasMicrophone) then
  begin
    if (trim(NomeArquivo) = '') then //CAMINHO TEMPORÁRIO CASO USUÁRIO NÃO DEFINIR
       NomeArquivo := GetAudioFileName(nomeArquivoTemporario);
    FMicrophone.FileName := NomeArquivo;

    try
      FMicrophone.StartCapture;

      if (lbTimeTemp <> nil) then
         changedCaptureLabel;
    except on E : Exception do
      raise Exception.Create('Captura de Áudio: Operação não liberada para este dispositivo.'+sLineBreak+'Motivo: '+e.Message);
    end;
  end else
    raise Exception.Create('Desculpe, microfone inválido!');
end;

procedure TAudioPlayer.Stop;
begin
  Try
    if (FMediaPlayer <> nil) and (FMediaPlayer.Media <> nil) then
    begin
       if (FMediaPlayer.Media.State = TMediaState.Playing) then
          FMediaPlayer.Media.Stop;
    end;
  Except End;


  Try
    if (assigned(FMediaPlayer)) then
       FMediaPlayer.Media.Free;
    FMediaPlayer := TMediaPlayer.Create( FPai );
  Except end;
end;

function TAudioPlayer.StopCapture : Boolean;
begin
  result := false;
  Try
    if (IsMicrophoneRecording) then
    begin
       try
         FMicrophone.StopCapture;
         result := true;
       except on E : Exception do
         raise Exception.Create('Operação não suportada por este dispositivo.' +sLineBreak+ 'Mensagem: '+ E.Message);
       end;
    end;
  Except end;
end;

end.
