program ExemploAudio;

uses
  System.StartUpCopy,
  FMX.Forms,
  AudioPlay in 'AudioPlay.pas' {FrmPrincipal},
  UAudioControl in 'UAudioControl.pas',
  uAudioVisualControl in 'uAudioVisualControl.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFrmPrincipal, FrmPrincipal);
  Application.Run;
end.
