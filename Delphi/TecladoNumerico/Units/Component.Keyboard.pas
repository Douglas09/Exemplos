unit Component.Keyboard;

interface

uses
    Component.KeyBoard.Objects,
    System.SysUtils, System.UITypes, System.Classes,
    FMX.Objects,FMX.Forms, FMX.Types;

Type
  TTypeFormat = TTecTypeFormat;
  TTeclado = class(TObject)
  private
    Layout : TTecLayout;
  public
    constructor Create(pPai: TCommonCustomForm);
    procedure Close;
    procedure Open(pComponent : TObject; pFormat : TTypeFormat; pDescription : String = ''; After : TProc = nil; Before : TProc = nil);
    procedure DisposeOf;
  published
    function Parent : TCommonCustomForm;
    procedure SetCasasDecimais(Value : Integer = 2);
  end;

implementation

{ TTeclado }

procedure TTeclado.Close;
begin
  if (assigned(Layout)) then
     Layout.Close;
end;

constructor TTeclado.Create(pPai: TCommonCustomForm);
begin
  Layout := TTecLayout.Create(pPai);
end;

procedure TTeclado.DisposeOf;
begin
  if (Assigned(Layout)) then
  begin
     Try Layout.DisposeOf; Except end;
     Layout := nil;
  end;
end;

procedure TTeclado.Open(pComponent: TObject; pFormat: TTypeFormat; pDescription : String = ''; After : TProc = nil; Before : TProc = nil);
begin
  if (assigned(Before)) then //ANTES DE ABRIR O TECLADO
     Before;

  if (assigned(Layout)) then
  begin
     Try
        if (Screen.ActiveForm <> nil) then
           Screen.ActiveForm.Focused := nil;
     Except end;

     Layout.Format := pFormat;
     Layout.Open(pComponent, pDescription, After);
  end else begin
     if (assigned(After)) then //DEPOIS DE ABRIR O TECLADO (SE ENTRAR AQUI O TECLADO NÃO FOI EXIBIDO)
        After;
  end;
end;

function TTeclado.Parent: TCommonCustomForm;
begin
  result := nil;
  Try
    if (Assigned(Layout)) and (Layout.Owner.Owner <> nil) then
       result := Layout.Owner.Owner;
  Except End;
end;

procedure TTeclado.SetCasasDecimais(Value: Integer);
begin
  Layout.CasasDecimais := Value;
end;

end.
