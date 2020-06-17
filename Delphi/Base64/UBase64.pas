{** Douglas Colombo
  * Classe de conversão de arquivos em texto
  * e de Texto em Arquivos, BASE64
**}

unit UBase64;

interface

uses System.Classes, System.netEncoding, System.SysUtils, FMX.Graphics;


Type
   tBase64 = class

   private
     procedure Toast(const Msg: string; Duration: Integer);
   public
     procedure Base64ToFile(Arquivo, caminhoSalvar : String);
     function Base64ToStream(imagem : String) : TMemoryStream;
     function Base64ToBitmap(imagem : String) : TBitmap;
     function BitmapToBase64(imagem : TBitmap) : String;
     function FileToBase64(Arquivo : String) : String;
     function StreamToBase64(STream : TMemoryStream) : String;
end;

implementation

{$IFDEF ANDROID}
  Uses
    Androidapi.Helpers, FMX.Helpers.Android, Androidapi.Jni.Widget;
{$ENDIF}


{ tBase64 }
function tBase64.Base64ToBitmap(imagem: String): TBitmap;
Var sTream : TMemoryStream;
begin
  if (trim(imagem) <> '') then
  begin
     Try
        sTream := Base64ToStream(imagem);
        result := TBitmap.CreateFromStream(sTream);
     Finally
        sTream.DisposeOf;
        sTream := nil;
     End;
  end else
     Toast('Base64ToBitmap: Sem conteúdo...', 8);
end;

procedure tBase64.Base64ToFile(Arquivo, caminhoSalvar : String);
Var sTream : TMemoryStream;
begin
  Try
    sTream := Base64ToStream(Arquivo);
    sTream.SaveToFile(caminhoSalvar);
  Finally
    sTream.free;
    sTream:=nil;
  End;
end;

function tBase64.Base64ToStream(imagem: String): TMemoryStream;
var Base64 : TBase64Encoding;
    bytes : tBytes;
begin
  Try
    Base64 := TBase64Encoding.Create;
    bytes  := Base64.DecodeStringToBytes(imagem);
    result := TBytesStream.Create(bytes);
    result.Position := 0; {ANDROID 64 e 32 Bits}
//    result.Seek(0, 0); {ANDROID 32 Bits}
  Finally
    Base64.Free;
    Base64:=nil;
    SetLength(bytes, 0);
  End;
end;

function tBase64.BitmapToBase64(imagem: TBitmap): String;
Var sTream : TMemoryStream;
begin
  result := '';

  if not (imagem.IsEmpty) then
  begin
     Try
        sTream := TMemoryStream.Create;
        imagem.SaveToStream(sTream);
        result := StreamToBase64(sTream);
        sTream.DisposeOf;
        sTream := nil;
     Except End;
  end;
end;

function tBase64.FileToBase64(Arquivo : String): String;
Var sTream : tMemoryStream;
begin
  if (Trim(Arquivo) <> '') then
  begin
     sTream := TMemoryStream.Create;
     Try
       sTream.LoadFromFile(Arquivo);
       result := StreamToBase64(sTream);
     Finally
       Stream.Free;
       Stream:=nil;
     End;
  end else
     result := '';
end;

function tBase64.StreamToBase64(STream: TMemoryStream): String;
Var Base64 : tBase64Encoding;
begin
  Try
    Stream.Position := 0; {ANDROID 64 e 32 Bits}
//    Stream.Seek(0, 0); {ANDROID 32 Bits}
    Base64 := TBase64Encoding.Create;
    Result := Base64.EncodeBytesToString(sTream.Memory, sTream.Size);
  Finally
    Base64.Free;
    Base64:=nil;
  End;
end;

procedure tBase64.Toast(const Msg: string; Duration: Integer);
begin
  {$IFDEF ANDROID}
    CallInUiThread(
      procedure
      begin
        TJToast.JavaClass.makeText(
          TAndroidHelper.Context,
          StrToJCharSequence(msg),
          Duration).show
      end);
   {$ENDIF}
end;

end.
