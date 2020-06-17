unit Component.Keyboard.Functions;

interface
uses System.Math, System.Classes, System.netEncoding, System.SysUtils, System.StrUtils,
     FMX.Graphics;

function retornaCaractereFormatado(Value: String): String;
function SomenteNumeros(const s: string): string;
function PreencheValor(value : String; QtdeCaracteres : Integer) : String;

function FormataValor(VALOR: string; casasDecimais : Integer = 2): string;
function FormataCPF(CPF: string): string;
function FormataCNPJ(CNPJ: string): string;
function FormataCelular(Celular: String): String;
function FormataTelefone(Telefone: String): String;
function FormataData(Data: String): String;
function FormataCep(Cep : String): String;


function CELULARValido(CELULAR: string): Boolean;
function TELEFONEValido(TELEFONE: string): Boolean;
function EMAILValido(EMAIL: string): Boolean;
function DATAValida(DATA: string): Boolean;
function CNPJValido(CNPJ: string) : Boolean;
function CPFValido(CPF: string): boolean;
function CEPValido(CEP : string): Boolean;
function SENHAValida(Senha, SenhaRepetida : String) : String;


function Base64ToStream(imagem: String): TMemoryStream;
function Base64ToBitmap(imagem: String): TBitmap;
function StreamToBase64(STream: TMemoryStream): String;
function IcoOK : TBitmap;
function IcoCancel : TBitmap;
function IcoBackSpace : TBitmap;

implementation

function Base64ToStream(imagem: String): TMemoryStream;
var Base64 : TBase64Encoding;
    bytes : tBytes;
begin
  Try
    Base64 := TBase64Encoding.Create;
    bytes  := Base64.DecodeStringToBytes(imagem);
    result := TBytesStream.Create(bytes);
    result.Position := 0; {ANDROID 64 BITS}
    //result.Seek(0, 0); {ANDROID 32 BITS SOMENTE}
  Finally
    Base64.Free;
    Base64:=nil;
    SetLength(bytes, 0);
  End;
end;

function Base64ToBitmap(imagem: String): TBitmap;
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
  end;
end;

function StreamToBase64(STream: TMemoryStream): String;
Var Base64 : tBase64Encoding;
begin
  Try
    Stream.Position := 0; {ANDROID 64 BITS}
    //result.Seek(0, 0); {ANDROID 32 BITS SOMENTE}
    Base64 := TBase64Encoding.Create;
    Result := Base64.EncodeBytesToString(sTream.Memory, sTream.Size);
  Finally
    Base64.Free;
    Base64:=nil;
  End;
end;

function IcoOK : TBitmap;
Var iconOk : String;
begin
  iconOk := 'iVBORw0KGgoAAAANSUhEUgAAACQAAAAkCAYAAADhAJiYAAABVUlEQVR4AezSEZACYRjH4YWD4ODw' +
            'fIIwODy4mfMJwyCfMFzIJzzMJ8g5CA7ChSAIwjAInpOoZr57275N7n4+7z6z37/470+ELkossMIW' +
            'WywxxxjtphFPGGEnXoVBE5geNuq3xnsuTClPJwzvfaK5/E3rgiaaa3grpq/ZTuFNoYW9vC3xdnF3' +
            'HR9xfszz+XbnAjWI/J1Ddsw5vKICQJUG8flADEA7Bfp6MAbGKdD3gzEwT4F2rtvjA1UDGFimQFxj' +
            'OsHjdTCwjYOo8Br4SF0M7FKgfRyVBQOrFGgNUVQGDCxSoBlEURkwUKZAPYiiMmCgmwI94xhFZcDs' +
            'it/CFCKoOzEwioBecIig7sRs8FREwkjz9YpbwkxzlcWtoYWl/M1DgARqJl+T8G4Cmzqo3x79Imd4' +
            'wRRH8Q4o0SqaCj/jAeUQaFSeR6uQHwPxaWjL0xnskOEORgEAcPG2Z1a1IiQAAAAASUVORK5CYII=';
  result := Base64ToBitmap(iconOk);
end;

function IcoCancel : TBitmap;
Var iconCancel : String;
begin
  iconCancel := 'iVBORw0KGgoAAAANSUhEUgAAACQAAAAkCAYAAADhAJiYAAABZElEQVR4Ae3XIUwCYRjHYYKBQLQn' +
                '++yz2UcwGI3G6zMYCEb6iEQCgZ4IBMJFAoFAIBIJhMeIgf13Hh9Feeptt9/ue9/dvs6/cINHDDDF' +
                'AhtsMMcEH3i4dkQXFbaaW+ENd6Vj+lhrr8ZTqZiBMo54vyTkDhPlDdsGfbme99/GvLquY+OZQg+7' +
                'OKDN1flZg+3DJ/n8MQRZ1WAO33IMXeydN+uc5Ciqn8sRvtQqB/Esn/tLijrFnKCSPaSgkSxEhZjs' +
                'IwUtBSkqxmSTFLQVpKgYk81DUJCjcky2aR0UBjgPerZNQbs2MRdGLVJQLQvbFLYvm6agccuYtH1k' +
                'gxTUbxnjgqjH/GPlED5tiIlRdRjojPyPOhMToxq8L8M99mkINXPELDxfo5tr0nGU188VaePKG7S9' +
                'h82VN7n0cjhWzleRSyMq7LW3w2unJNxjiIPm9vhEr3Mt6KGPMWrsnGyxxAjPYa3/jptvX9ZEe1SH' +
                'Di4AAAAASUVORK5CYII=';
  result := Base64ToBitmap(iconCancel);
end;

function IcoBackSpace : TBitmap;
Var iconBackSpace : String;
begin
  iconBackSpace := 'iVBORw0KGgoAAAANSUhEUgAAACQAAAAkCAYAAADhAJiYAAABCElEQVR4Ae2WgQXDQBhGg4xRFB2g' +
                   '6B5FB+gAxY3QAYoOEgQZIAMUHSUooHgFoYJ8vuTyB/I44PDcy5+7YiOCDeAIJOC+0Do5Mk9ieAF7' +
                   'JXMllmpMZgd0xNKNCTWsgE4VzNxUbwTu3jmpnsYUJqAEKgRTUzXGryH97SvVSU1N9QUuWoo02JMQ' +
                   '6FSmlJCRWKkMKVumR6fCl5IyI+hUppSW0UIteUg5LuYC2AOfnDJaSn/UNxz0aF+0lB77NoeMmD5D' +
                   'SKQzZNBSWshNVwuZMSl9dfSY6dJQRknZJ+Sno0bTSzVzHmg34tBCOl28kE4X/chfKd2jUAAVMXTA' +
                   'QQtBCZyB+4IrAbtiYyMDP8mcgcHTm26MAAAAAElFTkSuQmCC';
  result := Base64ToBitmap(iconBackSpace);
end;

function CELULARValido(CELULAR: string): Boolean;
begin
  result := false;
  if (Trim(CELULAR) <> '') then
  begin
      CELULAR := SomenteNumeros(Trim(CELULAR));
      result := ((length(CELULAR) = 10) or (length(CELULAR) = 11));
  end;
end;

function TELEFONEValido(TELEFONE: string): Boolean;
begin
  result := false;
  if (Trim(TELEFONE) <> '') then
  begin
      TELEFONE := SomenteNumeros(Trim(TELEFONE));
      result := (length(TELEFONE) = 10);
      if (result) then
      begin

      end;
  end;
end;

function SomenteNumeros(const s: string): string;
var i: integer;
begin
  result := '';
  if (Trim(s) <> '') then
  begin
     for i := 0 to length(s) do
       if pos(s[i], '0123456789') > 0 then
         result := result + s[i];
  end;
end;

function EMAILValido(EMAIL: string): Boolean;
begin
  result := false;
  EMAIL := Trim(UpperCase(EMAIL));
  if (length(EMAIL) >= 10) then
     if (Pos('@', EMAIL) > 0) and (Pos('.', EMAIL) > 0) then
        if not (EMAIL.Contains(' ')) then
           Result := True;
end;

function DATAValida(DATA: string): Boolean;
Var settingsDate : TFormatSettings;
begin
  result := false;
  if ((length(DATA) = 6) or (length(DATA) = 8) or (length(DATA) = 10)) then { 140797 - 14/07/97 - 14/07/1997 }
  begin
     DATA := SomenteNumeros(DATA);
     if (trim(DATA) <> '') then
     begin
        settingsDate := TFormatSettings.Create;
        DATA := Copy(DATA, 1, 2) + settingsDate.DateSeparator + Copy(DATA, 3, 2) + settingsDate.DateSeparator + Copy(DATA, 5, length(DATA));
     end;

     Try
        strToDate(DATA);
        result := true;
     Except End;
  end;
end;

function CNPJValido(CNPJ: string): Boolean;
var dg1, dg2: integer;
    x, total: integer;
    ret: boolean;
begin
  result := false;
  if (Trim(CNPJ) <> '') then
  begin
      CNPJ := SomenteNumeros(Trim(CNPJ));
      ret  := (length(CNPJ) = 14);

      //Verifica
      if (ret) then
      begin
        try
          //1° digito
          total:=0;
          for x:=1 to 12 do
          begin
            if x < 5 then
              Inc(total, StrToInt(Copy(cnpj, x, 1)) * (6 - x))
            else
              Inc(total, StrToInt(Copy(cnpj, x, 1)) * (14 - x));
          end;
          dg1:=11 - (total mod 11);
          if dg1 > 9 then
            dg1:=0;
          //2° digito
          total:=0;
          for x:=1 to 13 do
          begin
            if x < 6 then
              Inc(total, StrToInt(Copy(cnpj, x, 1)) * (7 - x))
            else
              Inc(total, StrToInt(Copy(cnpj, x, 1)) * (15 - x));
          end;
          dg2:=11 - (total mod 11);
          if dg2 > 9 then
            dg2:=0;
          //Validação final
          if (dg1 = StrToInt(Copy(cnpj, 13, 1))) and (dg2 = StrToInt(Copy(cnpj, 14, 1))) then
            ret:=True
          else
            ret:=False;
        except
          ret:=False;
        end;
        //Inválidos
        case AnsiIndexStr(cnpj,['00000000000000', '11111111111111', '22222222222222', '33333333333333',
                                '44444444444444', '55555555555555', '66666666666666', '77777777777777',
                                '88888888888888', '99999999999999']) of
           0..9: ret:=False;
        end;
      end;
      Result := ret;
  end;
end;

function CPFValido(CPF: string): boolean;
var n1,n2,n3,n4,n5,n6,n7,n8,n9, n10, n11, d1,d2: integer;
    digitado, calculado: string;
    vResultBOL : Boolean;
begin
    vResultBOL := false;
    if (trim(CPF) <> '') then
    begin
       CPF := SomenteNumeros(CPF);
       if (length(CPF) = 11)  then
       begin
          {040.051.350-13}
          {$IFDEF MSWINDOWS}
            n1  := StrToInt(Copy(CPF, 1, 1)); //0
            n2  := StrToInt(Copy(CPF, 2, 1)); //4
            n3  := StrToInt(Copy(CPF, 3, 1)); //0
            n4  := StrToInt(Copy(CPF, 4, 1)); //0
            n5  := StrToInt(Copy(CPF, 5, 1)); //5
            n6  := StrToInt(Copy(CPF, 6, 1)); //1
            n7  := StrToInt(Copy(CPF, 7, 1)); //3
            n8  := StrToInt(Copy(CPF, 8, 1)); //5
            n9  := StrToInt(Copy(CPF, 9, 1)); //0
            n10 := StrToInt(Copy(CPF, 10, 1)); //1
            n11 := StrToInt(Copy(CPF, 11, 1));//3
          {$ELSE}
            n1  := StrToInt(CPF[0]); //0
            n2  := StrToInt(CPF[1]); //4
            n3  := StrToInt(CPF[2]); //0
            n4  := StrToInt(CPF[3]); //0
            n5  := StrToInt(CPF[4]); //5
            n6  := StrToInt(CPF[5]); //1
            n7  := StrToInt(CPF[6]); //3
            n8  := StrToInt(CPF[7]); //5
            n9  := StrToInt(CPF[8]); //0
            n10 := StrToInt(CPF[9]); //1
            n11 := StrToInt(CPF[10]);//3
          {$ENDIF}

          d1:=n9*2+n8*3+n7*4+n6*5+n5*6+n4*7+n3*8+n2*9+n1*10;
          d1:=11-(d1 mod 11);
          if (d1>=10) then
             d1:=0;
          d2:=d1*2+n9*3+n8*4+n7*5+n6*6+n5*7+n4*8+n3*9+n2*10+n1*11;
          d2:=11-(d2 mod 11);
          if d2 >= 10 then
             d2 := 0;

          calculado := inttostr(d1) + inttostr(d2);
          digitado := inttostr(n10) + inttostr(n11);
          if calculado = digitado then
             vResultBOL := true
          else
             vResultBOL := false;
       end;
    end;

    if (vResultBOL) then
    begin
      case AnsiIndexStr(CPF,['00000000000', '11111111111', '22222222222', '33333333333',
                             '44444444444', '55555555555', '66666666666', '77777777777',
                             '88888888888', '99999999999']) of
         0..9: vResultBOL := False;
      end;
    end;

    Result := vResultBOL;
end;

function retornaCaractereFormatado(Value: String): String;
begin
  if (Trim(Value) <> '') then
     result := Value
  else
     result := '_';
end;

function FormataValor(VALOR: string; casasDecimais: Integer): string;
Var i : Integer;
    temp : String;
begin
  result := SomenteNumeros(Trim(VALOR));
  if (trim(result) = '') then
     result := '0';

  result := FloatToStr(strToFloat(result));
  while (length(trim(result)) <= casasDecimais) do
     result := '0' + result;

  if (Trim(VALOR) <> '') then
  begin
     temp   := Copy(result, 1, (length(result) - casasDecimais)) + ',';
     temp   := temp + Copy(result, (length(result) - casasDecimais) + 1, length(result));
  end else begin
     temp := '0,';
     i := 0;
     while (i < casasDecimais) do
     begin
       temp := temp + '0';
       i := i + 1;
     end;
  end;

  result := temp;
  if (Copy(result, 1, 1) = ',') then
     result := '0' + result;

  temp := Copy(result, length(result), 1);
  if (temp = ',') then
     result := Copy(result, 1, length(result) - 1);
end;

function FormataCPF(CPF: string): string;
Var Temp : String;
begin
  Temp := SomenteNumeros(Trim(CPF));

  result := result + retornaCaractereFormatado(Trim(Copy(Temp, 1, 1)));   {0}
  result := result + retornaCaractereFormatado(Trim(Copy(Temp, 2, 1)));   {4}
  result := result + retornaCaractereFormatado(Trim(Copy(Temp, 3, 1)));   {0}
  result := result + '.';                                                 {.}
  result := result + retornaCaractereFormatado(Trim(Copy(Temp, 4, 1)));   {0}
  result := result + retornaCaractereFormatado(Trim(Copy(Temp, 5, 1)));   {5}
  result := result + retornaCaractereFormatado(Trim(Copy(Temp, 6, 1)));   {1}
  result := result + '.';                                                 {.}
  result := result + retornaCaractereFormatado(Trim(Copy(Temp, 7, 1)));   {3}
  result := result + retornaCaractereFormatado(Trim(Copy(Temp, 8, 1)));   {5}
  result := result + retornaCaractereFormatado(Trim(Copy(Temp, 9, 1)));   {0}
  result := result + '-';                                                 {-}
  result := result + retornaCaractereFormatado(Trim(Copy(Temp, 10, 1)));  {1}
  result := result + retornaCaractereFormatado(Trim(Copy(Temp, 11, 1)));  {3}
end;

function FormataCNPJ(CNPJ: string): string;
Var Temp : String;
begin
  Temp := SomenteNumeros(Trim(CNPJ));

  result := result + retornaCaractereFormatado(Trim(Copy(Temp, 1, 1)));   {1}
  result := result + retornaCaractereFormatado(Trim(Copy(Temp, 2, 1)));   {8}
  result := result + '.';                                                 {.}
  result := result + retornaCaractereFormatado(Trim(Copy(Temp, 3, 1)));   {8}
  result := result + retornaCaractereFormatado(Trim(Copy(Temp, 4, 1)));   {3}
  result := result + retornaCaractereFormatado(Trim(Copy(Temp, 5, 1)));   {2}
  result := result + '.';                                                 {.}
  result := result + retornaCaractereFormatado(Trim(Copy(Temp, 6, 1)));   {9}
  result := result + retornaCaractereFormatado(Trim(Copy(Temp, 7, 1)));   {2}
  result := result + retornaCaractereFormatado(Trim(Copy(Temp, 8, 1)));   {1}
  result := result + '/';                                                 {/}
  result := result + retornaCaractereFormatado(Trim(Copy(Temp, 9, 1)));   {0}
  result := result + retornaCaractereFormatado(Trim(Copy(Temp, 10, 1)));  {0}
  result := result + retornaCaractereFormatado(Trim(Copy(Temp, 11, 1)));  {0}
  result := result + retornaCaractereFormatado(Trim(Copy(Temp, 12, 1)));  {1}
  result := result + '-';                                                 {-}
  result := result + retornaCaractereFormatado(Trim(Copy(Temp, 13, 1)));  {8}
  result := result + retornaCaractereFormatado(Trim(Copy(Temp, 14, 1)));  {6}
end;

function FormataCelular(Celular: String): String;
Var Temp : String;
begin
  Temp := SomenteNumeros(Trim(Celular));

  result := '(';                                                            {(}
  result := result + retornaCaractereFormatado(Trim(Copy(Temp, 1, 1)));     {5}
  result := result + retornaCaractereFormatado(Trim(Copy(Temp, 2, 1)));     {1}
  result := result + ') ';                                                  {)}
  if (length(Temp) <= 10) then
  begin
     result := result + retornaCaractereFormatado(Trim(Copy(Temp, 3, 1)));  {9}
     result := result + retornaCaractereFormatado(Trim(Copy(Temp, 4, 1)));  {5}
     result := result + retornaCaractereFormatado(Trim(Copy(Temp, 5, 1)));  {5}
     result := result + retornaCaractereFormatado(Trim(Copy(Temp, 6, 1)));  {0}
     result := result + '-';                                                {-}
     result := result + retornaCaractereFormatado(Trim(Copy(Temp, 7, 1)));  {2}
     result := result + retornaCaractereFormatado(Trim(Copy(Temp, 8, 1)));  {6}
     result := result + retornaCaractereFormatado(Trim(Copy(Temp, 9, 1)));  {3}
     result := result + retornaCaractereFormatado(Trim(Copy(Temp, 10, 1))); {6}
  end else if (length(Celular) = 11) then
  begin
     result := result + retornaCaractereFormatado(Trim(Copy(Temp, 3, 1)));  {9}
     result := result + retornaCaractereFormatado(Trim(Copy(Temp, 4, 1)));  {9}
     result := result + retornaCaractereFormatado(Trim(Copy(Temp, 5, 1)));  {5}
     result := result + retornaCaractereFormatado(Trim(Copy(Temp, 6, 1)));  {5}
     result := result + retornaCaractereFormatado(Trim(Copy(Temp, 7, 1)));  {0}
     result := result + '-';                                                {-}
     result := result + retornaCaractereFormatado(Trim(Copy(Temp, 8, 1)));  {2}
     result := result + retornaCaractereFormatado(Trim(Copy(Temp, 9, 1)));  {6}
     result := result + retornaCaractereFormatado(Trim(Copy(Temp, 10, 1))); {3}
     result := result + retornaCaractereFormatado(Trim(Copy(Temp, 11, 1))); {6}
  end;
end;

function FormataTelefone(Telefone: String): String;
Var Temp : String;
begin
  Temp := SomenteNumeros(Trim(Telefone));

  if (length(Temp) <= 10) then
  begin
     result := '(';                                                         {(}
     result := result + retornaCaractereFormatado(Trim(Copy(Temp, 1, 1)));  {5}
     result := result + retornaCaractereFormatado(Trim(Copy(Temp, 2, 1)));  {1}
     result := result + ') ';                                               {)}
     result := result + retornaCaractereFormatado(Trim(Copy(Temp, 3, 1)));  {3}
     result := result + retornaCaractereFormatado(Trim(Copy(Temp, 4, 1)));  {7}
     result := result + retornaCaractereFormatado(Trim(Copy(Temp, 5, 1)));  {5}
     result := result + retornaCaractereFormatado(Trim(Copy(Temp, 6, 1)));  {6}
     result := result + '-';                                                {-}
     result := result + retornaCaractereFormatado(Trim(Copy(Temp, 7, 1)));  {1}
     result := result + retornaCaractereFormatado(Trim(Copy(Temp, 8, 1)));  {2}
     result := result + retornaCaractereFormatado(Trim(Copy(Temp, 9, 1)));  {7}
     result := result + retornaCaractereFormatado(Trim(Copy(Temp, 10, 1))); {3}
  end else
     result := Telefone;
end;

function FormataData(Data: String): String;
Var Temp : String;
begin
  Temp := SomenteNumeros(Trim(Data));

  if (length(Temp) = 6) then
  begin
     result := result + retornaCaractereFormatado(Trim(Copy(Temp, 1, 1))); {1}
     result := result + retornaCaractereFormatado(Trim(Copy(Temp, 2, 1))); {4}
     result := result + '/';
     result := result + retornaCaractereFormatado(Trim(Copy(Temp, 3, 1))); {0}
     result := result + retornaCaractereFormatado(Trim(Copy(Temp, 4, 1))); {7}
     result := result + '/';
     result := result + retornaCaractereFormatado(Trim(Copy(Temp, 5, 1))); {9}
     result := result + retornaCaractereFormatado(Trim(Copy(Temp, 6, 1))); {7}
  end else if (length(Temp) <= 8) then
  begin
     result := result + retornaCaractereFormatado(Trim(Copy(Temp, 1, 1))); {1}
     result := result + retornaCaractereFormatado(Trim(Copy(Temp, 2, 1))); {4}
     result := result + '/';
     result := result + retornaCaractereFormatado(Trim(Copy(Temp, 3, 1))); {0}
     result := result + retornaCaractereFormatado(Trim(Copy(Temp, 4, 1))); {7}
     result := result + '/';
     result := result + retornaCaractereFormatado(Trim(Copy(Temp, 5, 1))); {1}
     result := result + retornaCaractereFormatado(Trim(Copy(Temp, 6, 1))); {9}
     result := result + retornaCaractereFormatado(Trim(Copy(Temp, 7, 1))); {9}
     result := result + retornaCaractereFormatado(Trim(Copy(Temp, 8, 1))); {7}
  end else
     result := Data;
end;

function FormataCep(Cep : String): String;
Var Temp : String;
begin
  Temp   := SomenteNumeros(Cep);
  result := '';
  result := result + retornaCaractereFormatado(Trim(Copy(Temp, 1, 1)));
  result := result + retornaCaractereFormatado(Trim(Copy(Temp, 2, 1))) +'.';
  result := result + retornaCaractereFormatado(Trim(Copy(Temp, 3, 1)));
  result := result + retornaCaractereFormatado(Trim(Copy(Temp, 4, 1)));
  result := result + retornaCaractereFormatado(Trim(Copy(Temp, 5, 1))) +'-';
  result := result + retornaCaractereFormatado(Trim(Copy(Temp, 6, 1)));
  result := result + retornaCaractereFormatado(Trim(Copy(Temp, 7, 1)));
  result := result + retornaCaractereFormatado(Trim(Copy(Temp, 8, 1)));
end;

function CEPValido(CEP : string): Boolean;
begin
  result := false;
  if (trim(CEP) <> '') then
  begin
     CEP := SomenteNumeros(CEP);
     result := (Length(CEP) = 8);
  end;
end;

function PreencheValor(value : String; QtdeCaracteres : Integer) : String;
begin
   result := value;
   if (Length(result) > QtdeCaracteres) then
      result := Copy(result, 1, QtdeCaracteres);
   while (Length(result) < QtdeCaracteres) do
      result := result + '0';
end;

function SENHAValida(Senha, SenhaRepetida: String): String; {IMPLEMENTAR SENHA COM NÍVEIS (FRACO, MEDIO, FORTE)}
Var temporario : String;
    indice : Integer;
    temMinuscula, temMaiuscula : Boolean;
Const minusculo : String = 'abcdefghijklmnopqrstwxyz';
      maiusculo : String = 'ABCDEFGHIJKLMNOPQRSTWXYZ';
begin
  { SE A SENHA POSSUIR >= 6 CARACTERES
  + SE A SENHA FOR IGUAL A SENHA_REPETIDA
  + SE A SENHA POSSUIR NÚMEROS
  + SE A SENHA POSSUIR MAIÚSCULO E MINUSCULO
  = SENHA LIBERADA! }
  result := '';
  if (Length(Senha) >= 6) then
  begin
     if (Senha = SenhaRepetida) then
     begin
      { senha só será bloqueada se for menor que 6 caracteres }

     end else
        result := 'Desculpe, a senha informada não corresponde a senha repetida.';
  end else
     result := 'Desculpe, senha inválida.' +sLineBreak+ 'Número mínimo de caracteres (6 caracteres).';
end;

end.
