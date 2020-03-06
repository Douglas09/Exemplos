{--
  - Desenvolvedor: Douglas Colombo
  - Fone: (51) 99550-2036
  - YouTube: https://www.youtube.com/user/CHUCKNORRIS0002
  - Since: 02/03/2020
  - Plataformas: WINDOWS / ANDROID / IOS
--}

unit BitmapConvert;

interface

uses Fmx.Graphics;

type
  TBitmapConvert = class
  public
    class function toJPG(bitmap : TBitmap; fileName : String; Quality : Integer = 100) : boolean;
    class function toPNG(bitmap : TBitmap; fileName : String; Quality : Integer = 100) : boolean;
    class function toGIF(bitmap : TBitmap; fileName : String; Quality : Integer = 100) : boolean;
    class function toBMP(bitmap : TBitmap; fileName : String; Quality : Integer = 100) : boolean;
  end;

implementation

uses System.SysUtils;

{ TBitmapConvert }

class function TBitmapConvert.toBMP(bitmap: TBitmap; fileName: String; Quality : Integer): boolean;
var BmpResult: TBitmap;
    params: TBitmapCodecSaveParams;
begin
  result := false;
  if (FileExists(fileName)) then
     DeleteFile(fileName);

  if (assigned(bitmap)) and (Trim(fileName) <> '') then
  begin
     if (AnsiUpperCase(Copy(fileName, length(fileName) - 2, 3)) <> 'BMP') then
     begin
        raise Exception.Create('Desculpe, este arquivo não será BMP: '+ fileName);
        exit;
     end;

     BmpResult := TBitmap.Create;
     try
       BmpResult.Assign(bitmap);
       if ((Quality > 0) and (Quality < 100)) then
          params.Quality := Quality
       else
          params.Quality := 100;

       Try
         if not (DirectoryExists( ExtractFilePath(fileName) )) then
            ForceDirectories( ExtractFilePath(fileName) );

         BmpResult.SaveToFile(fileName, @params);
         result := true;
       Except End;
     finally
       BmpResult.Free;
     end;
  end;
end;

class function TBitmapConvert.toGIF(bitmap: TBitmap; fileName: String; Quality : Integer): boolean;
var BmpResult: TBitmap;
    params: TBitmapCodecSaveParams;
begin
  result := false;
  if (FileExists(fileName)) then
     DeleteFile(fileName);

  if (assigned(bitmap)) and (Trim(fileName) <> '') then
  begin
     if (AnsiUpperCase(Copy(fileName, length(fileName) - 2, 3)) <> 'GIF') then
     begin
        raise Exception.Create('Desculpe, este arquivo não será GIF: '+ fileName);
        exit;
     end;

     BmpResult := TBitmap.Create;
     try
       BmpResult.Assign(bitmap);
       if ((Quality > 0) and (Quality < 100)) then
          params.Quality := Quality
       else
          params.Quality := 100;

       Try
         if not (DirectoryExists( ExtractFilePath(fileName) )) then
            ForceDirectories( ExtractFilePath(fileName) );

         BmpResult.SaveToFile(fileName, @params);
         result := true;
       Except End;
     finally
       BmpResult.Free;
     end;
  end;
end;

class function TBitmapConvert.toJPG(bitmap: TBitmap; fileName: String; Quality : Integer): boolean;
var BmpResult: TBitmap;
    params: TBitmapCodecSaveParams;
begin
  result := false;
  if (FileExists(fileName)) then
     DeleteFile(fileName);

  if (assigned(bitmap)) and (Trim(fileName) <> '') then
  begin
     if (AnsiUpperCase(Copy(fileName, length(fileName) - 2, 3)) <> 'JPG') then
     begin
        raise Exception.Create('Desculpe, este arquivo não será JPG: '+ fileName);
        exit;
     end;

     BmpResult := TBitmap.Create;
     try
       BmpResult.Assign(bitmap);
       if ((Quality > 0) and (Quality < 100)) then
          params.Quality := Quality
       else
          params.Quality := 100;

       Try
         if not (DirectoryExists( ExtractFilePath(fileName) )) then
            ForceDirectories( ExtractFilePath(fileName) );

         BmpResult.SaveToFile(fileName, @params);
         result := true;
       Except End;
     finally
       BmpResult.Free;
     end;
  end;
end;

class function TBitmapConvert.toPNG(bitmap: TBitmap; fileName: String; Quality : Integer): boolean;
var BmpResult: TBitmap;
    params: TBitmapCodecSaveParams;
begin
  result := false;
  if (FileExists(fileName)) then
     DeleteFile(fileName);

  if (assigned(bitmap)) and (Trim(fileName) <> '') then
  begin
     if (AnsiUpperCase(Copy(fileName, length(fileName) - 2, 3)) <> 'PNG') then
     begin
        raise Exception.Create('Desculpe, este arquivo não será PNG: '+ fileName);
        exit;
     end;

     BmpResult := TBitmap.Create;
     try
       BmpResult.Assign(bitmap);
       if ((Quality > 0) and (Quality < 100)) then
          params.Quality := Quality
       else
          params.Quality := 100;

       Try
         if not (DirectoryExists( ExtractFilePath(fileName) )) then
            ForceDirectories( ExtractFilePath(fileName) );

         BmpResult.SaveToFile(fileName, @params);
         result := true;
       Except End;
     finally
       BmpResult.Free;
     end;
  end;
end;

end.
