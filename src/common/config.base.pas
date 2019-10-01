unit config.base;

{$mode delphi}

interface

uses
  Classes,
  SysUtils,
  config.types;

type

  { TConfigImpl }
  (*
    base class for IConfig
  *)
  TConfigImpl = class(TInterfacedObject, IConfig)
  strict private
  strict protected
    function DoToString : String; virtual; abstract;
    function DoFromString(Const AFile : String; Out Error : String) : Boolean; virtual; abstract;
  protected
  public
    function SaveToFile(Const AFileName : String; Out Error : String) : Boolean;
    function LoadFromFile(Const AFileName : String; Out Error : String) : Boolean;
  end;

implementation

{ TConfigImpl }

function TConfigImpl.SaveToFile(const AFileName: String; out Error: String): Boolean;
var
  LContents: TStringList;
begin
  Result := False;

  LContents := TStringList.Create;
  try
    try
      //get the contents
      LContents.Text := DoToString;

      //save to the file
      LContents.SaveToFile(AFileName);

      //success
      Result := True;
    except on E : Exception do
      Error := E.Message;
    end;
  finally
    LContents.Free;
  end;
end;

function TConfigImpl.LoadFromFile(const AFileName: String; out Error: String): Boolean;
var
  LContents: TStringList;
begin
  Result := False;

  //check if the file exists first
  if not FileExists(AFileName) then
  begin
    Error := 'file does not exist [' + AFileName + ']';
    Exit;
  end;

  LContents := TStringList.Create;
  try
    try
      //fetch file contents
      LContents.LoadFromFile(AFileName);

      //call to child
      Result := DoFromString(LContents.Text, Error);
    except on E : Exception do
      Error := E.Message;
    end;
  finally
    LContents.Free;
  end;
end;

end.

