unit config.types;

{$mode delphi}

interface

uses
  Classes,
  SysUtils;

type

  { IConfig }

  IConfig = interface
    ['{5CB1759D-1BF9-40C1-98F0-50C3368B468C}']
    function SaveToFile(Const AFileName : String; Out Error : String) : Boolean;
    function LoadFromFile(Const AFileName : String; Out Error : String) : Boolean;
  end;

  { IJSONConfig }

  IJSONConfig = interface(IConfig)
    ['{2C835EC9-5888-40B3-A7D2-4109A16D21D9}']
    //property methods
    function GetValByName(const AName : String): String;

    //properties
    property ValueByName[Const AName : String] : String read GetValByName; default;

    //methods
    procedure UpsertValue(Const AName, AValue : String);
    function ToJSON : String;
    function FromJSON(Const AJSON : String; Out Error : String) : Boolean;
  end;

function CreateJSONConfig : IJSONConfig;

implementation
uses
  config.json;

function CreateJSONConfig: IJSONConfig;
begin
  Result := TJSONConfigImpl.Create;
end;

end.

