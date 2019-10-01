unit config.json;

{$mode delphi}

interface

uses
  Classes,
  SysUtils,
  fpjson,
  jsonparser,
  fgl,
  config.base,
  config.types;

type

  { TJSONConfigImpl }
  (*
    standard json configuration implementation. children
    add any number of properties they would like to ease accessibility.
  *)
  TJSONConfigImpl = class(TConfigImpl, IJSONConfig)
  protected
    type
      TValueMap = TFPGMap<String, String>;
  strict private
    FValues : TValueMap;
  strict protected
    function DoToString: String; override;
    function DoFromString(const AFile: String; out Error: String): Boolean;override;

    //for special processing, children override can override these two methods
    procedure DoSerializeValue(Const AName, AValue : String; Out JSON : TJSONData); virtual;
    function DoDeserializeValue(Const AName : String; Const AValue : TJSONData) : String; virtual;
  protected
    //interface specific methods
   function GetValByName(const AName : String): String;
  public
    property ValueByName[Const AName : String] : String read GetValByName; default;

    procedure UpsertValue(Const AName, AValue : String);
    function ToJSON : String;
    function FromJSON(Const AJSON : String; Out Error : String) : Boolean;

    constructor Create; virtual;
    destructor destroy; override;
  end;

implementation

{ TJSONConfigImpl }

function TJSONConfigImpl.DoToString: String;
begin
  Result := ToJSON;
end;

function TJSONConfigImpl.DoFromString(const AFile: String; out Error: String): Boolean;
begin
  Result := FromJSON(AFile, Error);
end;

procedure TJSONConfigImpl.DoSerializeValue(const AName, AValue: String; out
  JSON: TJSONData);
begin
  JSON := TJSONString.Create(AValue);
end;

function TJSONConfigImpl.DoDeserializeValue(const AName: String;
  const AValue: TJSONData): String;
begin
  Result := AValue.AsString;
end;

function TJSONConfigImpl.GetValByName(const AName: String): String;
var
  I : Integer;
begin
  Result := '';

  FValues.Sorted := True;

  if FValues.Find(AName, I) then
    Result := FValues.Data[I];
end;

procedure TJSONConfigImpl.UpsertValue(const AName, AValue: String);
begin
  FValues.AddOrSetData(AName, AValue);
end;

function TJSONConfigImpl.ToJSON: String;
var
  LObj : TJSONObject;
  I: Integer;
  LData: TJSONData;
begin
  LObj := TJSONObject.Create;

  try
    //iterate and call to child serialize method
    for I := 0 to Pred(FValues.Count) do
    begin
      DoSerializeValue(FValues.Keys[I], FValues.Data[I], LData);
      LObj.Add(FValues.Keys[I], LData);
    end;

    //always return a json object
    Result := LObj.FormatJSON();
  finally
    LObj.Free;
  end;
end;

function TJSONConfigImpl.FromJSON(const AJSON: String; out Error: String): Boolean;
var
  LData : TJSONData;
  LObj : TJSONObject;
  I: Integer;
begin
  Result := False;

  try
    //parse json string
    LData := GetJSON(AJSON);

    //parse error?
    if not Assigned(LData) then
    begin
      Error := 'invalid json format';
      Exit;
    end;

    try
      //needs to be object type
      if LData.JSONType <> jtObject then
      begin
        Error := 'json needs to be a valid object';
        Exit;
      end;

      //cast to object
      LObj := TJSONObject(LData);

      for I := 0 to Pred(LObj.Count) do
        UpsertValue(LObj.Names[I], DoDeserializeValue(LObj.Names[I], LObj.Elements[LObj.Names[I]]));

      //success
      Result := True;
    finally
      LData.Free;
    end;
  except on E : Exception do
    Error := E.Message;
  end;
end;

constructor TJSONConfigImpl.Create;
begin
  FValues := TValueMap.Create;
end;

destructor TJSONConfigImpl.destroy;
begin
  FValues.Free;
  inherited destroy;
end;

end.

