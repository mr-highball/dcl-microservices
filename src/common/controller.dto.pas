unit controller.dto;

{$mode delphi}

interface

uses
  Classes, SysUtils;

type

  { TErrorResponse }
  (*
    common error response structure
  *)
  TErrorResponse = record
  strict private
    FMsg: String;
  public
    const
      PROP_ERROR = 'error';
  public
    property Message : String read FMsg write FMsg;

    function ToJSON() : String;
    procedure FromJSON(Const AJSON : String);
  end;

  { TBoolResponse }

  TBoolResponse = record
  public
    const
      PROP_SUCCESS = 'success';
  strict private
    FSuccess : Boolean;
  public
    property Success : Boolean read FSuccess write FSuccess;

    function ToJSON() : String;
    procedure FromJSON(Const AJSON : String);
  end;

  { TDatasetResponse }
  (*
    json serialized form of a dataset where rows are
    json objects in a the result array
  *)
  TDatasetResponse = record
  public
    const
      PROP_RESULT = 'result';
  private
    FRows : TArray<String>;
    function GetCount: Integer;
    function GetRow(const AIndex : Integer): String;
  public
    //single row json object representations
    property Rows : TArray<String> read FRows write FRows;
    property Row[Const AIndex : Integer] : String read GetRow; default;
    property Count : Integer read GetCount;

    procedure AddRow(Const ARowJSON : String);
    function ToJSON() : String;
    procedure FromJSON(Const AJSON : String);
  end;

implementation
uses
  fpjson,
  jsonparser;

{ TBoolResponse }

function TBoolResponse.ToJSON(): String;
begin
  with TJSONObject.Create do
  begin
    Add(PROP_SUCCESS, FSuccess);
    Result := AsJSON;
    Free;
  end;
end;

procedure TBoolResponse.FromJSON(const AJSON: String);
var
  LObj : TJSONData;
begin
  LObj := GetJSON(AJSON);

  if not Assigned(LObj) then
    raise Exception.Create('TBoolResponse::FromJSON::invalid json for error');

  if not (LObj.JSONType = jtObject) then
  begin
    LObj.Free;
    raise Exception.Create('TBoolResponse::FromJSON::json is not object');
  end;

  try
    FSuccess := TJSONObject(LObj).Get(PROP_SUCCESS);
  finally
    LObj.Free;
  end;
end;

{ TDatasetResponse }

function TDatasetResponse.GetRow(const AIndex : Integer): String;
begin
  Result := FRows[AIndex];
end;

function TDatasetResponse.GetCount: Integer;
begin
  Result := Length(FRows);
end;

procedure TDatasetResponse.AddRow(const ARowJSON: String);
begin
  SetLength(FRows, Succ(Length(FRows)));
  FRows[High(FRows)] := ARowJSON;
end;

function TDatasetResponse.ToJSON(): String;
var
  I: Integer;
  LResult : TJSONObject;
  LArr : TJSONArray;
  LObj : TJSONData;
begin
  Result := '{"' + PROP_RESULT + '" : []}';

  //init result json structure
  LResult := TJSONObject.Create;
  LArr := TJSONArray.Create;
  LResult.Add(PROP_RESULT, LArr);

  //add to result array
  try
    for I := 0 to High(FRows) do
    begin
      LObj := GetJSON(FRows[I]);

      if not Assigned(LObj) then
        continue;

      if not (Lobj.JSONType = jtObject) then
      begin
        LObj.Free;
        continue;
      end;

      LArr.Add(LObj);
    end;

    //serialize
    Result := LResult.AsJSON;
  finally
    LResult.Free;
  end;
end;

procedure TDatasetResponse.FromJSON(const AJSON: String);
var
  LObj: TJSONData;
  LArr: TJSONArray;
  I: Integer;
begin
  //initialize
  SetLength(FRows, 0);
  LObj := GetJSON(AJSON);

  if not Assigned(LObj) then
    raise Exception.Create('TDatasetResponse::FromJSON::invalid json for error');

  if not (LObj.JSONType = jtObject) then
  begin
    LObj.Free;
    raise Exception.Create('TDatasetResponse::FromJSON::json is not object');
  end;

  LArr := TJSONObject(LObj).Arrays[PROP_RESULT];

  if not Assigned(LArr) then
  begin
    LObj.Free;
    raise Exception.Create('TDatasetResponse::FromJSON::invalid json for error');
  end;

  //now traverse array and fill internal rows
  try
    SetLength(FRows, LArr.Count);

    //assumes all items are objects, otherwise will either be uneven
    //or throw an exception
    for I := 0 to Pred(LArr.Count) do
      FRows[I] := LArr.Objects[I].AsJSON;
  finally
    LObj.Free;
  end;
end;

{ TErrorResponse }

function TErrorResponse.ToJSON(): String;
var
  LObj : TJSONObject;
begin
  LObj := TJSONObject.Create;
  try
    LObj.Add(PROP_ERROR, FMsg);
    Result := LObj.AsJSON;
  finally
    Lobj.Free;
  end;
end;

procedure TErrorResponse.FromJSON(const AJSON: String);
var
  LObj: TJSONData;
begin
  LObj := GetJSON(AJSON);

  if not Assigned(LObj) then
    raise Exception.Create('TErrorResponse::FromJSON::invalid json for error');

  if not (LObj.JSONType = jtObject) then
  begin
    LObj.Free;
    raise Exception.Create('TErrorResponse::FromJSON::json is not object');
  end;

  try
    FMsg := TJSONObject(LObj).Get(PROP_ERROR);
  finally
    LObj.Free;
  end;
end;

end.

