unit controller.base;

{$mode delphi}{$H+}

interface

uses
  SysUtils,
  Classes,
  httpdefs,
  fpHTTP,
  fpWeb,
  SQLite3Conn,
  SQLDB,
  eventlog,
  controller.dto,
  openssl,
  sslbase,
  opensslsockets,
  sslsockets;

type

  //controller log types
  TControllerLogType = (
    clInfo,
    clWarn,
    clError
  );

  //set of log types used by controller to determine what to log
  TControllerLogTypes = set of TControllerLogType;

  { TAction }
  (*
    singular action for a controller
  *)
  TAction = record
  private
    FAction: TWebActionEvent;
    FName: String;
  public
    property Name : String read FName write FName;
    property Action : TWebActionEvent read FAction write FAction;
  end;

  //controller action array
  TActions = TArray<TAction>;

  { TControllerCertificate }

  TControllerCertificate = class(TX509Certificate)
  public
    function CreateCertificateAndKey: TCertAndKey; override;
  end;

  { TControllerSSLHandler }

  TControllerSSLHandler = class(TSSLSocketHandler)
  private
  protected
    function CreateCertificateData: TCertificateData; override;
    function CreateCertGenerator: TX509Certificate; override;
  public
  end;

  { TBaseController }
  (*
    base class for controllers that offers virtual methods
    to override for consistency across dcl micro services
  *)
  TBaseController = class(TFPWebModule)
    //default health check event
    procedure DataModuleGetAction(Sender: TObject; ARequest: TRequest;
      var ActionName: String);
    procedure HealthCheck(Sender : TObject; ARequest : TRequest;
      AResponse : TResponse; Var Handled : Boolean);
  strict private
    FRoot : String;
    FName : String;
    FLogTypes : TControllerLogTypes;
    function GetDBName: String;
    function GetFullPath: String;
    function GetLogTypes: TControllerLogTypes;
    function GetRoot: String;
    procedure SetDBName(Const AValue: String);
    procedure SetLogTypes(Const AValue: TControllerLogTypes);
    procedure SetRoot(Const AValue: String);

    //check if the db exists on the file system
    function DBExists : Boolean;

    //create the db if the file doesn't exist
    procedure CreateDB;
  strict protected

    (*
      logging methods accessible to children
      --------------------------------------------------------------------------
    *)

    //log info messages
    procedure LogInfo(Const AMsg : String);

    //log warning messages
    procedure LogWarn(Const AMsg : String);

    //log error messages
    procedure LogError(Const AMsg : String);

    procedure LogRequester(Const AMsg : String; Const ARequest : TRequest);

    (*
      raw sql commands
      --------------------------------------------------------------------------
    *)

    //executes blocking sql that will return an exception if failed
    function ExecuteSQL(Const ASQL : String; Out Error : String) : Boolean;

    //runs non-blocking sql that provides only evidence in the log a failure occurred
    procedure FireAndForget(Const ASQL : String);

    (*
      will execute and open a query that returns a result set. this will be
      returned in a json object where result is a json array to
      object representations of the rows
    *)
    function GetSQLResultsJSON(Const ASQL : String; Out Data : TDatasetResponse;
      Out Error : String) : Boolean;

    (*
      helper methods for children
      --------------------------------------------------------------------------
    *)

    //helper method for returning a json object with an error result
    function GetErrorJSON(Const AError : String) : String;

    (*
      children override these methods
      --------------------------------------------------------------------------
    *)

    //called when Initialize() is called to ensure db is setup
    procedure DoInitialize(); virtual;

    //called when a controller is initialized to define the actions available
    function DoInitializeActions() : TActions; virtual;
  public
    (*
      properties
      --------------------------------------------------------------------------
    *)

    //name of the sqlite db file
    property DataBaseName : String read GetDBName write SetDBName;

    //root directory of the db file
    property RootPath : String read GetRoot write SetRoot;

    //full path to the db file (root + delim + dbname)
    property FullPath : String read GetFullPath;

    //types of logging to perform
    property LogTypes : TControllerLogTypes read GetLogTypes write SetLogTypes;

    (*
      methods
      --------------------------------------------------------------------------
    *)

    (*
      performs any initialization step required to the database file
      ensuring future queries have everything they need to operate,
      as well as setting up actions
    *)
    procedure SetupController;

    constructor Create; virtual; overload;
    constructor Create(AOwner: TComponent); override; overload;
    destructor Destroy; override;
  end;

var
  DEFAULT_DB_NAME : String;
  CONTROLLER_LOG_TYPES : TControllerLogTypes;
  CERT_PASSPHRASE : String;
  CERT_PRIVATE_FILE : String;
  CERT_PUBLIC_FILE : String;
  connection : TSQLite3Connection;
  transaction : TSQLTransaction;

function GetControllerRoute(Const AAction : String) : String;

implementation
uses
  fpjson,
  jsonparser,
  DB;
var
  FLog : TEventLog; //log singleton

function GetControllerRoute(const AAction: String): String;
begin
  Result := 'controller/' + AAction;
end;

{$R *.lfm}

{ TControllerCertificate }

function TControllerCertificate.CreateCertificateAndKey: TCertAndKey;
var
  LData : TMemoryStream;
  LBuf : TBytes;
begin
  if not FileExists(CERT_PUBLIC_FILE) then
    raise Exception.Create('Controller::Base::CERT_PUBLIC_FILE not set to a valid file');

  if not FileExists(CERT_PRIVATE_FILE) then
    raise Exception.Create('Controller::Base::CERT_PRIVATE_FILE not set to a valid file');

  LData := TMemoryStream.Create;
  try
    //load public key contents
    LData.LoadFromFile(CERT_PUBLIC_FILE);
    SetLength(LBuf, LData.Size);
    LData.ReadBuffer(LBuf, LData.Size);
    Result.Certificate := LBuf;

    //clear stream
    LData.Clear;

    //load private key contents
    LData.LoadFromFile(CERT_PRIVATE_FILE);
    SetLength(LBuf, LData.Size);
    LData.ReadBuffer(LBuf, LData.Size);
    Result.PrivateKey := LBuf;
  finally
    LData.Free;
  end;
end;

{ TControllerSSLHandler }

function TControllerSSLHandler.CreateCertificateData: TCertificateData;
begin
  Result:=inherited CreateCertificateData;
  Result.HostName := '127.0.0.1';
  Result.KeyPassword := CERT_PASSPHRASE;
  Result.Certificate.FileName := CERT_PUBLIC_FILE;
  Result.PrivateKey.FileName := CERT_PRIVATE_FILE;
end;

function TControllerSSLHandler.CreateCertGenerator: TX509Certificate;
begin
  Result := TControllerCertificate.Create;
end;

{ TBaseController }

function TBaseController.GetDBName: String;
begin
  Result := FName;
end;

function TBaseController.GetFullPath: String;
begin
  if Length(FRoot) < 1 then
    Result := DataBaseName
  else
  begin
    //check first to see if the last index is a path delimiter
    if IsPathDelimiter(FRoot, High(FRoot)) then
      Result := FRoot + DataBaseName
    //otherwise we need to append the delimiter to the end of root
    else
      Result := FRoot + PathDelim + DataBaseName;
  end;
end;

function TBaseController.GetLogTypes: TControllerLogTypes;
begin
  Result := FLogTypes;
end;

function TBaseController.GetRoot: String;
begin
  Result := FRoot;
end;

procedure TBaseController.SetDBName(const AValue: String);
begin
  FName := AValue;
end;

procedure TBaseController.SetLogTypes(const AValue: TControllerLogTypes);
begin
  FLogTypes := AValue;
end;

procedure TBaseController.SetRoot(const AValue: String);
begin
  FRoot := AValue;
end;

function TBaseController.DBExists: Boolean;
begin
  Result := FileExists(FullPath);
end;

procedure TBaseController.CreateDB;
begin
  //set to default if caller hasn't specified
  if FName.IsEmpty then
  begin
    LogInfo('CreateDB::database name is default [' + DEFAULT_DB_NAME + ']');
    FName := DEFAULT_DB_NAME;
  end;

  if not connection.Connected then
  begin
    connection.DatabaseName := FullPath;
    connection.Open;
  end;
end;

procedure TBaseController.HealthCheck(Sender: TObject; ARequest: TRequest;
  AResponse: TResponse; var Handled: Boolean);
begin
  try
    LogRequester('HealthCheck::', ARequest);
    Handled := True;
    AResponse.ContentType := 'application/json';
    AResponse.Content := '{"status" : "OK"}';
  except on E : Exception do
    AResponse.Content := GetErrorJSON(E.Message);
  end;
end;

procedure TBaseController.DataModuleGetAction(Sender: TObject;
  ARequest: TRequest; var ActionName: String);
var
  LAction: TFPWebAction;
begin
  LogRequester('GetAction::', ARequest);
  LAction := Actions.FindAction(ActionName);

  if not Assigned(LAction) then
    LogWarn('GetAction::unable to find action [' + ActionName + '] check spelling?');
end;

procedure TBaseController.LogInfo(const AMsg: String);
begin
  if not (clInfo in FLogTypes) then
    Exit;

  while FLog.Active do
    Sleep(5);

  //we disable the log each time to prevent file locking as well as
  //to keep up a "rolling" log file
  try
    FLog.AppendContent:=True;
    FLog.LogType:=ltFile;
    FLog.FileName:=ExtractFileDir(ParamStr(0)) + PathDelim + DataBaseName.Split('.')[0] + '_' + FormatDateTime('mm_dd_yyyy',Now) + '.log';
    FLog.Active:=True;
    FLog.Info(Self.ClassName + '::' + AMsg);
  finally
    FLog.Active := False;
  end;
end;

procedure TBaseController.LogWarn(const AMsg: String);
begin
  if not (clWarn in FLogTypes) then
    Exit;

  while FLog.Active do
    Sleep(5);

  //log warnings
  try
    FLog.AppendContent:=True;
    FLog.LogType:=ltFile;
    FLog.FileName:=ExtractFileDir(ParamStr(0)) + PathDelim + DataBaseName.Split('.')[0] + '_' + FormatDateTime('mm_dd_yyyy',Now) + '.log';
    FLog.Active:=True;
    FLog.Warning(Self.ClassName + '::' + AMsg);
  finally
    FLog.Active := False;
  end;
end;

procedure TBaseController.LogError(const AMsg: String);
begin
  if not (clError in FLogTypes) then
    Exit;

  while FLog.Active do
    Sleep(5);

  //log errors
  try
    FLog.AppendContent:=True;
    FLog.LogType:=ltFile;
    FLog.FileName:=ExtractFileDir(ParamStr(0)) + PathDelim + DataBaseName.Split('.')[0] + '_' + FormatDateTime('mm_dd_yyyy',Now) + '.log';
    FLog.Active:=True;
    FLog.Error(Self.ClassName + '::' +AMsg);
  finally
    FLog.Active := False;
  end;
end;

procedure TBaseController.LogRequester(const AMsg: String;
  const ARequest: TRequest);
var
  LMsg: String;
  I: Integer;
begin
  LMsg := AMsg + sLineBreak +
    'Host:' + sLineBreak + ARequest.GetVariableHeaderName(hvRemoteHost) +
    'Address:' + sLineBreak + ARequest.GetVariableHeaderName(hvRemoteAddress) + sLineBreak +
    'Fields:' + sLineBreak;
  {$Warnings OFF}
  for I := 0 to Pred(ARequest.FieldCount) do
    LMsg := LMsg + 'Name = ' + ARequest.FieldNames[I] + ' Value = ' + ARequest.FieldValues[I] + sLineBreak;
  {$Warnings ON}
  LogInfo(LMsg);
end;

function TBaseController.ExecuteSQL(const ASQL: String; out Error: String): Boolean;
var
  LQuery : TSQLQuery;
begin
  Result := False;

  try
    //create a query
    LQuery := TSQLQuery.Create(nil);
    try
      LQuery.SQLConnection := connection;
      LQuery.SQL.Text := ASQL;
      LQuery.ExecSQL;
      Result := True;
    finally
      connection.Transaction.CommitRetaining;
      LQuery.Free;
    end;
  except on E : Exception do
  begin
    Error := E.Message;
    LogError('ExecuteSQL::sql exception occurred with [' + Error + '], here is the raw command:' + sLineBreak + ASQL + sLineBreak);
  end
  end;
end;

procedure TBaseController.FireAndForget(const ASQL: String);
var
  LError: String;
begin
  //need to create an ezthread and fire
  ExecuteSQL(ASQL, LError); //todo - make this async
end;

function TBaseController.GetSQLResultsJSON(const ASQL: String;
  out Data : TDatasetResponse; out Error: String): Boolean;
var
  LObj : TJSONObject;
  LQuery : TSQLQuery;
  I: Integer;
  LField: TField;
begin
  Result := False;

  LQuery := TSQLQuery.Create(nil);
  try
    try
      //init query
      LQuery.SQLConnection := connection;
      LQuery.ReadOnly := True;
      LQuery.UniDirectional := True; //discards records after reading, smaller footprint
      LQuery.SQL.Text := ASQL;

      //now we can open the query for reading
      LQuery.Open;
      try
        //traverse dataset and build up a json object to add to the the row
        while not LQuery.EOF do
        begin
          LObj := TJSONObject.Create;
          try
            try

              //iterate fields to build the object
              for I := 0 to Pred(LQuery.FieldCount) do
              begin
                //get a reference to the field
                LField := LQuery.Fields[I];

                case LField.DataType of
                  //boolean case
                  ftBoolean : LObj.Add(LField.DisplayLabel, TJSONBoolean.Create(LField.AsBoolean));

                  //number types
                  ftInteger,
                  ftFloat,
                  ftCurrency,
                  ftsmallint,
                  ftLargeint,
                  ftWord : LObj.Add(LField.DisplayLabel, LField.AsFloat);

                  //default to string otherwise
                  else
                    LObj.Add(LField.DisplayLabel, LField.AsString);
                end;
              end;

              //add the row
              Data.AddRow(LObj.AsJSON);
            finally
              LObj.Free;
            end;
          except on E : Exception do
          begin
            Error := E.Message;
            Exit;
          end
          end;

          //next row
          LQuery.Next;
        end;
      finally
        LQuery.Close;
      end;

      //success
      Result := True;
    except on E : Exception do
      Error := E.Message;
    end;
  finally
    LQuery.Free;
  end;
end;

function TBaseController.GetErrorJSON(const AError: String): String;
var
  LError : TErrorResponse;
begin
  LError.Message := AError;
  Result := LError.ToJSON();
end;

procedure TBaseController.DoInitialize();
begin
  //nothing
end;

function TBaseController.DoInitializeActions(): TActions;
var
  LHealth : TAction;
begin
  //init result
  Result := [];
  SetLength(Result, 1);

  //define the health action which outputs all available actions
  LHealth.Name := 'health';
  LHealth.Action := HealthCheck;

  //add the health action
  Result[0] := LHealth;
end;

procedure TBaseController.SetupController;
var
  LActions : TActions;
  I: Integer;
  LAction: TFPWebAction;
begin
  try
    //before children SetupController is called, we need to ensure the
    //database exists, and if not, create it
    if not DBExists then
      CreateDB;

    //attempt to call child method
    DoInitialize;

    //SetupController the actions for this controller
    Actions.Clear; //clear existing if any
    LActions := DoInitializeActions; //get child actions

    //for each of the actions returned to us by our children, add
    //an actual action item
    for I := 0 to High(LActions) do
    begin
      LAction := Actions.Add;

      //first action will be the default
      if I = 0 then
      begin
        Actions.DefActionWhenUnknown := True;
        LAction.Default := True;
      end;

      //configure the action
      LAction.Name := LActions[I].Name;
      LAction.OnRequest := LActions[I].Action;
    end;

    //log the registered actions
    for I := 0 to Pred(Actions.Count) do
      LogInfo('SetupController::Action [' + Actions[I].Name + '] is index [' + IntToStr(I) + ']');
  except on E : Exception do
  begin
    LogError(E.Message); //log
    raise E; //re-throw so caller knows and can handle
  end;
  end;
end;

constructor TBaseController.Create;
begin
  Create(nil);
end;

constructor TBaseController.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FLogTypes := CONTROLLER_LOG_TYPES;

  //we need at least one default action defined in base before calling this
  //ie. the health check will be defined at the design level
  SetupController;
end;

destructor TBaseController.Destroy;
begin

  inherited Destroy;
end;

initialization
  connection := TSQLite3Connection.Create(nil);
  transaction := TSQLTransaction.Create(connection);
  connection.Transaction := transaction;
  DEFAULT_DB_NAME := 'database.sqlite3';
  CONTROLLER_LOG_TYPES := [clInfo, clWarn, clError];
  CERT_PASSPHRASE := 'changeMe';
  CERT_PRIVATE_FILE := 'key.pem';
  CERT_PUBLIC_FILE := 'cert.pem';
  //TSSLSocketHandler.SetDefaultHandlerClass(TControllerSSLHandler);
  FLog := TEventLog.Create(nil);
finalization
  FLog.Active := False;
  FLog.Free;
  connection.Close();
  connection.free;
end.

