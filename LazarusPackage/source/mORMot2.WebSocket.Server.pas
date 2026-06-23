{$mode delphi}
unit mORMot2.WebSocket.Server;

interface

uses
  Classes,
  SysUtils,
  Generics.Collections,
  SyncObjs,
  TypInfo,
  Forms,
  mormot.core.base,
  mormot.core.os,
  mormot.core.unicode,
  mormot.core.text,
  mormot.core.log,
  mormot.core.threads,
  mormot.net.ws.core,
  mormot.net.ws.async,
  mormot.net.async,
  mormot.net.http,
  mormot.net.Server,
  mormot.crypt.core,
  mormot.crypt.secure;

const
  ENCRYPTION_HEADER_SIZE = 16;
  ENCRYPTION_MAGIC = $4D4F524D;

type
  TWebSocketServerState = (ssIdle, ssStarting, ssListening, ssStopping, ssError);

  TWebSocketClientState = (csDisconnected, csConnecting, csConnected,
    csDisconnecting, csError);

  TAESMode = (amECB, amCBC, amCFB, amOFB, amCTR, amGCM, amCFC, amOFC, amCTC);

  TAESKeySize = (aks128, aks192, aks256);

  TmORMot2WebSocketServer = class;

  TWebSocketClientConnectedEvent    = procedure(Sender: TObject; ClientID: Integer) of object;
  TWebSocketClientDisconnectedEvent = procedure(Sender: TObject; ClientID: Integer) of object;
  TWebSocketErrorEvent              = procedure(Sender: TObject; const ErrorMsg: string) of object;
  TWebSocketDataReceivedEvent       = procedure(Sender: TObject; ClientID: Integer; const Data: TBytes) of object;
  TWebSocketDataSentEvent           = procedure(Sender: TObject; ClientID: Integer; const Data: TBytes) of object;
  TWebSocketHandleCommandEvent      = procedure(Sender: TObject; ClientID: Integer; const Command: TBytes) of object;
  TWebSocketServerStateChangeEvent  = procedure(Sender: TObject; OldState, NewState: TWebSocketServerState; const StateDescription: string) of object;

  TCustomWebSocketAsyncProcess = class(TWebSocketAsyncProcess)
  public
    property Connection: TWebSocketAsyncConnection read fConnection;
  end;

  TCustomWebSocketServerProtocol = class(TWebSocketProtocolJson)
  private
    fOwnerServer: TmORMot2WebSocketServer;
  protected
    procedure ProcessIncomingFrame(Sender: TWebSocketProcess;
      var Request: TWebSocketFrame; const Info: RawUtf8); override;
  public
    constructor Create(AOwnerServer: TmORMot2WebSocketServer);
    function Clone(const aClientUri: RawUtf8): TWebSocketProtocol; override;
  end;

  TCustomAsyncServerRest = class(TWebSocketAsyncServerRest)
  private
    fOwnerComponent: TmORMot2WebSocketServer;
    fConnectionMap:   TDictionary<TWebSocketAsyncConnection, Integer>;
    fConnectionIdMap: TDictionary<Integer, TWebSocketAsyncConnection>;
    fClientIPMap:     TDictionary<Integer, string>;
    fNextConnectionId: Integer;
    fMapLock: TCriticalSection;
  protected
    procedure DoConnect(Context: TWebSocketAsyncConnection); override;
    procedure DoDisconnect(Context: TWebSocketAsyncConnection); override;
    function  GetConnectionId(Connection: TWebSocketAsyncConnection): Integer;
    procedure RemoveConnection(Connection: TWebSocketAsyncConnection);
    function  ExtractIPFromConnection(Connection: TWebSocketAsyncConnection): string;
  public
    constructor Create(const aPort: RawUtf8;
      const OnStart, OnStop: TOnNotifyThread; const aProcessName: RawUtf8;
      ServerThreadPoolCount: Integer;
      const aWebSocketsURI, aWebSocketsEncryptionKey: RawUtf8;
      aWebSocketsAjax: boolean; ProcessOptions: THttpServerOptions);
    destructor Destroy; override;
    function  SendToClient(ClientID: Integer; const Data: TBytes): boolean;
    procedure BroadcastToAll(const Data: TBytes);
    function  GetClientConnection(ClientID: Integer): TWebSocketAsyncConnection;
    function  GetStoredClientIP(ClientID: Integer): string;
    property  ProcessClass: TWebSocketAsyncProcessClass read fProcessClass write fProcessClass;
    property  OwnerComponent: TmORMot2WebSocketServer read fOwnerComponent write fOwnerComponent;
  end;

  TClientConnectionInfo = record
    ClientID:    Integer;
    Connection:  TWebSocketAsyncConnection;
    IP:          string;
    ConnectedAt: TDateTime;
    State:       TWebSocketClientState;
  end;

  TmORMot2WebSocketServer = class(TComponent)
  private
    fServer: TCustomAsyncServerRest;
    fPort:   Integer;
    fActive: boolean;
    fLastError:   string;
    fClientCount: Integer;
    fShuttingDown: boolean;
    fIsDestroying: boolean;
    fServerThreadPoolCount: Integer;
    fKeepAliveTimeOut: Integer;
    fWebSocketsURI:    string;
    fWebSocketsAjax:   boolean;
    fClientInfoMap: TDictionary<Integer, TClientConnectionInfo>;
    fClientLock:    TCriticalSection;
    fServerState:   TWebSocketServerState;
    fEncryptionEnabled: boolean;
    fEncryptionKey:     string;
    fEncryptionMode:    TAESMode;
    fEncryptionKeySize: TAESKeySize;
    { Pre-derived AES key — refreshed whenever EncryptionKey/Mode/KeySize changes.
      Avoids running PBKDF2 on every single WebSocket frame. }
    fCryptoLock:  TCriticalSection;
    fDerivedKey:  THash256;
    fKeyDerived:  Boolean;
    fConnectionTimeout: Integer;
    fDescription:    string;
    fKeepAlive:      Boolean;
    fLogLevel:       Integer;
    fMaxConnections: Integer;
    fNoDelay:        Boolean;
    fReceiveBufferSize: Integer;
    fReusePort:      Boolean;
    fSendBufferSize: Integer;
    fThreadPoolSize: Integer;
    fVersion:        string;
    fTotalActiveConnections: Integer;
    fTotalBytesReceived:     Int64;
    fTotalBytesSent:         Int64;
    fTotalConnections:       Int64;
    fTotalMessagesReceived:  Int64;
    fTotalMessagesSent:      Int64;
    fOnClientConnected:    TWebSocketClientConnectedEvent;
    fOnClientDisconnected: TWebSocketClientDisconnectedEvent;
    fOnError:              TWebSocketErrorEvent;
    fOnDataReceived:       TWebSocketDataReceivedEvent;
    fOnDataSent:           TWebSocketDataSentEvent;
    fOnHandleCommand:      TWebSocketHandleCommandEvent;
    fOnServerStateChange:  TWebSocketServerStateChangeEvent;

    function  ServerStateToString(State: TWebSocketServerState): string;
    function  ClientStateToString(State: TWebSocketClientState): string;
    function  GetServerStateDescription: string;
    procedure SetServerState(NewState: TWebSocketServerState);
    procedure SetClientState(ClientID: Integer; NewState: TWebSocketClientState);
    function  GetEncryptionInfo: string;
    function  EncryptData(const Data: TBytes; const ClientIP: string): TBytes;
    function  DecryptData(const Data: TBytes; const ClientIP: string): TBytes;
    procedure SetActive(const Value: boolean);
    procedure SetPort(const Value: Integer);
    procedure SetServerThreadPoolCount(const Value: Integer);
    procedure SetKeepAliveTimeOut(const Value: Integer);
    procedure SetWebSocketsURI(const Value: string);
    procedure SetWebSocketsAjax(const Value: boolean);
    procedure SetEncryptionEnabled(const Value: boolean);
    procedure SetEncryptionKey(const Value: string);
    procedure SetEncryptionMode(const Value: TAESMode);
    procedure SetEncryptionKeySize(const Value: TAESKeySize);
    procedure SetConnectionTimeout(const Value: Integer);
    procedure SetDescription(const Value: string);
    procedure SetKeepAlive(const Value: Boolean);
    procedure SetLogLevel(const Value: Integer);
    procedure SetMaxConnections(const Value: Integer);
    procedure SetNoDelay(const Value: Boolean);
    procedure SetReceiveBufferSize(const Value: Integer);
    procedure SetReusePort(const Value: Boolean);
    procedure SetSendBufferSize(const Value: Integer);
    procedure SetThreadPoolSize(const Value: Integer);
    procedure SetVersion(const Value: string);
    procedure DoError(const ErrorMsg: string);
    procedure DoServerStateChange(OldState, NewState: TWebSocketServerState);

  protected
    procedure InternalStart;
    procedure InternalStop;
    procedure InitializeDefaults;
    procedure DeriveEncryptionKey;

  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;
    function  Start: boolean;
    procedure Stop;
    function  SendCommandToClient(ClientID: Integer; const Command: TBytes): boolean;
    procedure BroadcastCommand(const Command: TBytes);
    function  IsActive: boolean;
    function  GetLastError: string;
    function  GetClientCount: Integer;
    function  GetServerStats: string;
    procedure ResetStatistics;
    function  GetClientInfo(ClientID: Integer): string;
    function  GetClientIP(ClientID: Integer): string;
    function  GetClientEndpoint(ClientID: Integer): string;
    function  GetServerStateAsString: string;
    function  GetClientStateAsString(ClientID: Integer): string;
    function  GetAllClientStates: string;
    function  CanAcceptNewConnection: Boolean;
    function  GetConnectionsRemaining: Integer;
    procedure OnInternalClientConnected(ClientID: Integer;
      Connection: TWebSocketAsyncConnection; const ClientIP: string);
    procedure OnInternalClientDisconnected(ClientID: Integer);
    procedure OnInternalDataReceived(ClientID: Integer; const RawData: TBytes;
      const DecData: TBytes);

  published
    property Port:   Integer read fPort  write SetPort  default 8080;
    property Active: boolean read fActive write SetActive default False;
    property ServerThreadPoolCount: Integer read fServerThreadPoolCount write SetServerThreadPoolCount default 32;
    property KeepAliveTimeOut:      Integer read fKeepAliveTimeOut      write SetKeepAliveTimeOut      default 30000;
    property WebSocketsURI:  string  read fWebSocketsURI  write SetWebSocketsURI;
    property WebSocketsAjax: boolean read fWebSocketsAjax write SetWebSocketsAjax default True;
    property ServerState:            TWebSocketServerState read fServerState;
    property ServerStateDescription: string               read GetServerStateDescription;
    property EncryptionEnabled: boolean    read fEncryptionEnabled write SetEncryptionEnabled default False;
    property EncryptionKey:     string     read fEncryptionKey     write SetEncryptionKey;
    property EncryptionMode:    TAESMode   read fEncryptionMode    write SetEncryptionMode    default amCBC;
    property EncryptionKeySize: TAESKeySize read fEncryptionKeySize write SetEncryptionKeySize default aks256;
    property EncryptionInfo:    string     read GetEncryptionInfo;
    property ConnectionTimeout: Integer read fConnectionTimeout write SetConnectionTimeout default 10;
    property Description:       string  read fDescription       write SetDescription;
    property KeepAlive:         Boolean read fKeepAlive         write SetKeepAlive         default True;
    property LogLevel:          Integer read fLogLevel          write SetLogLevel          default 1000;
    property MaxConnections:    Integer read fMaxConnections    write SetMaxConnections    default 1000;
    property NoDelay:           Boolean read fNoDelay           write SetNoDelay           default True;
    property ReceiveBufferSize: Integer read fReceiveBufferSize write SetReceiveBufferSize default 8192;
    property ReusePort:         Boolean read fReusePort         write SetReusePort         default True;
    property SendBufferSize:    Integer read fSendBufferSize    write SetSendBufferSize    default 8192;
    property ThreadPoolSize:    Integer read fThreadPoolSize    write SetThreadPoolSize    default 4;
    property Version:           string  read fVersion           write SetVersion;
    property TotalActiveConnections: Integer read fTotalActiveConnections;
    property TotalBytesReceived:     Int64   read fTotalBytesReceived;
    property TotalBytesSent:         Int64   read fTotalBytesSent;
    property TotalConnections:       Int64   read fTotalConnections;
    property TotalMessagesReceived:  Int64   read fTotalMessagesReceived;
    property TotalMessagesSent:      Int64   read fTotalMessagesSent;
    property OnClientConnected:    TWebSocketClientConnectedEvent    read fOnClientConnected    write fOnClientConnected;
    property OnClientDisconnected: TWebSocketClientDisconnectedEvent read fOnClientDisconnected write fOnClientDisconnected;
    property OnError:              TWebSocketErrorEvent              read fOnError              write fOnError;
    property OnDataReceived:       TWebSocketDataReceivedEvent       read fOnDataReceived       write fOnDataReceived;
    property OnDataSent:           TWebSocketDataSentEvent           read fOnDataSent           write fOnDataSent;
    property OnHandleCommand:      TWebSocketHandleCommandEvent      read fOnHandleCommand      write fOnHandleCommand;
    property OnServerStateChange:  TWebSocketServerStateChangeEvent  read fOnServerStateChange  write fOnServerStateChange;
  end;

procedure Register;

implementation

{ Self-destroying helper objects that carry per-call data and marshal events
  from mORMot2 async threads to the LCL main thread.
  TThread.Queue posts Run (a method pointer) to the main message loop;
  the object frees itself at the end of Run. }

type
  TClientEventRunner = class
  private
    fServer:    TmORMot2WebSocketServer;
    fClientID:  Integer;
    fIsConnect: Boolean;
    procedure Run(Data: PtrInt);
  end;

  TDataEventRunner = class
  private
    fServer:   TmORMot2WebSocketServer;
    fClientID: Integer;
    fRawData:  TBytes;
    fDecData:  TBytes;
    procedure Run(Data: PtrInt);
  end;

procedure TClientEventRunner.Run(Data: PtrInt);
begin
  try
    if not fServer.fIsDestroying then
    begin
      if fIsConnect then
      begin
        if Assigned(fServer.fOnClientConnected) then
          fServer.fOnClientConnected(fServer, fClientID);
      end
      else
      begin
        if Assigned(fServer.fOnClientDisconnected) then
          fServer.fOnClientDisconnected(fServer, fClientID);
      end;
    end;
  finally
    Free;
  end;
end;

procedure TDataEventRunner.Run(Data: PtrInt);
begin
  try
    if not fServer.fIsDestroying then
    begin
      Inc(fServer.fTotalBytesReceived, Length(fRawData));
      Inc(fServer.fTotalMessagesReceived);
      if Assigned(fServer.fOnDataReceived) then
        fServer.fOnDataReceived(fServer, fClientID, fRawData);
      if Assigned(fServer.fOnHandleCommand) then
        fServer.fOnHandleCommand(fServer, fClientID, fDecData);
    end;
  finally
    Free;
  end;
end;

{ TCustomWebSocketServerProtocol }

constructor TCustomWebSocketServerProtocol.Create(AOwnerServer: TmORMot2WebSocketServer);
begin
  fOwnerServer := AOwnerServer;
  inherited Create('');
end;

function TCustomWebSocketServerProtocol.Clone(const aClientUri: RawUtf8): TWebSocketProtocol;
begin
  Result := TCustomWebSocketServerProtocol.Create(fOwnerServer);
end;

procedure TCustomWebSocketServerProtocol.ProcessIncomingFrame(
  Sender: TWebSocketProcess; var Request: TWebSocketFrame; const Info: RawUtf8);
var
  rawBytes:       TBytes;
  decryptedBytes: TBytes;
  ClientID:       Integer;
  Connection:     TWebSocketAsyncConnection;
begin
  if (fOwnerServer <> nil) and not fOwnerServer.fIsDestroying and (Sender <> nil) then
  begin
    case Request.opcode of
      focText:
        begin
          SetLength(rawBytes, Length(Request.payload));
          if Length(rawBytes) > 0 then
            Move(Request.payload[1], rawBytes[0], Length(rawBytes));

          if Sender is TCustomWebSocketAsyncProcess then
          begin
            Connection := TCustomWebSocketAsyncProcess(Sender).Connection;
            ClientID   := fOwnerServer.fServer.GetConnectionId(Connection);
          end
          else
            ClientID := 0;

          decryptedBytes := rawBytes;
          if fOwnerServer.fEncryptionEnabled then
            decryptedBytes := fOwnerServer.DecryptData(rawBytes,
              fOwnerServer.fServer.GetStoredClientIP(ClientID));

          { Dispatch data + command events to main thread.
            Per-call runner object avoids races between concurrent client threads. }
          with TDataEventRunner.Create do
          begin
            fServer   := fOwnerServer;
            fClientID := ClientID;
            fRawData  := rawBytes;
            fDecData  := decryptedBytes;
            Application.QueueAsyncCall(Run, 0);
          end;
        end;
    end;
  end;
  inherited ProcessIncomingFrame(Sender, Request, Info);
end;

{ TCustomAsyncServerRest }

constructor TCustomAsyncServerRest.Create(const aPort: RawUtf8;
  const OnStart, OnStop: TOnNotifyThread; const aProcessName: RawUtf8;
  ServerThreadPoolCount: Integer;
  const aWebSocketsURI, aWebSocketsEncryptionKey: RawUtf8;
  aWebSocketsAjax: boolean; ProcessOptions: THttpServerOptions);
begin
  fNextConnectionId := 1;
  fConnectionMap   := TDictionary<TWebSocketAsyncConnection, Integer>.Create;
  fConnectionIdMap := TDictionary<Integer, TWebSocketAsyncConnection>.Create;
  fClientIPMap     := TDictionary<Integer, string>.Create;
  fMapLock         := TCriticalSection.Create;

  inherited Create(aPort, OnStart, OnStop, aProcessName, ServerThreadPoolCount,
    aWebSocketsURI, aWebSocketsEncryptionKey, aWebSocketsAjax, ProcessOptions, nil);

  ProcessClass := TCustomWebSocketAsyncProcess;
end;

destructor TCustomAsyncServerRest.Destroy;
begin
  fMapLock.Free;
  fClientIPMap.Free;
  fConnectionIdMap.Free;
  fConnectionMap.Free;
  inherited;
end;

function TCustomAsyncServerRest.ExtractIPFromConnection(
  Connection: TWebSocketAsyncConnection): string;
begin
  Result := '127.0.0.1';
  if Connection = nil then
    Exit;
  try
    if Connection.RemoteIP <> '' then
      Result := Connection.RemoteIP;
  except
    on E: Exception do
      if fOwnerComponent <> nil then
        fOwnerComponent.DoError('IP extraction error: ' + E.Message);
  end;
end;

function TCustomAsyncServerRest.GetConnectionId(
  Connection: TWebSocketAsyncConnection): Integer;
begin
  fMapLock.Enter;
  try
    if not fConnectionMap.TryGetValue(Connection, Result) then
    begin
      Result := fNextConnectionId;
      Inc(fNextConnectionId);
      fConnectionMap.Add(Connection, Result);
      fConnectionIdMap.Add(Result, Connection);
    end;
  finally
    fMapLock.Leave;
  end;
end;

function TCustomAsyncServerRest.GetStoredClientIP(ClientID: Integer): string;
begin
  fMapLock.Enter;
  try
    if not fClientIPMap.TryGetValue(ClientID, Result) then
      Result := 'localhost';
  finally
    fMapLock.Leave;
  end;
end;

procedure TCustomAsyncServerRest.RemoveConnection(
  Connection: TWebSocketAsyncConnection);
var
  ConnectionID: Integer;
begin
  fMapLock.Enter;
  try
    if fConnectionMap.TryGetValue(Connection, ConnectionID) then
    begin
      fConnectionMap.Remove(Connection);
      fConnectionIdMap.Remove(ConnectionID);
      fClientIPMap.Remove(ConnectionID);
    end;
  finally
    fMapLock.Leave;
  end;
end;

function TCustomAsyncServerRest.GetClientConnection(ClientID: Integer)
  : TWebSocketAsyncConnection;
begin
  fMapLock.Enter;
  try
    if not fConnectionIdMap.TryGetValue(ClientID, Result) then
      Result := nil;
  finally
    fMapLock.Leave;
  end;
end;

function TCustomAsyncServerRest.SendToClient(ClientID: Integer;
  const Data: TBytes): boolean;
var
  Connection:      TWebSocketAsyncConnection;
  frame:           TWebSocketFrame;
  connectionArray: THttpServerConnectionIDDynArray;
begin
  Result := False;
  Connection := GetClientConnection(ClientID);
  if Connection = nil then
    Exit;
  try
    SetLength(frame.payload, Length(Data));
    if Length(Data) > 0 then
      Move(Data[0], frame.payload[1], Length(Data));
    frame.opcode  := focText;
    frame.content := [];
    frame.tix     := 0;
    SetLength(connectionArray, 1);
    connectionArray[0] := Connection.Handle;
    Result := WebSocketBroadcast(frame, connectionArray) > 0;
  except
    on E: Exception do
      if fOwnerComponent <> nil then
        fOwnerComponent.DoError('Send to client error: ' + E.Message);
  end;
end;

procedure TCustomAsyncServerRest.BroadcastToAll(const Data: TBytes);
var
  frame: TWebSocketFrame;
begin
  SetLength(frame.payload, Length(Data));
  if Length(Data) > 0 then
    Move(Data[0], frame.payload[1], Length(Data));
  frame.opcode  := focText;
  frame.content := [];
  frame.tix     := 0;
  WebSocketBroadcast(frame, nil);
end;

procedure TCustomAsyncServerRest.DoConnect(Context: TWebSocketAsyncConnection);
var
  ClientID: Integer;
  ClientIP: string;
begin
  if (fOwnerComponent <> nil) and
     (fOwnerComponent.fMaxConnections > 0) and
     (fOwnerComponent.fClientCount >= fOwnerComponent.fMaxConnections) then
  begin
    ClientIP := ExtractIPFromConnection(Context);
    if fOwnerComponent <> nil then
      fOwnerComponent.DoError(Format(
        'Connection REJECTED from %s: Maximum connections (%d) reached',
        [ClientIP, fOwnerComponent.fMaxConnections]));
    try
      Context.Socket.Close;
    except
    end;
    Exit;
  end;

  inherited DoConnect(Context);

  if fOwnerComponent <> nil then
  begin
    ClientID := GetConnectionId(Context);
    ClientIP := ExtractIPFromConnection(Context);
    fMapLock.Enter;
    try
      fClientIPMap.AddOrSetValue(ClientID, ClientIP);
    finally
      fMapLock.Leave;
    end;
    fOwnerComponent.OnInternalClientConnected(ClientID, Context, ClientIP);
  end;
end;

{ Fixed: previously called RemoveConnection (which acquires fMapLock) while
  already inside fMapLock — deadlock on Linux non-recursive mutexes.
  Now the map is updated inline and the user event is fired AFTER releasing the lock. }
procedure TCustomAsyncServerRest.DoDisconnect(Context: TWebSocketAsyncConnection);
var
  ClientID:  Integer;
  hasClient: Boolean;
begin
  hasClient := False;
  if fOwnerComponent <> nil then
  begin
    fMapLock.Enter;
    try
      if fConnectionMap.TryGetValue(Context, ClientID) then
      begin
        hasClient := True;
        fConnectionMap.Remove(Context);
        fConnectionIdMap.Remove(ClientID);
        fClientIPMap.Remove(ClientID);
      end;
    finally
      fMapLock.Leave;
    end;
    { Fire user event outside the lock to prevent re-entrant deadlock }
    if hasClient then
      fOwnerComponent.OnInternalClientDisconnected(ClientID);
  end;
  inherited DoDisconnect(Context);
end;

{ TmORMot2WebSocketServer }

constructor TmORMot2WebSocketServer.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  fIsDestroying  := False;
  fClientInfoMap := TDictionary<Integer, TClientConnectionInfo>.Create;
  fClientLock    := TCriticalSection.Create;
  fCryptoLock    := TCriticalSection.Create;
  fServerState   := ssIdle;
  InitializeDefaults;
end;

procedure TmORMot2WebSocketServer.InitializeDefaults;
begin
  fPort               := 8080;
  fActive             := False;
  fServer             := nil;
  fClientCount        := 0;
  fShuttingDown       := False;
  fServerThreadPoolCount := 32;
  fKeepAliveTimeOut   := 30000;
  fWebSocketsURI      := 'websocket';
  fWebSocketsAjax     := True;
  fEncryptionEnabled  := False;
  fEncryptionKey      := '';
  fEncryptionMode     := amCBC;
  fEncryptionKeySize  := aks256;
  fKeyDerived         := False;
  FillChar(fDerivedKey, SizeOf(fDerivedKey), 0);
  fConnectionTimeout  := 10;
  fDescription        := 'mORMot2 WebSocket Server Component';
  fKeepAlive          := True;
  fLogLevel           := 1000;
  fMaxConnections     := 1000;
  fNoDelay            := True;
  fReceiveBufferSize  := 8192;
  fReusePort          := True;
  fSendBufferSize     := 8192;
  fThreadPoolSize     := 4;
  fVersion            := '2.0.7';
  fTotalActiveConnections := 0;
  fTotalBytesReceived := 0;
  fTotalBytesSent     := 0;
  fTotalConnections   := 0;
  fTotalMessagesReceived := 0;
  fTotalMessagesSent  := 0;
end;

destructor TmORMot2WebSocketServer.Destroy;
begin
  { Set destroying flag FIRST so queued callbacks bail out cleanly }
  fIsDestroying := True;
  try
    if fActive then
      InternalStop;
    FreeAndNil(fClientInfoMap);
    FreeAndNil(fClientLock);
    FillChar(fDerivedKey, SizeOf(fDerivedKey), 0);
    FreeAndNil(fCryptoLock);
    fServer := nil;
  except
  end;
  inherited Destroy;
end;

function TmORMot2WebSocketServer.ServerStateToString(State: TWebSocketServerState): string;
begin
  case State of
    ssIdle:     Result := 'Idle';
    ssStarting: Result := 'Starting';
    ssListening:Result := 'Listening';
    ssStopping: Result := 'Stopping';
    ssError:    Result := 'Error';
  else
    Result := 'Unknown';
  end;
end;

function TmORMot2WebSocketServer.ClientStateToString(State: TWebSocketClientState): string;
begin
  case State of
    csDisconnected:  Result := 'Disconnected';
    csConnecting:    Result := 'Connecting';
    csConnected:     Result := 'Connected';
    csDisconnecting: Result := 'Disconnecting';
    csError:         Result := 'Error';
  else
    Result := 'Unknown';
  end;
end;

function TmORMot2WebSocketServer.GetServerStateDescription: string;
begin
  Result := Format('Server is %s on port %d', [ServerStateToString(fServerState), fPort]);
end;

function TmORMot2WebSocketServer.GetServerStateAsString: string;
begin
  Result := ServerStateToString(fServerState);
end;

function TmORMot2WebSocketServer.GetClientStateAsString(ClientID: Integer): string;
var
  clientInfo: TClientConnectionInfo;
begin
  if fClientInfoMap.TryGetValue(ClientID, clientInfo) then
    Result := ClientStateToString(clientInfo.State)
  else
    Result := 'Unknown';
end;

function TmORMot2WebSocketServer.GetAllClientStates: string;
var
  clientID:   Integer;
  clientInfo: TClientConnectionInfo;
  states:     TStringList;
begin
  states := TStringList.Create;
  try
    fClientLock.Enter;
    try
      for clientID in fClientInfoMap.Keys do
        if fClientInfoMap.TryGetValue(clientID, clientInfo) then
          states.Add(Format('Client #%d (%s): %s',
            [clientID, clientInfo.IP, ClientStateToString(clientInfo.State)]));
    finally
      fClientLock.Leave;
    end;
    if states.Count > 0 then
      Result := states.Text
    else
      Result := 'No active clients';
  finally
    states.Free;
  end;
end;

procedure TmORMot2WebSocketServer.SetServerState(NewState: TWebSocketServerState);
var
  OldState: TWebSocketServerState;
begin
  OldState := fServerState;
  if OldState <> NewState then
  begin
    fServerState := NewState;
    DoServerStateChange(OldState, NewState);
  end;
end;

procedure TmORMot2WebSocketServer.SetClientState(ClientID: Integer;
  NewState: TWebSocketClientState);
var
  clientInfo: TClientConnectionInfo;
begin
  fClientLock.Enter;
  try
    if fClientInfoMap.TryGetValue(ClientID, clientInfo) then
    begin
      if clientInfo.State <> NewState then
      begin
        clientInfo.State := NewState;
        fClientInfoMap.AddOrSetValue(ClientID, clientInfo);
      end;
    end;
  finally
    fClientLock.Leave;
  end;
end;

function TmORMot2WebSocketServer.CanAcceptNewConnection: Boolean;
begin
  Result := (fMaxConnections <= 0) or (fClientCount < fMaxConnections);
end;

function TmORMot2WebSocketServer.GetConnectionsRemaining: Integer;
begin
  if fMaxConnections <= 0 then
    Result := -1
  else
    Result := fMaxConnections - fClientCount;
end;

function TmORMot2WebSocketServer.GetClientIP(ClientID: Integer): string;
var
  clientInfo: TClientConnectionInfo;
begin
  fClientLock.Enter;
  try
    if fClientInfoMap.TryGetValue(ClientID, clientInfo) then
      Result := clientInfo.IP
    else if fServer <> nil then
      Result := fServer.GetStoredClientIP(ClientID)
    else
      Result := 'Unknown';
  finally
    fClientLock.Leave;
  end;
end;

function TmORMot2WebSocketServer.GetClientEndpoint(ClientID: Integer): string;
begin
  Result := Format('Client #%d (%s)', [ClientID, GetClientIP(ClientID)]);
end;

function TmORMot2WebSocketServer.GetClientInfo(ClientID: Integer): string;
var
  clientInfo: TClientConnectionInfo;
begin
  fClientLock.Enter;
  try
    if fClientInfoMap.TryGetValue(ClientID, clientInfo) then
      Result := Format('Client #%d - IP: %s - State: %s - Connected: %s - Encryption: %s',
        [ClientID, clientInfo.IP, ClientStateToString(clientInfo.State),
         DateTimeToStr(clientInfo.ConnectedAt), GetEncryptionInfo])
    else
      Result := Format('Client #%d not found', [ClientID]);
  finally
    fClientLock.Leave;
  end;
end;

procedure TmORMot2WebSocketServer.ResetStatistics;
begin
  fClientLock.Enter;
  try
    fTotalBytesReceived    := 0;
    fTotalBytesSent        := 0;
    fTotalConnections      := 0;
    fTotalMessagesReceived := 0;
    fTotalMessagesSent     := 0;
    fTotalActiveConnections := fClientCount;
  finally
    fClientLock.Leave;
  end;
end;

{ Derives (or re-derives) the AES key from fEncryptionKey using PBKDF2-SHA256.
  Called whenever EncryptionKey, EncryptionMode, or EncryptionKeySize changes.
  Must be called with fCryptoLock held OR before the server is accessible from
  other threads (e.g. during property-setter calls on the main thread). }
procedure TmORMot2WebSocketServer.DeriveEncryptionKey;
var
  Password, Salt: RawByteString;
begin
  FillChar(fDerivedKey, SizeOf(fDerivedKey), 0);
  fKeyDerived := False;
  if fEncryptionKey = '' then Exit;
  Password := ToUtf8(fEncryptionKey);
  Salt     := ToUtf8(fEncryptionKey + '_mormot2_ws_salt');
  Pbkdf2HmacSha256(Password, Salt, 1024, fDerivedKey);
  fKeyDerived := True;
end;

function TmORMot2WebSocketServer.GetEncryptionInfo: string;
const
  ModeNames:    array[TAESMode]   of string = ('ECB','CBC','CFB','OFB','CTR','GCM','CFC','OFC','CTC');
  KeySizeNames: array[TAESKeySize] of string = ('128','192','256');
begin
  if fEncryptionEnabled and (fEncryptionKey <> '') then
    Result := Format('AES-%s-%s', [KeySizeNames[fEncryptionKeySize], ModeNames[fEncryptionMode]])
  else if fEncryptionEnabled then
    Result := 'ENABLED BUT NO KEY SET'
  else
    Result := 'DISABLED';
end;

function TmORMot2WebSocketServer.EncryptData(const Data: TBytes;
  const ClientIP: string): TBytes;
var
  DKey:     THash256;
  Mode:     TAESMode;
  KSize:    TAESKeySize;
  KSBits:   Integer;
  DataStr:  RawByteString;
  EncStr:   RawByteString;
  AES:      TAesAbstract;
  Header:   array[0..ENCRYPTION_HEADER_SIZE-1] of Byte;
  Final:    TBytes;
begin
  Result := Data;
  if not fEncryptionEnabled then Exit;
  { Grab a snapshot of crypto state under the lock so the main thread can
    change EncryptionKey/Mode/KeySize concurrently without data races. }
  fCryptoLock.Enter;
  try
    if not fKeyDerived then Exit;
    DKey  := fDerivedKey;
    Mode  := fEncryptionMode;
    KSize := fEncryptionKeySize;
  finally
    fCryptoLock.Leave;
  end;
  case KSize of
    aks128: KSBits := 128;
    aks192: KSBits := 192;
    aks256: KSBits := 256;
  else      KSBits := 256;
  end;
  SetLength(DataStr, Length(Data));
  if Length(Data) > 0 then
    Move(Data[0], DataStr[1], Length(Data));
  try
    case Mode of
      amECB: AES := TAesEcb.Create(DKey, KSBits);
      amCBC: AES := TAesCbc.Create(DKey, KSBits);
      amCFB: AES := TAesCfb.Create(DKey, KSBits);
      amOFB: AES := TAesOfb.Create(DKey, KSBits);
      amCTR: AES := TAesCtr.Create(DKey, KSBits);
      amGCM: AES := TAesGcm.Create(DKey, KSBits);
      amCFC: AES := TAesCfc.Create(DKey, KSBits);
      amOFC: AES := TAesOfc.Create(DKey, KSBits);
      amCTC: AES := TAesCtc.Create(DKey, KSBits);
    else    AES := TAesCbc.Create(DKey, KSBits);
    end;
    try
      { EncryptPkcs7(data, True) prepends a random 16-byte IV automatically }
      EncStr := AES.EncryptPkcs7(DataStr, True);
      FillChar(Header, SizeOf(Header), 0);
      PCardinal(@Header[0])^ := ENCRYPTION_MAGIC;
      PCardinal(@Header[4])^ := Cardinal(Mode);
      PCardinal(@Header[8])^ := Cardinal(KSize);
      SetLength(Final, ENCRYPTION_HEADER_SIZE + Length(EncStr));
      Move(Header[0], Final[0], ENCRYPTION_HEADER_SIZE);
      if Length(EncStr) > 0 then
        Move(EncStr[1], Final[ENCRYPTION_HEADER_SIZE], Length(EncStr));
      Result := Final;
    finally
      AES.Free;
    end;
  except
    Result := Data;  { encryption failed — send raw rather than crashing }
  end;
  FillChar(DKey, SizeOf(DKey), 0);
end;

function TmORMot2WebSocketServer.DecryptData(const Data: TBytes;
  const ClientIP: string): TBytes;
var
  DKey:    THash256;
  Header:  array[0..ENCRYPTION_HEADER_SIZE-1] of Byte;
  Magic:   Cardinal;
  Mode:    TAESMode;
  KSize:   TAESKeySize;
  KSBits:  Integer;
  EncPart: RawByteString;
  DataStr: RawByteString;
  AES:     TAesAbstract;
begin
  { Default: return the raw bytes unchanged.  This is what callers see when
    encryption is off, when no key has been derived yet, when the data has no
    encryption header (peer sent plaintext), or when AES fails (wrong key). }
  Result := Data;
  if not fEncryptionEnabled then Exit;
  fCryptoLock.Enter;
  try
    if not fKeyDerived then Exit;
    DKey := fDerivedKey;
  finally
    fCryptoLock.Leave;
  end;
  if Length(Data) < ENCRYPTION_HEADER_SIZE then Exit;
  { Check magic — no error event; peer may have sent an unencrypted frame }
  Move(Data[0], Header[0], ENCRYPTION_HEADER_SIZE);
  Magic := PCardinal(@Header[0])^;
  if Magic <> ENCRYPTION_MAGIC then Exit;
  { Read mode and key-size from the header written by the sender.
    We use whatever the sender specified, not our own current settings,
    so decryption works even after the receiver changes mode on the fly. }
  Mode  := TAESMode(PCardinal(@Header[4])^);
  KSize := TAESKeySize(PCardinal(@Header[8])^);
  SetLength(EncPart, Length(Data) - ENCRYPTION_HEADER_SIZE);
  if Length(EncPart) > 0 then
    Move(Data[ENCRYPTION_HEADER_SIZE], EncPart[1], Length(EncPart));
  case KSize of
    aks128: KSBits := 128;
    aks192: KSBits := 192;
    aks256: KSBits := 256;
  else      KSBits := 256;
  end;
  try
    case Mode of
      amECB: AES := TAesEcb.Create(DKey, KSBits);
      amCBC: AES := TAesCbc.Create(DKey, KSBits);
      amCFB: AES := TAesCfb.Create(DKey, KSBits);
      amOFB: AES := TAesOfb.Create(DKey, KSBits);
      amCTR: AES := TAesCtr.Create(DKey, KSBits);
      amGCM: AES := TAesGcm.Create(DKey, KSBits);
      amCFC: AES := TAesCfc.Create(DKey, KSBits);
      amOFC: AES := TAesOfc.Create(DKey, KSBits);
      amCTC: AES := TAesCtc.Create(DKey, KSBits);
    else    AES := TAesCbc.Create(DKey, KSBits);
    end;
    try
      DataStr := AES.DecryptPkcs7(EncPart, True);
      SetLength(Result, Length(DataStr));
      if Length(DataStr) > 0 then
        Move(DataStr[1], Result[0], Length(DataStr));
    finally
      AES.Free;
    end;
  except
    Result := Data;  { wrong key or corrupt — return raw bytes, no error event }
  end;
  FillChar(DKey, SizeOf(DKey), 0);
end;

function TmORMot2WebSocketServer.Start: boolean;
begin
  Result := False;
  try
    InternalStart;
    Result := True;
  except
    on E: Exception do
    begin
      fLastError := E.Message;
      DoError(E.Message);
    end;
  end;
end;

procedure TmORMot2WebSocketServer.InternalStart;
var
  Protocol: TCustomWebSocketServerProtocol;
begin
  if fActive then
    Exit;
  try
    SetServerState(ssStarting);
    if fMaxConnections <= 0 then
    begin
      DoError('MaxConnections must be greater than 0');
      raise Exception.Create('Invalid MaxConnections value');
    end;
    InternalStop;
    fLastError := '';
    fClientLock.Enter;
    try
      fClientInfoMap.Clear;
      fClientCount := 0;
    finally
      fClientLock.Leave;
    end;
    { Pass empty URI/key so the parent constructor does NOT auto-register a
      TWebSocketProtocolJson named 'Json'. If we let it register one and then
      call WebSocketProtocols.AddOnce(Protocol), AddOnce finds 'Json' already
      present and silently frees our custom protocol without adding it.
      Result: the built-in protocol handles all connections and our
      ProcessIncomingFrame override never fires — server receives nothing.
      With empty URI, we are the sole 'Json' protocol; AddOnce adds us;
      empty URI acts as a catch-all and matches any client connection path. }
    fServer := TCustomAsyncServerRest.Create(
      ToUtf8(IntToStr(fPort)), nil, nil,
      ToUtf8('mORMot2WebSocketServer'), fServerThreadPoolCount,
      '', '', fWebSocketsAjax, []);
    fServer.OwnerComponent := Self;
    Protocol := TCustomWebSocketServerProtocol.Create(Self);
    fServer.WebSocketProtocols.AddOnce(Protocol);
    fServer.WaitStarted(10000);
    fActive := True;
    SetServerState(ssListening);
  except
    on E: Exception do
    begin
      fLastError := E.Message;
      SetServerState(ssError);
      if fServer <> nil then
      begin
        try fServer.Shutdown; except end;
        FreeAndNil(fServer);
      end;
      fActive := False;
      raise;
    end;
  end;
end;

procedure TmORMot2WebSocketServer.Stop;
begin
  if fShuttingDown then
    Exit;
  fShuttingDown := True;
  try
    InternalStop;
  finally
    fShuttingDown := False;
  end;
end;

procedure TmORMot2WebSocketServer.InternalStop;
begin
  if not fActive then
    Exit;
  SetServerState(ssStopping);
  fActive := False;
  if fServer <> nil then
  begin
    try fServer.Shutdown; except end;
    FreeAndNil(fServer);
  end;
  fClientLock.Enter;
  try
    fClientInfoMap.Clear;
    fClientCount            := 0;
    fTotalActiveConnections := 0;
  finally
    fClientLock.Leave;
  end;
  SetServerState(ssIdle);
end;

{ Called from mORMot2's async thread. Updates the map on the calling thread
  (fClientLock protects it), then posts the user event to the main thread
  via Application.QueueAsyncCall to avoid LCL threading violations. }
procedure TmORMot2WebSocketServer.OnInternalClientConnected(ClientID: Integer;
  Connection: TWebSocketAsyncConnection; const ClientIP: string);
var
  clientInfo: TClientConnectionInfo;
begin
  fClientLock.Enter;
  try
    clientInfo.ClientID    := ClientID;
    clientInfo.Connection  := Connection;
    clientInfo.IP          := ClientIP;
    clientInfo.ConnectedAt := Now;
    clientInfo.State       := csConnected;
    fClientInfoMap.AddOrSetValue(ClientID, clientInfo);
    Inc(fClientCount);
    Inc(fTotalActiveConnections);
    Inc(fTotalConnections);
  finally
    fClientLock.Leave;
  end;

  if not fIsDestroying and Assigned(fOnClientConnected) then
  begin
    with TClientEventRunner.Create do
    begin
      fServer    := Self;
      fClientID  := ClientID;
      fIsConnect := True;
      Application.QueueAsyncCall(Run, 0);
    end;
  end;
end;

procedure TmORMot2WebSocketServer.OnInternalClientDisconnected(ClientID: Integer);
begin
  fClientLock.Enter;
  try
    if fClientInfoMap.ContainsKey(ClientID) then
    begin
      fClientInfoMap.Remove(ClientID);
      Dec(fClientCount);
      Dec(fTotalActiveConnections);
      if fClientCount            < 0 then fClientCount            := 0;
      if fTotalActiveConnections < 0 then fTotalActiveConnections := 0;
    end;
  finally
    fClientLock.Leave;
  end;

  if not fIsDestroying and Assigned(fOnClientDisconnected) then
  begin
    with TClientEventRunner.Create do
    begin
      fServer    := Self;
      fClientID  := ClientID;
      fIsConnect := False;
      Application.QueueAsyncCall(Run, 0);
    end;
  end;
end;

{ Signature changed: RawData + DecData passed directly from ProcessIncomingFrame
  instead of relying on a shared field. }
procedure TmORMot2WebSocketServer.OnInternalDataReceived(ClientID: Integer;
  const RawData: TBytes; const DecData: TBytes);
begin
  if fIsDestroying then Exit;
  with TDataEventRunner.Create do
  begin
    fServer   := Self;
    fClientID := ClientID;
    fRawData  := RawData;
    fDecData  := DecData;
    Application.QueueAsyncCall(Run, 0);
  end;
end;

function TmORMot2WebSocketServer.SendCommandToClient(ClientID: Integer;
  const Command: TBytes): boolean;
var
  clientInfo: TClientConnectionInfo;
  dataToSend: TBytes;
begin
  Result := False;
  if (fServer = nil) or not fActive then
    Exit;
  fClientLock.Enter;
  try
    if not fClientInfoMap.TryGetValue(ClientID, clientInfo) then
      Exit;
  finally
    fClientLock.Leave;
  end;
  try
    dataToSend := Command;
    if fEncryptionEnabled then
      dataToSend := EncryptData(Command, clientInfo.IP);
    Result := fServer.SendToClient(ClientID, dataToSend);
    if Result then
    begin
      Inc(fTotalBytesSent, Length(dataToSend));
      Inc(fTotalMessagesSent);
      if Assigned(fOnDataSent) then
        fOnDataSent(Self, ClientID, dataToSend);
    end;
  except
    on E: Exception do
    begin
      fLastError := E.Message;
      DoError('Send to client ' + IntToStr(ClientID) + ' failed: ' + E.Message);
    end;
  end;
end;

procedure TmORMot2WebSocketServer.BroadcastCommand(const Command: TBytes);
var
  ClientID:   Integer;
  clientInfo: TClientConnectionInfo;
  dataToSend: TBytes;
begin
  if (fServer = nil) or not fActive then
    Exit;
  fClientLock.Enter;
  try
    for ClientID in fClientInfoMap.Keys do
    begin
      if fClientInfoMap.TryGetValue(ClientID, clientInfo) then
      begin
        try
          dataToSend := Command;
          if fEncryptionEnabled then
            dataToSend := EncryptData(Command, clientInfo.IP);
          if fServer.SendToClient(ClientID, dataToSend) then
          begin
            Inc(fTotalBytesSent, Length(dataToSend));
            Inc(fTotalMessagesSent);
            if Assigned(fOnDataSent) then
              fOnDataSent(Self, ClientID, dataToSend);
          end;
        except
          on E: Exception do
            DoError('Broadcast to client ' + IntToStr(ClientID) + ' failed: ' + E.Message);
        end;
      end;
    end;
  finally
    fClientLock.Leave;
  end;
end;

function TmORMot2WebSocketServer.IsActive: boolean;
begin
  Result := fActive and (fServer <> nil);
end;

function TmORMot2WebSocketServer.GetLastError: string;
begin
  Result := fLastError;
end;

function TmORMot2WebSocketServer.GetClientCount: Integer;
begin
  Result := fClientCount;
end;

function TmORMot2WebSocketServer.GetServerStats: string;
var
  ConnectionInfo: string;
begin
  if fMaxConnections > 0 then
    ConnectionInfo := Format('%d/%d', [fClientCount, fMaxConnections])
  else
    ConnectionInfo := Format('%d/unlimited', [fClientCount]);
  Result := Format(
    'State: %s, Active: %s, Connections: %s, Total: %d, Msgs Sent: %d, Msgs Rcvd: %d, Bytes Sent: %d, Bytes Rcvd: %d, Encryption: %s',
    [ServerStateToString(fServerState), BoolToStr(fActive, True), ConnectionInfo,
     fTotalConnections, fTotalMessagesSent, fTotalMessagesReceived,
     fTotalBytesSent, fTotalBytesReceived, GetEncryptionInfo]);
end;

procedure TmORMot2WebSocketServer.SetActive(const Value: boolean);
begin
  if fActive <> Value then
  begin
    if Value then Start else Stop;
  end;
end;

procedure TmORMot2WebSocketServer.SetPort(const Value: Integer);
begin
  if fPort <> Value then
  begin
    if fActive then
      raise Exception.Create('Cannot change Port while server is active');
    fPort := Value;
  end;
end;

procedure TmORMot2WebSocketServer.SetServerThreadPoolCount(const Value: Integer);
begin
  if fServerThreadPoolCount <> Value then
  begin
    if fActive then
      raise Exception.Create('Cannot change thread pool size while server is active');
    fServerThreadPoolCount := Value;
  end;
end;

procedure TmORMot2WebSocketServer.SetKeepAliveTimeOut(const Value: Integer);
begin fKeepAliveTimeOut := Value; end;

procedure TmORMot2WebSocketServer.SetWebSocketsURI(const Value: string);
begin fWebSocketsURI := Value; end;

procedure TmORMot2WebSocketServer.SetWebSocketsAjax(const Value: boolean);
begin fWebSocketsAjax := Value; end;

procedure TmORMot2WebSocketServer.SetEncryptionEnabled(const Value: boolean);
begin
  fCryptoLock.Enter;
  try
    fEncryptionEnabled := Value;
    DeriveEncryptionKey;
  finally
    fCryptoLock.Leave;
  end;
end;

procedure TmORMot2WebSocketServer.SetEncryptionKey(const Value: string);
begin
  fCryptoLock.Enter;
  try
    fEncryptionKey := Value;
    DeriveEncryptionKey;
  finally
    fCryptoLock.Leave;
  end;
end;

procedure TmORMot2WebSocketServer.SetEncryptionMode(const Value: TAESMode);
begin
  fCryptoLock.Enter;
  try
    fEncryptionMode := Value;
    DeriveEncryptionKey;
  finally
    fCryptoLock.Leave;
  end;
end;

procedure TmORMot2WebSocketServer.SetEncryptionKeySize(const Value: TAESKeySize);
begin
  fCryptoLock.Enter;
  try
    fEncryptionKeySize := Value;
    DeriveEncryptionKey;
  finally
    fCryptoLock.Leave;
  end;
end;

procedure TmORMot2WebSocketServer.SetConnectionTimeout(const Value: Integer);
begin fConnectionTimeout := Value; end;

procedure TmORMot2WebSocketServer.SetDescription(const Value: string);
begin fDescription := Value; end;

procedure TmORMot2WebSocketServer.SetKeepAlive(const Value: Boolean);
begin fKeepAlive := Value; end;

procedure TmORMot2WebSocketServer.SetLogLevel(const Value: Integer);
begin fLogLevel := Value; end;

procedure TmORMot2WebSocketServer.SetMaxConnections(const Value: Integer);
begin
  if Value <= 0 then
    raise Exception.Create('MaxConnections must be greater than 0');
  if fMaxConnections <> Value then
  begin
    if fActive and (Value < fClientCount) then
      raise Exception.Create(Format(
        'Cannot set MaxConnections to %d - currently have %d active connections',
        [Value, fClientCount]));
    fMaxConnections := Value;
  end;
end;

procedure TmORMot2WebSocketServer.SetNoDelay(const Value: Boolean);
begin fNoDelay := Value; end;

procedure TmORMot2WebSocketServer.SetReceiveBufferSize(const Value: Integer);
begin fReceiveBufferSize := Value; end;

procedure TmORMot2WebSocketServer.SetReusePort(const Value: Boolean);
begin fReusePort := Value; end;

procedure TmORMot2WebSocketServer.SetSendBufferSize(const Value: Integer);
begin fSendBufferSize := Value; end;

procedure TmORMot2WebSocketServer.SetThreadPoolSize(const Value: Integer);
begin fThreadPoolSize := Value; end;

procedure TmORMot2WebSocketServer.SetVersion(const Value: string);
begin fVersion := Value; end;

procedure TmORMot2WebSocketServer.DoError(const ErrorMsg: string);
begin
  if Assigned(fOnError) then
    fOnError(Self, ErrorMsg);
end;

procedure TmORMot2WebSocketServer.DoServerStateChange(OldState,
  NewState: TWebSocketServerState);
begin
  if Assigned(fOnServerStateChange) then
    fOnServerStateChange(Self, OldState, NewState, GetServerStateDescription);
end;

procedure Register;
begin
  RegisterComponents('mORMot2 WebSocket', [TmORMot2WebSocketServer]);
end;

end.
