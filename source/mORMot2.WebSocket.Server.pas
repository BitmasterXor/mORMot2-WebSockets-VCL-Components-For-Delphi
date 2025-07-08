unit mORMot2.WebSocket.Server;

interface

uses
  Classes,
  SysUtils,
  System.Generics.Collections,
  SyncObjs,
  TypInfo,
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
  // ENCRYPTION COORDINATION CONSTANTS - SAME AS CLIENT!
  ENCRYPTION_HEADER_SIZE = 16;
  // 4 bytes magic + 4 bytes mode + 4 bytes keysize + 4 bytes reserved
  ENCRYPTION_MAGIC = $4D4F524D; // "MORM" in hex

type
  /// Human-readable server states
  TWebSocketServerState = (ssIdle, // Server is idle/stopped
    ssStarting, // Server is starting up
    ssListening, // Server is listening for connections
    ssStopping, // Server is shutting down
    ssError // Server encountered an error
    );

  /// Human-readable client connection states (server-side view)
  TWebSocketClientState = (csDisconnected, // Client is disconnected
    csConnecting, // Client is establishing connection
    csConnected, // Client is connected and active
    csDisconnecting, // Client is disconnecting
    csError // Client connection has an error
    );

  /// AES Encryption Mode enumeration - SAME AS CLIENT!
  TAESMode = (amECB, amCBC, amCFB, amOFB, amCTR, amGCM, amCFC, amOFC, amCTC);

  /// AES Key Size enumeration - SAME AS CLIENT!
  TAESKeySize = (aks128, aks192, aks256);

  /// Forward declarations
  TmORMot2WebSocketServer = class;

  /// FIXED EVENTS - OnDataReceived/OnDataSent are for STATISTICS ONLY!
  TWebSocketClientConnectedEvent = procedure(Sender: TObject; ClientID: Integer)
    of object;
  TWebSocketClientDisconnectedEvent = procedure(Sender: TObject;
    ClientID: Integer) of object;
  TWebSocketErrorEvent = procedure(Sender: TObject; const ErrorMsg: string)
    of object;
  TWebSocketDataReceivedEvent = procedure(Sender: TObject; ClientID: Integer;
    const Data: TBytes) of object;  // FOR STATISTICS/LOGGING ONLY!
  TWebSocketDataSentEvent = procedure(Sender: TObject; ClientID: Integer;
    const Data: TBytes) of object;  // FOR STATISTICS/LOGGING ONLY!
  TWebSocketHandleCommandEvent = procedure(Sender: TObject; ClientID: Integer;
    const Command: TBytes) of object;  // THIS IS WHERE YOU PROCESS DATA!

  /// STATE CHANGE EVENTS
  TWebSocketServerStateChangeEvent = procedure(Sender: TObject;
    OldState, NewState: TWebSocketServerState; const StateDescription: string)
    of object;

  /// Custom WebSocket Async Process with public connection access
  TCustomWebSocketAsyncProcess = class(TWebSocketAsyncProcess)
  public
    /// Public access to the connection
    property Connection: TWebSocketAsyncConnection read fConnection;
  end;

  /// FIXED: Custom WebSocket Server Protocol using mORMot2 async approach
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

  /// FIXED: Custom async server that properly tracks connections AND ENFORCES MAX CONNECTIONS
  TCustomAsyncServerRest = class(TWebSocketAsyncServerRest)
  private
    fOwnerComponent: TmORMot2WebSocketServer;
    fConnectionMap: TDictionary<TWebSocketAsyncConnection, Integer>;
    fConnectionIdMap: TDictionary<Integer, TWebSocketAsyncConnection>;
    fClientIPMap: TDictionary<Integer, string>; // Store client IPs immediately
    fNextConnectionId: Integer;
    fMapLock: TCriticalSection;
  protected
    procedure DoConnect(Context: TWebSocketAsyncConnection); override;
    procedure DoDisconnect(Context: TWebSocketAsyncConnection); override;
    function GetConnectionId(Connection: TWebSocketAsyncConnection): Integer;
    procedure RemoveConnection(Connection: TWebSocketAsyncConnection);
    function GetClientIP(Connection: TWebSocketAsyncConnection): string;
    function ExtractIPFromConnection(Connection: TWebSocketAsyncConnection): string;
  public
    constructor Create(const aPort: RawUtf8;
      const OnStart, OnStop: TOnNotifyThread; const aProcessName: RawUtf8;
      ServerThreadPoolCount: Integer;
      const aWebSocketsURI, aWebSocketsEncryptionKey: RawUtf8;
      aWebSocketsAjax: boolean; ProcessOptions: THttpServerOptions);
    destructor Destroy; override;

    // Send data to specific client using mORMot2's WebSocketBroadcast
    function SendToClient(ClientID: Integer; const Data: TBytes): boolean;
    procedure BroadcastToAll(const Data: TBytes);
    function GetClientConnection(ClientID: Integer): TWebSocketAsyncConnection;
    function GetStoredClientIP(ClientID: Integer): string;

    // Override to use our custom process class
    property ProcessClass: TWebSocketAsyncProcessClass read fProcessClass
      write fProcessClass;
    property OwnerComponent: TmORMot2WebSocketServer read fOwnerComponent
      write fOwnerComponent;
  end;

  /// Client connection info for tracking
  TClientConnectionInfo = record
    ClientID: Integer;
    Connection: TWebSocketAsyncConnection;
    IP: string;
    ConnectedAt: TDateTime;
    State: TWebSocketClientState;
  end;

  /// COMPLETE WebSocket Server Component - ALL FEATURES FROM OLD + NEW ASYNC!
  TmORMot2WebSocketServer = class(TComponent)
  private
    fServer: TCustomAsyncServerRest;

    // Core properties (EXISTING - DON'T BREAK)
    fPort: Integer;
    fActive: boolean;
    fLastError: string;
    fClientCount: Integer;
    fShuttingDown: boolean;

    // ASYNC SERVER CONFIGURATION
    fServerThreadPoolCount: Integer;
    fKeepAliveTimeOut: Integer;
    fWebSocketsURI: string;
    fWebSocketsAjax: boolean;

    // Client tracking
    fClientInfoMap: TDictionary<Integer, TClientConnectionInfo>;
    fClientLock: TCriticalSection;
    fServerState: TWebSocketServerState;

    // ENCRYPTION PROPERTIES - SAME AS CLIENT!
    fEncryptionEnabled: boolean;
    fEncryptionKey: string;
    fEncryptionMode: TAESMode;
    fEncryptionKeySize: TAESKeySize;

    // ALL ADDITIONAL PROPERTIES FROM OLD SERVER
    fConnectionTimeout: Integer;
    fDescription: string;
    fKeepAlive: Boolean;
    fLogLevel: Integer;
    fMaxConnections: Integer;
    fName: string;
    fNoDelay: Boolean;
    fReceiveBufferSize: Integer;
    fReusePort: Boolean;
    fSendBufferSize: Integer;
    fTag: Integer;
    fThreadPoolSize: Integer;
    fVersion: string;

    // Statistics
    fTotalActiveConnections: Integer;
    fTotalBytesReceived: Int64;
    fTotalBytesSent: Int64;
    fTotalConnections: Int64;
    fTotalMessagesReceived: Int64;
    fTotalMessagesSent: Int64;

    // FIXED EVENTS - OnDataReceived/OnDataSent are for STATISTICS ONLY!
    fOnClientConnected: TWebSocketClientConnectedEvent;
    fOnClientDisconnected: TWebSocketClientDisconnectedEvent;
    fOnError: TWebSocketErrorEvent;
    fOnDataReceived: TWebSocketDataReceivedEvent;  // FOR STATISTICS/LOGGING ONLY!
    fOnDataSent: TWebSocketDataSentEvent;  // FOR STATISTICS/LOGGING ONLY!
    fOnHandleCommand: TWebSocketHandleCommandEvent;  // THIS IS WHERE YOU PROCESS DATA!
    fOnServerStateChange: TWebSocketServerStateChangeEvent;

    // Helper methods
    function ServerStateToString(State: TWebSocketServerState): string;
    function ClientStateToString(State: TWebSocketClientState): string;
    function GetServerStateDescription: string;
    function GetClientStateDescription(ClientID: Integer): string;
    procedure SetServerState(NewState: TWebSocketServerState);
    procedure SetClientState(ClientID: Integer; NewState: TWebSocketClientState);
    function GetEncryptionInfo: string;

    // FIXED: Add encryption methods matching client exactly
    function EncryptData(const Data: TBytes; const ClientIP: string): TBytes;
    function DecryptData(const Data: TBytes; const ClientIP: string): TBytes;

    // Core setters (DON'T BREAK)
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

    // ALL ADDITIONAL PROPERTY SETTERS FROM OLD SERVER
    procedure SetConnectionTimeout(const Value: Integer);
    procedure SetDescription(const Value: string);
    procedure SetKeepAlive(const Value: Boolean);
    procedure SetLogLevel(const Value: Integer);
    procedure SetMaxConnections(const Value: Integer);
    procedure SetName(const Value: string);
    procedure SetNoDelay(const Value: Boolean);
    procedure SetReceiveBufferSize(const Value: Integer);
    procedure SetReusePort(const Value: Boolean);
    procedure SetSendBufferSize(const Value: Integer);
    procedure SetTag(const Value: Integer);
    procedure SetThreadPoolSize(const Value: Integer);
    procedure SetVersion(const Value: string);

    // Event triggers
    procedure DoError(const ErrorMsg: string);
    procedure DoServerStateChange(OldState, NewState: TWebSocketServerState);

  protected
    procedure InternalStart;
    procedure InternalStop;
    procedure InitializeDefaults;
    procedure UpdateStatistics;

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    // Main methods (DON'T BREAK)
    function Start: boolean;
    procedure Stop;
    function SendCommandToClient(ClientID: Integer;
      const Command: TBytes): boolean;
    procedure BroadcastCommand(const Command: TBytes);
    function IsActive: boolean;
    function GetLastError: string;
    function GetClientCount: Integer;
    function GetServerStats: string;

    // ALL ADDITIONAL METHODS FROM OLD SERVER
    procedure ResetStatistics;
    function GetClientInfo(ClientID: Integer): string;
    function GetClientIP(ClientID: Integer): string;
    function GetClientEndpoint(ClientID: Integer): string;
    function GetServerStateAsString: string;
    function GetClientStateAsString(ClientID: Integer): string;
    function GetAllClientStates: string;

    // NEW CONNECTION LIMIT METHODS
    function CanAcceptNewConnection: Boolean;
    function GetConnectionsRemaining: Integer;

    // FIXED: Internal event handlers called by custom server and protocol
    procedure OnInternalClientConnected(ClientID: Integer;
      Connection: TWebSocketAsyncConnection; const ClientIP: string);
    procedure OnInternalClientDisconnected(ClientID: Integer);
    procedure OnInternalDataReceived(ClientID: Integer; const Data: TBytes);

  published
    // Core properties (DON'T BREAK)
    property Port: Integer read fPort write SetPort default 8080;
    property Active: boolean read fActive write SetActive default False;

    // Server configuration
    property ServerThreadPoolCount: Integer read fServerThreadPoolCount
      write SetServerThreadPoolCount default 32;
    property KeepAliveTimeOut: Integer read fKeepAliveTimeOut
      write SetKeepAliveTimeOut default 30000;
    property WebSocketsURI: string read fWebSocketsURI write SetWebSocketsURI;
    property WebSocketsAjax: boolean read fWebSocketsAjax
      write SetWebSocketsAjax default True;

    // State properties
    property ServerState: TWebSocketServerState read fServerState;
    property ServerStateDescription: string read GetServerStateDescription;

    // Encryption properties - SAME AS CLIENT!
    property EncryptionEnabled: boolean read fEncryptionEnabled
      write SetEncryptionEnabled default False;
    property EncryptionKey: string read fEncryptionKey write SetEncryptionKey;
    property EncryptionMode: TAESMode read fEncryptionMode
      write SetEncryptionMode default amCBC;
    property EncryptionKeySize: TAESKeySize read fEncryptionKeySize
      write SetEncryptionKeySize default aks256;
    property EncryptionInfo: string read GetEncryptionInfo;

    // ALL ADDITIONAL PROPERTIES FROM OLD SERVER
    property ConnectionTimeout: Integer read fConnectionTimeout write SetConnectionTimeout default 10;
    property Description: string read fDescription write SetDescription;
    property KeepAlive: Boolean read fKeepAlive write SetKeepAlive default True;
    property LogLevel: Integer read fLogLevel write SetLogLevel default 1000;
    property MaxConnections: Integer read fMaxConnections write SetMaxConnections default 1000;
    property Name: string read fName write SetName;
    property NoDelay: Boolean read fNoDelay write SetNoDelay default True;
    property ReceiveBufferSize: Integer read fReceiveBufferSize write SetReceiveBufferSize default 8192;
    property ReusePort: Boolean read fReusePort write SetReusePort default True;
    property SendBufferSize: Integer read fSendBufferSize write SetSendBufferSize default 8192;
    property Tag: Integer read fTag write SetTag default 0;
    property ThreadPoolSize: Integer read fThreadPoolSize write SetThreadPoolSize default 4;
    property Version: string read fVersion write SetVersion;

    // Statistics
    property TotalActiveConnections: Integer read fTotalActiveConnections;
    property TotalBytesReceived: Int64 read fTotalBytesReceived;
    property TotalBytesSent: Int64 read fTotalBytesSent;
    property TotalConnections: Int64 read fTotalConnections;
    property TotalMessagesReceived: Int64 read fTotalMessagesReceived;
    property TotalMessagesSent: Int64 read fTotalMessagesSent;

    // FIXED EVENTS - OnDataReceived/OnDataSent are for STATISTICS ONLY!
    property OnClientConnected: TWebSocketClientConnectedEvent
      read fOnClientConnected write fOnClientConnected;
    property OnClientDisconnected: TWebSocketClientDisconnectedEvent
      read fOnClientDisconnected write fOnClientDisconnected;
    property OnError: TWebSocketErrorEvent read fOnError write fOnError;
    property OnDataReceived: TWebSocketDataReceivedEvent read fOnDataReceived
      write fOnDataReceived;  // FOR STATISTICS/LOGGING ONLY!
    property OnDataSent: TWebSocketDataSentEvent read fOnDataSent
      write fOnDataSent;  // FOR STATISTICS/LOGGING ONLY!
    property OnHandleCommand: TWebSocketHandleCommandEvent read fOnHandleCommand
      write fOnHandleCommand;  // THIS IS WHERE YOU PROCESS DATA!
    property OnServerStateChange: TWebSocketServerStateChangeEvent
      read fOnServerStateChange write fOnServerStateChange;
  end;

procedure Register;

implementation

// =============================================================================
// FIXED: CUSTOM PROTOCOL IMPLEMENTATION - using proper mORMot2 approach
// =============================================================================

{ TCustomWebSocketServerProtocol }

constructor TCustomWebSocketServerProtocol.Create(AOwnerServer
  : TmORMot2WebSocketServer);
begin
  fOwnerServer := AOwnerServer;
  inherited Create('');
end;

function TCustomWebSocketServerProtocol.Clone(const aClientUri: RawUtf8)
  : TWebSocketProtocol;
begin
  Result := TCustomWebSocketServerProtocol.Create(fOwnerServer);
end;

procedure TCustomWebSocketServerProtocol.ProcessIncomingFrame
  (Sender: TWebSocketProcess; var Request: TWebSocketFrame;
  const Info: RawUtf8);
var
  rawBytes: TBytes;
  decryptedBytes: TBytes;
  ClientID: Integer;
  Connection: TWebSocketAsyncConnection;
begin
  if (fOwnerServer <> nil) and (Sender <> nil) then
  begin
    case Request.opcode of
      focText:
        begin
          // Convert payload to TBytes
          SetLength(rawBytes, Length(Request.payload));
          if Length(rawBytes) > 0 then
            Move(Request.payload[1], rawBytes[0], Length(rawBytes));

          // FIXED: Get connection info using the proper mORMot2 async approach
          if Sender is TCustomWebSocketAsyncProcess then
          begin
            Connection := TCustomWebSocketAsyncProcess(Sender).Connection;
            ClientID := fOwnerServer.fServer.GetConnectionId(Connection);
          end
          else
          begin
            // Fallback for other process types - use a default ID
            ClientID := 0;
          end;

          // FIXED: Fire OnDataReceived FIRST for statistics (with raw encrypted data)
          fOwnerServer.OnInternalDataReceived(ClientID, rawBytes);

          // FIXED: Now decrypt the data for command processing
          decryptedBytes := rawBytes;
          if fOwnerServer.fEncryptionEnabled and
            (fOwnerServer.fEncryptionKey <> '') then
          begin
            // Get the stored client IP for decryption
            decryptedBytes := fOwnerServer.DecryptData(rawBytes,
              fOwnerServer.fServer.GetStoredClientIP(ClientID));
          end;

          // FIXED: Fire OnHandleCommand for actual data processing (with decrypted data)
          if Assigned(fOwnerServer.fOnHandleCommand) then
            fOwnerServer.fOnHandleCommand(fOwnerServer, ClientID, decryptedBytes);
        end;
    end;
  end;

  inherited ProcessIncomingFrame(Sender, Request, Info);
end;

// =============================================================================
// CUSTOM ASYNC SERVER IMPLEMENTATION - FIXED IP DETECTION + MAX CONNECTIONS ENFORCEMENT
// =============================================================================

{ TCustomAsyncServerRest }

constructor TCustomAsyncServerRest.Create(const aPort: RawUtf8;
  const OnStart, OnStop: TOnNotifyThread; const aProcessName: RawUtf8;
  ServerThreadPoolCount: Integer; const aWebSocketsURI, aWebSocketsEncryptionKey
  : RawUtf8; aWebSocketsAjax: boolean; ProcessOptions: THttpServerOptions);
begin
  fNextConnectionId := 1;
  fConnectionMap := TDictionary<TWebSocketAsyncConnection, Integer>.Create;
  fConnectionIdMap := TDictionary<Integer, TWebSocketAsyncConnection>.Create;
  fClientIPMap := TDictionary<Integer, string>.Create;
  fMapLock := TCriticalSection.Create;

  // Call the parent constructor properly
  inherited Create(aPort, OnStart, OnStop, aProcessName, ServerThreadPoolCount,
    aWebSocketsURI, aWebSocketsEncryptionKey, aWebSocketsAjax, ProcessOptions);

  // Set our custom process class to expose the connection
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

function TCustomAsyncServerRest.ExtractIPFromConnection(Connection: TWebSocketAsyncConnection): string;
begin
  Result := '127.0.0.1'; // Default fallback for local connections

  if Connection = nil then
    Exit;

  try
    // Method 1: Direct RemoteIP property (ONLY reliable method in mORMot2)
    if Connection.RemoteIP <> '' then
    begin
      Result := Connection.RemoteIP;
      Exit;
    end;

    // If RemoteIP is empty, assume it's a local connection
    Result := '127.0.0.1';

  except
    on E: Exception do
    begin
      // Log the error but don't fail completely
      if fOwnerComponent <> nil then
        fOwnerComponent.DoError('IP extraction error: ' + E.Message);
      Result := '127.0.0.1';
    end;
  end;
end;

function TCustomAsyncServerRest.GetConnectionId
  (Connection: TWebSocketAsyncConnection): Integer;
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

function TCustomAsyncServerRest.GetClientIP(Connection: TWebSocketAsyncConnection): string;
var
  ClientID: Integer;
begin
  if Connection = nil then
  begin
    Result := 'unknown';
    Exit;
  end;

  fMapLock.Enter;
  try
    // First try to get from our stored mapping (most reliable)
    if fConnectionMap.TryGetValue(Connection, ClientID) then
    begin
      if fClientIPMap.TryGetValue(ClientID, Result) then
        Exit; // Found in our stored mapping
    end;
  finally
    fMapLock.Leave;
  end;

  // Fallback: extract IP directly from connection
  Result := ExtractIPFromConnection(Connection);
end;

function TCustomAsyncServerRest.GetStoredClientIP(ClientID: Integer): string;
begin
  fMapLock.Enter;
  try
    if not fClientIPMap.TryGetValue(ClientID, Result) then
      Result := 'localhost'; // Fallback
  finally
    fMapLock.Leave;
  end;
end;

procedure TCustomAsyncServerRest.RemoveConnection
  (Connection: TWebSocketAsyncConnection);
var
  ConnectionID: Integer;
begin
  fMapLock.Enter;
  try
    if fConnectionMap.TryGetValue(Connection, ConnectionID) then
    begin
      fConnectionMap.Remove(Connection);
      fConnectionIdMap.Remove(ConnectionID);
      fClientIPMap.Remove(ConnectionID); // Also remove IP mapping
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
  Connection: TWebSocketAsyncConnection;
  frame: TWebSocketFrame;
  connectionArray: THttpServerConnectionIDDynArray;
begin
  Result := False;

  Connection := GetClientConnection(ClientID);
  if Connection = nil then
    Exit;

  try
    // Convert TBytes to frame payload
    SetLength(frame.payload, Length(Data));
    if Length(Data) > 0 then
      Move(Data[0], frame.payload[1], Length(Data));

    frame.opcode := focText;
    frame.content := [];
    frame.tix := 0;

    // Use mORMot2's WebSocketBroadcast with specific connection ID
    SetLength(connectionArray, 1);
    connectionArray[0] := Connection.Handle; // Use the connection handle as ID

    Result := WebSocketBroadcast(frame, connectionArray) > 0;
  except
    on E: Exception do
    begin
      if fOwnerComponent <> nil then
        fOwnerComponent.DoError('Send to client error: ' + E.Message);
    end;
  end;
end;

procedure TCustomAsyncServerRest.BroadcastToAll(const Data: TBytes);
var
  frame: TWebSocketFrame;
begin
  // Convert TBytes to frame payload
  SetLength(frame.payload, Length(Data));
  if Length(Data) > 0 then
    Move(Data[0], frame.payload[1], Length(Data));

  frame.opcode := focText;
  frame.content := [];
  frame.tix := 0;

  // Use mORMot2's WebSocketBroadcast to all connections
  WebSocketBroadcast(frame, nil);
end;

// FIXED: ENFORCES MAX CONNECTIONS LIMIT!
procedure TCustomAsyncServerRest.DoConnect(Context: TWebSocketAsyncConnection);
var
  ClientID: Integer;
  ClientIP: string;
begin
  // CHECK MAX CONNECTIONS FIRST - REJECT IF OVER LIMIT!
  if (fOwnerComponent <> nil) and
     (fOwnerComponent.fMaxConnections > 0) and
     (fOwnerComponent.fClientCount >= fOwnerComponent.fMaxConnections) then
  begin
    // LOG THE REJECTION
    ClientIP := ExtractIPFromConnection(Context);
    if fOwnerComponent <> nil then
      fOwnerComponent.DoError(Format('Connection REJECTED from %s: Maximum connections (%d) reached',
        [ClientIP, fOwnerComponent.fMaxConnections]));

    // CLOSE THE CONNECTION IMMEDIATELY
    try
      Context.Socket.Close;
    except
      // Ignore close errors
    end;

    // DO NOT CALL INHERITED - REJECT THE CONNECTION!
    Exit;
  end;

  // Only proceed if under the limit
  inherited DoConnect(Context);

  if fOwnerComponent <> nil then
  begin
    ClientID := GetConnectionId(Context);

    // CAPTURE IP IMMEDIATELY when connection is established
    ClientIP := ExtractIPFromConnection(Context);

    // Store the IP in our mapping for later retrieval
    fMapLock.Enter;
    try
      fClientIPMap.AddOrSetValue(ClientID, ClientIP);
    finally
      fMapLock.Leave;
    end;

    fOwnerComponent.OnInternalClientConnected(ClientID, Context, ClientIP);
  end;
end;

procedure TCustomAsyncServerRest.DoDisconnect
  (Context: TWebSocketAsyncConnection);
var
  ClientID: Integer;
begin
  if fOwnerComponent <> nil then
  begin
    fMapLock.Enter;
    try
      if fConnectionMap.TryGetValue(Context, ClientID) then
      begin
        RemoveConnection(Context);
        fOwnerComponent.OnInternalClientDisconnected(ClientID);
      end;
    finally
      fMapLock.Leave;
    end;
  end;

  inherited DoDisconnect(Context);
end;

// =============================================================================
// MAIN COMPONENT IMPLEMENTATION
// =============================================================================

{ TmORMot2WebSocketServer }

constructor TmORMot2WebSocketServer.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  fClientInfoMap := TDictionary<Integer, TClientConnectionInfo>.Create;
  fClientLock := TCriticalSection.Create;
  fServerState := ssIdle;

  InitializeDefaults;
end;

procedure TmORMot2WebSocketServer.InitializeDefaults;
begin
  // Core defaults (DON'T BREAK)
  fPort := 8080;
  fActive := False;
  fServer := nil;
  fClientCount := 0;
  fShuttingDown := False;
  fServerThreadPoolCount := 32;
  fKeepAliveTimeOut := 30000;
  fWebSocketsURI := 'websocket';
  fWebSocketsAjax := True;
  fEncryptionEnabled := False;
  fEncryptionKey := '';
  fEncryptionMode := amCBC;
  fEncryptionKeySize := aks256;

  // ALL ADDITIONAL DEFAULTS FROM OLD SERVER
  fConnectionTimeout := 10;
  fDescription := 'mORMot2 WebSocket Server Component with COMPLETE FEATURE SET';
  fKeepAlive := True;
  fLogLevel := 1000;
  fMaxConnections := 1000;
  fName := 'mMServer1';
  fNoDelay := True;
  fReceiveBufferSize := 8192;
  fReusePort := True;
  fSendBufferSize := 8192;
  fTag := 0;
  fThreadPoolSize := 4;
  fVersion := '2.0.7'; // Updated version for event handling fix

  // Statistics
  fTotalActiveConnections := 0;
  fTotalBytesReceived := 0;
  fTotalBytesSent := 0;
  fTotalConnections := 0;
  fTotalMessagesReceived := 0;
  fTotalMessagesSent := 0;
end;

destructor TmORMot2WebSocketServer.Destroy;
begin
  try
    if fActive then
      InternalStop;

    FreeAndNil(fClientInfoMap);
    FreeAndNil(fClientLock);
    fServer := nil;
  except
    // Ignore cleanup errors
  end;
  inherited Destroy;
end;

// =============================================================================
// STATE HELPER METHODS (FROM OLD SERVER)
// =============================================================================

function TmORMot2WebSocketServer.ServerStateToString(State: TWebSocketServerState): string;
begin
  case State of
    ssIdle: Result := 'Idle';
    ssStarting: Result := 'Starting';
    ssListening: Result := 'Listening';
    ssStopping: Result := 'Stopping';
    ssError: Result := 'Error';
  else
    Result := 'Unknown';
  end;
end;

function TmORMot2WebSocketServer.ClientStateToString(State: TWebSocketClientState): string;
begin
  case State of
    csDisconnected: Result := 'Disconnected';
    csConnecting: Result := 'Connecting';
    csConnected: Result := 'Connected';
    csDisconnecting: Result := 'Disconnecting';
    csError: Result := 'Error';
  else
    Result := 'Unknown';
  end;
end;

function TmORMot2WebSocketServer.GetServerStateDescription: string;
begin
  Result := Format('Server is %s on port %d', [ServerStateToString(fServerState), fPort]);
end;

function TmORMot2WebSocketServer.GetClientStateDescription(ClientID: Integer): string;
var
  clientInfo: TClientConnectionInfo;
begin
  if fClientInfoMap.TryGetValue(ClientID, clientInfo) then
    Result := Format('Client #%d (%s) is %s', [ClientID, clientInfo.IP, ClientStateToString(clientInfo.State)])
  else
    Result := Format('Client #%d state unknown', [ClientID]);
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
  clientID: Integer;
  clientInfo: TClientConnectionInfo;
  states: TStringList;
begin
  states := TStringList.Create;
  try
    fClientLock.Enter;
    try
      for clientID in fClientInfoMap.Keys do
      begin
        if fClientInfoMap.TryGetValue(clientID, clientInfo) then
          states.Add(Format('Client #%d (%s): %s', [clientID, clientInfo.IP, ClientStateToString(clientInfo.State)]));
      end;
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

procedure TmORMot2WebSocketServer.SetClientState(ClientID: Integer; NewState: TWebSocketClientState);
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

// =============================================================================
// CONNECTION LIMIT METHODS - NEW!
// =============================================================================

function TmORMot2WebSocketServer.CanAcceptNewConnection: Boolean;
begin
  Result := (fMaxConnections <= 0) or (fClientCount < fMaxConnections);
end;

function TmORMot2WebSocketServer.GetConnectionsRemaining: Integer;
begin
  if fMaxConnections <= 0 then
    Result := -1  // Unlimited
  else
    Result := fMaxConnections - fClientCount;
end;

// =============================================================================
// CLIENT INFO METHODS (FROM OLD SERVER) - FIXED IP ACCESS
// =============================================================================

function TmORMot2WebSocketServer.GetClientIP(ClientID: Integer): string;
var
  clientInfo: TClientConnectionInfo;
begin
  fClientLock.Enter;
  try
    if fClientInfoMap.TryGetValue(ClientID, clientInfo) then
      Result := clientInfo.IP
    else
    begin
      // Try to get from server's IP mapping as fallback
      if fServer <> nil then
        Result := fServer.GetStoredClientIP(ClientID)
      else
        Result := 'Unknown';
    end;
  finally
    fClientLock.Leave;
  end;
end;

function TmORMot2WebSocketServer.GetClientEndpoint(ClientID: Integer): string;
var
  clientIP: string;
begin
  clientIP := GetClientIP(ClientID);
  Result := Format('Client #%d (%s)', [ClientID, clientIP]);
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
    fTotalBytesReceived := 0;
    fTotalBytesSent := 0;
    fTotalConnections := 0;
    fTotalMessagesReceived := 0;
    fTotalMessagesSent := 0;
    fTotalActiveConnections := fClientCount;
  finally
    fClientLock.Leave;
  end;
end;

procedure TmORMot2WebSocketServer.UpdateStatistics;
begin
  // Statistics are updated in real-time during operations
  fTotalActiveConnections := fClientCount;
end;

// =============================================================================
// ENCRYPTION METHODS MATCHING CLIENT EXACTLY
// =============================================================================

function TmORMot2WebSocketServer.GetEncryptionInfo: string;
const
  ModeNames: array [TAESMode] of string = ('ECB', 'CBC', 'CFB', 'OFB', 'CTR',
    'GCM', 'CFC', 'OFC', 'CTC');
  KeySizeNames: array [TAESKeySize] of string = ('128', '192', '256');
begin
  if fEncryptionEnabled and (fEncryptionKey <> '') then
    Result := Format('AES-%s-%s', [KeySizeNames[fEncryptionKeySize],
      ModeNames[fEncryptionMode]])
  else if fEncryptionEnabled then
    Result := 'ENABLED BUT NO KEY SET'
  else
    Result := 'DISABLED';
end;

function TmORMot2WebSocketServer.EncryptData(const Data: TBytes;
  const ClientIP: string): TBytes;
var
  DataStr: RawByteString;
  Password: RawByteString;
  KeyBytes: THash256;
  AES: TAesAbstract;
  KeySizeBits: Integer;
  Salt: RawByteString;
  Header: array [0 .. ENCRYPTION_HEADER_SIZE - 1] of Byte;
  EncryptedData: RawByteString;
  FinalResult: TBytes;
  SaltHost: string;
begin
  Result := Data; // Default to original data

  if not fEncryptionEnabled or (fEncryptionKey = '') then
    Exit;

  try
    // Convert TBytes to RawByteString
    SetLength(DataStr, Length(Data));
    if Length(Data) > 0 then
      Move(Data[0], DataStr[1], Length(Data));

    // Determine key size bits
    case fEncryptionKeySize of
      aks128:
        KeySizeBits := 128;
      aks192:
        KeySizeBits := 192;
      aks256:
        KeySizeBits := 256;
    else
      KeySizeBits := 256;
    end;

    // FIXED: Convert server's detected IP back to what client used
    if ClientIP = '127.0.0.1' then
      SaltHost := 'localhost'  // Client used 'localhost' to connect
    else
      SaltHost := ClientIP;    // For other IPs, use as-is

    // FIXED: Use SAME salt derivation as client
    Password := ToUtf8(fEncryptionKey);
    Salt := ToUtf8(fEncryptionKey + '_hardcoded_salt_2024');
    Pbkdf2HmacSha256(Password, Salt, 1000, KeyBytes);

    try
      // Create AES instance based on mode
      case fEncryptionMode of
        amECB:
          AES := TAesEcb.Create(KeyBytes, KeySizeBits);
        amCBC:
          AES := TAesCbc.Create(KeyBytes, KeySizeBits);
        amCFB:
          AES := TAesCfb.Create(KeyBytes, KeySizeBits);
        amOFB:
          AES := TAesOfb.Create(KeyBytes, KeySizeBits);
        amCTR:
          AES := TAesCtr.Create(KeyBytes, KeySizeBits);
        amGCM:
          AES := TAesGcm.Create(KeyBytes, KeySizeBits);
        amCFC:
          AES := TAesCfc.Create(KeyBytes, KeySizeBits);
        amOFC:
          AES := TAesOfc.Create(KeyBytes, KeySizeBits);
        amCTC:
          AES := TAesCtc.Create(KeyBytes, KeySizeBits);
      else
        AES := TAesEcb.Create(KeyBytes, KeySizeBits);
      end;

      try
        // Encrypt with IV
        EncryptedData := AES.EncryptPkcs7(DataStr, True);

        // Create header with magic number and encryption info
        FillChar(Header, SizeOf(Header), 0);
        PCardinal(@Header[0])^ := ENCRYPTION_MAGIC;
        PCardinal(@Header[4])^ := Cardinal(fEncryptionMode);
        PCardinal(@Header[8])^ := Cardinal(fEncryptionKeySize);
        // bytes 12-15 are reserved (already zeroed)

        // Combine header + encrypted data
        SetLength(FinalResult, ENCRYPTION_HEADER_SIZE + Length(EncryptedData));
        Move(Header[0], FinalResult[0], ENCRYPTION_HEADER_SIZE);
        if Length(EncryptedData) > 0 then
          Move(EncryptedData[1], FinalResult[ENCRYPTION_HEADER_SIZE],
            Length(EncryptedData));

        Result := FinalResult;

      finally
        AES.Free;
      end;
    except
      on E: Exception do
      begin
        DoError('Server encryption failed: ' + E.Message);
        Result := Data; // Return original data if encryption fails
      end;
    end;

    // Clear sensitive key material
    FillChar(KeyBytes, SizeOf(KeyBytes), 0);
  except
    on E: Exception do
    begin
      DoError('Server encryption parsing failed: ' + E.Message);
      Result := Data; // Return original data on error
    end;
  end;
end;

function TmORMot2WebSocketServer.DecryptData(const Data: TBytes;
  const ClientIP: string): TBytes;
var
  DataStr: RawByteString;
  Password: RawByteString;
  KeyBytes: THash256;
  AES: TAesAbstract;
  KeySizeBits: Integer;
  Salt: RawByteString;
  Header: array [0 .. ENCRYPTION_HEADER_SIZE - 1] of Byte;
  EncryptedPortion: RawByteString;
  Magic: Cardinal;
  Mode: TAESMode;
  KeySize: TAESKeySize;
  SaltHost: string;
begin
  Result := Data; // Default to original data

  if not fEncryptionEnabled or (fEncryptionKey = '') then
    Exit;

  // Check if data has encryption header
  if Length(Data) < ENCRYPTION_HEADER_SIZE then
    Exit;

  try
    // Extract header
    Move(Data[0], Header[0], ENCRYPTION_HEADER_SIZE);
    Magic := PCardinal(@Header[0])^;

    // Check magic number
    if Magic <> ENCRYPTION_MAGIC then
      Exit; // Not encrypted data, return as-is

    // Extract encryption parameters from header
    Mode := TAESMode(PCardinal(@Header[4])^);
    KeySize := TAESKeySize(PCardinal(@Header[8])^);

    // CRITICAL: VALIDATE RECEIVED PARAMETERS MATCH OURS
    if (Mode <> fEncryptionMode) then
    begin
      DoError(Format('ENCRYPTION MODE MISMATCH: received %s, expected %s',
        [GetEnumName(TypeInfo(TAESMode), Ord(Mode)),
         GetEnumName(TypeInfo(TAESMode), Ord(fEncryptionMode))]));
      Exit; // Don't try to decrypt with wrong mode!
    end;

    if (KeySize <> fEncryptionKeySize) then
    begin
      DoError(Format('KEY SIZE MISMATCH: received %s, expected %s',
        [GetEnumName(TypeInfo(TAESKeySize), Ord(KeySize)),
         GetEnumName(TypeInfo(TAESKeySize), Ord(fEncryptionKeySize))]));
      Exit; // Don't try to decrypt with wrong key size!
    end;

    // Extract encrypted portion
    SetLength(EncryptedPortion, Length(Data) - ENCRYPTION_HEADER_SIZE);
    if Length(EncryptedPortion) > 0 then
      Move(Data[ENCRYPTION_HEADER_SIZE], EncryptedPortion[1],
        Length(EncryptedPortion));

    // Determine key size bits
    case KeySize of
      aks128:
        KeySizeBits := 128;
      aks192:
        KeySizeBits := 192;
      aks256:
        KeySizeBits := 256;
    else
      KeySizeBits := 256;
    end;

    // FIXED: Convert server's detected IP back to what client used
    if ClientIP = '127.0.0.1' then
      SaltHost := 'localhost'  // Client used 'localhost' to connect
    else
      SaltHost := ClientIP;    // For other IPs, use as-is

    // FIXED: Use SAME salt derivation as client
    Password := ToUtf8(fEncryptionKey);
    Salt := ToUtf8(fEncryptionKey + '_hardcoded_salt_2024');
    Pbkdf2HmacSha256(Password, Salt, 1000, KeyBytes);

    try
      // Create AES instance based on mode from header
      case Mode of
        amECB:
          AES := TAesEcb.Create(KeyBytes, KeySizeBits);
        amCBC:
          AES := TAesCbc.Create(KeyBytes, KeySizeBits);
        amCFB:
          AES := TAesCfb.Create(KeyBytes, KeySizeBits);
        amOFB:
          AES := TAesOfb.Create(KeyBytes, KeySizeBits);
        amCTR:
          AES := TAesCtr.Create(KeyBytes, KeySizeBits);
        amGCM:
          AES := TAesGcm.Create(KeyBytes, KeySizeBits);
        amCFC:
          AES := TAesCfc.Create(KeyBytes, KeySizeBits);
        amOFC:
          AES := TAesOfc.Create(KeyBytes, KeySizeBits);
        amCTC:
          AES := TAesCtc.Create(KeyBytes, KeySizeBits);
      else
        AES := TAesEcb.Create(KeyBytes, KeySizeBits);
      end;

      try
        // Decrypt with IV
        DataStr := AES.DecryptPkcs7(EncryptedPortion, True);

        // Convert back to TBytes
        SetLength(Result, Length(DataStr));
        if Length(DataStr) > 0 then
          Move(DataStr[1], Result[0], Length(DataStr));

      finally
        AES.Free;
      end;
    except
      on E: Exception do
      begin
        DoError('Server decryption failed: ' + E.Message);
        Result := Data; // Return original data if decryption fails
      end;
    end;
  finally
    // Clear sensitive key material
    FillChar(KeyBytes, SizeOf(KeyBytes), 0);
  end;
end;

// =============================================================================
// CORE METHODS (DON'T BREAK)
// =============================================================================

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
  EncryptionKey: RawUtf8;
  Protocol: TCustomWebSocketServerProtocol;
begin
  if fActive then
    Exit;

  try
    SetServerState(ssStarting);

    // VALIDATE MAX CONNECTIONS SETTING
    if fMaxConnections <= 0 then
    begin
      DoError('MaxConnections must be greater than 0');
      raise Exception.Create('Invalid MaxConnections value');
    end;

    // Stop any existing server
    InternalStop;
    fLastError := '';

    // Clear client tracking
    fClientLock.Enter;
    try
      fClientInfoMap.Clear;
      fClientCount := 0;
    finally
      fClientLock.Leave;
    end;

    // Prepare encryption key
    if fEncryptionEnabled and (fEncryptionKey <> '') then
      EncryptionKey := ToUtf8(fEncryptionKey)
    else
      EncryptionKey := '';

    // Create the ASYNC server using proper mORMot2 approach
    fServer := TCustomAsyncServerRest.Create(ToUtf8(IntToStr(fPort)), nil, nil,
      // OnStart, OnStop
      ToUtf8('mORMot2WebSocketServer'), fServerThreadPoolCount,
      ToUtf8(fWebSocketsURI), EncryptionKey, fWebSocketsAjax, []);

    fServer.OwnerComponent := Self;

    // FIXED: Add our custom protocol to handle encryption
    Protocol := TCustomWebSocketServerProtocol.Create(Self);
    fServer.WebSocketProtocols.AddOnce(Protocol);

    // Wait for server to start (with timeout)
    fServer.WaitStarted(10000); // 10 second timeout

    fActive := True;
    SetServerState(ssListening);

  except
    on E: Exception do
    begin
      fLastError := E.Message;
      SetServerState(ssError);
      if fServer <> nil then
      begin
        try
          fServer.Shutdown;
        except
        end;
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
    try
      fServer.Shutdown;
    except
      // Ignore shutdown errors
    end;
    FreeAndNil(fServer);
  end;

  // Clear client tracking
  fClientLock.Enter;
  try
    fClientInfoMap.Clear;
    fClientCount := 0;
    fTotalActiveConnections := 0;
  finally
    fClientLock.Leave;
  end;

  SetServerState(ssIdle);
end;

// FIXED: Event handlers called by custom server and protocol
procedure TmORMot2WebSocketServer.OnInternalClientConnected(ClientID: Integer;
  Connection: TWebSocketAsyncConnection; const ClientIP: string);
var
  clientInfo: TClientConnectionInfo;
begin
  fClientLock.Enter;
  try
    // Create client info with CAPTURED IP
    clientInfo.ClientID := ClientID;
    clientInfo.Connection := Connection;
    clientInfo.IP := ClientIP; // Use the captured IP from DoConnect
    clientInfo.ConnectedAt := Now;
    clientInfo.State := csConnected;

    fClientInfoMap.AddOrSetValue(ClientID, clientInfo);
    Inc(fClientCount);
    Inc(fTotalActiveConnections);
    Inc(fTotalConnections);
  finally
    fClientLock.Leave;
  end;

  // Fire only the main event
  if Assigned(fOnClientConnected) then
    fOnClientConnected(Self, ClientID);
end;

procedure TmORMot2WebSocketServer.OnInternalClientDisconnected
  (ClientID: Integer);
begin
  fClientLock.Enter;
  try
    if fClientInfoMap.ContainsKey(ClientID) then
    begin
      fClientInfoMap.Remove(ClientID);
      Dec(fClientCount);
      Dec(fTotalActiveConnections);
      if fClientCount < 0 then
        fClientCount := 0;
      if fTotalActiveConnections < 0 then
        fTotalActiveConnections := 0;
    end;
  finally
    fClientLock.Leave;
  end;

  // Fire only the main event
  if Assigned(fOnClientDisconnected) then
    fOnClientDisconnected(Self, ClientID);
end;

// FIXED: OnDataReceived is for STATISTICS/LOGGING ONLY!
procedure TmORMot2WebSocketServer.OnInternalDataReceived(ClientID: Integer;
  const Data: TBytes);
begin
  // Update statistics
  Inc(fTotalBytesReceived, Length(Data));
  Inc(fTotalMessagesReceived);

  // Fire OnDataReceived for STATISTICS/LOGGING ONLY!
  if Assigned(fOnDataReceived) then
    fOnDataReceived(Self, ClientID, Data);
end;

// FIXED: Public methods - now fully working with mORMot2 async!
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
      Exit; // Client not found
  finally
    fClientLock.Leave;
  end;

  try
    dataToSend := Command;

    // FIXED: Auto-encrypt with proper coordination header
    if fEncryptionEnabled and (fEncryptionKey <> '') then
      dataToSend := EncryptData(Command, clientInfo.IP);

    Result := fServer.SendToClient(ClientID, dataToSend);

    if Result then
    begin
      // Update statistics
      Inc(fTotalBytesSent, Length(dataToSend));
      Inc(fTotalMessagesSent);

      // Fire OnDataSent for STATISTICS/LOGGING ONLY!
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
  ClientID: Integer;
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

          // FIXED: Auto-encrypt with proper coordination header for each client
          if fEncryptionEnabled and (fEncryptionKey <> '') then
            dataToSend := EncryptData(Command, clientInfo.IP);

          if fServer.SendToClient(ClientID, dataToSend) then
          begin
            // Update statistics
            Inc(fTotalBytesSent, Length(dataToSend));
            Inc(fTotalMessagesSent);

            // Fire OnDataSent for STATISTICS/LOGGING ONLY!
            if Assigned(fOnDataSent) then
              fOnDataSent(Self, ClientID, dataToSend);
          end;
        except
          on E: Exception do
          begin
            DoError('Broadcast to client ' + IntToStr(ClientID) + ' failed: ' +
              E.Message);
          end;
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

  Result := Format
    ('State: %s, Active: %s, Connections: %s, Total Connections: %d, Msgs Sent: %d, Msgs Received: %d, Bytes Sent: %d, Bytes Received: %d, Encryption: %s',
    [ServerStateToString(fServerState), BoolToStr(fActive, True), ConnectionInfo,
    fTotalConnections, fTotalMessagesSent, fTotalMessagesReceived, fTotalBytesSent, fTotalBytesReceived, GetEncryptionInfo]);
end;

// =============================================================================
// PROPERTY SETTERS (CORE - DON'T BREAK)
// =============================================================================

procedure TmORMot2WebSocketServer.SetActive(const Value: boolean);
begin
  if fActive <> Value then
  begin
    if Value then
      Start
    else
      Stop;
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

procedure TmORMot2WebSocketServer.SetServerThreadPoolCount
  (const Value: Integer);
begin
  if fServerThreadPoolCount <> Value then
  begin
    if fActive then
      raise Exception.Create
        ('Cannot change thread pool size while server is active');
    fServerThreadPoolCount := Value;
  end;
end;

procedure TmORMot2WebSocketServer.SetKeepAliveTimeOut(const Value: Integer);
begin
  fKeepAliveTimeOut := Value;
end;

procedure TmORMot2WebSocketServer.SetWebSocketsURI(const Value: string);
begin
  fWebSocketsURI := Value;
end;

procedure TmORMot2WebSocketServer.SetWebSocketsAjax(const Value: boolean);
begin
  fWebSocketsAjax := Value;
end;

procedure TmORMot2WebSocketServer.SetEncryptionEnabled(const Value: boolean);
begin
  fEncryptionEnabled := Value;
end;

procedure TmORMot2WebSocketServer.SetEncryptionKey(const Value: string);
begin
  fEncryptionKey := Value;
end;

procedure TmORMot2WebSocketServer.SetEncryptionMode(const Value: TAESMode);
begin
  fEncryptionMode := Value;
end;

procedure TmORMot2WebSocketServer.SetEncryptionKeySize
  (const Value: TAESKeySize);
begin
  fEncryptionKeySize := Value;
end;

// =============================================================================
// ALL ADDITIONAL PROPERTY SETTERS FROM OLD SERVER
// =============================================================================

procedure TmORMot2WebSocketServer.SetConnectionTimeout(const Value: Integer);
begin
  if fConnectionTimeout <> Value then
    fConnectionTimeout := Value;
end;

procedure TmORMot2WebSocketServer.SetDescription(const Value: string);
begin
  if fDescription <> Value then
    fDescription := Value;
end;

procedure TmORMot2WebSocketServer.SetKeepAlive(const Value: Boolean);
begin
  if fKeepAlive <> Value then
    fKeepAlive := Value;
end;

procedure TmORMot2WebSocketServer.SetLogLevel(const Value: Integer);
begin
  if fLogLevel <> Value then
    fLogLevel := Value;
end;

procedure TmORMot2WebSocketServer.SetMaxConnections(const Value: Integer);
begin
  if Value <= 0 then
    raise Exception.Create('MaxConnections must be greater than 0');

  if fMaxConnections <> Value then
  begin
    if fActive and (Value < fClientCount) then
      raise Exception.Create(Format('Cannot set MaxConnections to %d - currently have %d active connections',
        [Value, fClientCount]));
    fMaxConnections := Value;
  end;
end;

procedure TmORMot2WebSocketServer.SetName(const Value: string);
begin
  // FIXED: Actually set the component's Name, not just the internal field
  if (Value <> '') and (Value <> Name) then
  begin
    inherited Name := Value;  // This is the key fix!
    fName := Value;  // Keep internal field in sync
  end;
end;

procedure TmORMot2WebSocketServer.SetNoDelay(const Value: Boolean);
begin
  if fNoDelay <> Value then
    fNoDelay := Value;
end;

procedure TmORMot2WebSocketServer.SetReceiveBufferSize(const Value: Integer);
begin
  if fReceiveBufferSize <> Value then
    fReceiveBufferSize := Value;
end;

procedure TmORMot2WebSocketServer.SetReusePort(const Value: Boolean);
begin
  if fReusePort <> Value then
    fReusePort := Value;
end;

procedure TmORMot2WebSocketServer.SetSendBufferSize(const Value: Integer);
begin
  if fSendBufferSize <> Value then
    fSendBufferSize := Value;
end;

procedure TmORMot2WebSocketServer.SetTag(const Value: Integer);
begin
  if fTag <> Value then
    fTag := Value;
end;

procedure TmORMot2WebSocketServer.SetThreadPoolSize(const Value: Integer);
begin
  if fThreadPoolSize <> Value then
    fThreadPoolSize := Value;
end;

procedure TmORMot2WebSocketServer.SetVersion(const Value: string);
begin
  if fVersion <> Value then
    fVersion := Value;
end;

// =============================================================================
// EVENT TRIGGERS
// =============================================================================

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
