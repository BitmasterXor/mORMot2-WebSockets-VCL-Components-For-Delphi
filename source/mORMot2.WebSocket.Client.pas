unit mORMot2.WebSocket.Client;

interface

uses
  Classes,
  SysUtils,
  ExtCtrls,
  Math,
  SyncObjs,
  mormot.core.base,
  mormot.core.unicode,
  mormot.core.text,
  mormot.core.log,
  mormot.net.client,
  mormot.net.ws.core,
  mormot.net.ws.client,
  mormot.crypt.core,
  mormot.crypt.secure;

const
  // ENCRYPTION COORDINATION CONSTANTS
  ENCRYPTION_HEADER_SIZE = 16;  // 4 bytes magic + 4 bytes mode + 4 bytes keysize + 4 bytes reserved
  ENCRYPTION_MAGIC = $4D4F524D;  // "MORM" in hex

type
  /// Human-readable connection state enumeration
  TWebSocketConnectionState = (
    wsDisconnected,    // Not connected
    wsConnecting,      // Attempting to connect
    wsConnected,       // Successfully connected
    wsReconnecting,    // Attempting to reconnect
    wsDisconnecting,   // Gracefully disconnecting
    wsError           // Error state
  );

  /// Human-readable reconnection strategy enumeration
  TWebSocketReconnectStrategy = (
    rsLinear,          // Linear reconnection intervals
    rsExponential      // Exponential backoff intervals
  );

  /// AES Encryption Mode enumeration
  TAESMode = (
    amECB, amCBC, amCFB, amOFB, amCTR, amGCM, amCFC, amOFC, amCTC
  );

  /// AES Key Size enumeration
  TAESKeySize = (
    aks128, aks192, aks256
  );

  /// Forward declaration
  TmORMot2WebSocketClient = class;

  /// FIXED events - OnDataReceived/OnDataSent are for STATISTICS ONLY!
  TWebSocketConnectEvent = procedure(Sender: TObject) of object;
  TWebSocketDataReceivedEvent = procedure(Sender: TObject; const Data: TBytes) of object;  // FOR STATISTICS/LOGGING ONLY!
  TWebSocketDataSentEvent = procedure(Sender: TObject; const Data: TBytes) of object;  // FOR STATISTICS/LOGGING ONLY!
  TWebSocketDisconnectEvent = procedure(Sender: TObject) of object;
  TWebSocketErrorEvent = procedure(Sender: TObject; const ErrorMsg: string) of object;
  TWebSocketHandleCommandEvent = procedure(Sender: TObject; const Command: TBytes) of object;  // THIS IS WHERE YOU PROCESS DATA!
  TWebSocketReconnectingEvent = procedure(Sender: TObject; AttemptNumber: Integer) of object;
  TWebSocketReconnectFailedEvent = procedure(Sender: TObject; AttemptNumber: Integer; const ErrorMsg: string) of object;

  /// STATE CHANGE EVENT
  TWebSocketStateChangeEvent = procedure(Sender: TObject; OldState, NewState: TWebSocketConnectionState; const StateDescription: string) of object;

  /// Fixed WebSocket client protocol
  TWebSocketClientProtocol = class(TWebSocketProtocolJson)
  protected
    fOwner: TmORMot2WebSocketClient;
    procedure ProcessIncomingFrame(Sender: TWebSocketProcess;
      var Request: TWebSocketFrame; const Info: RawUtf8); override;
  public
    constructor Create(AOwner: TmORMot2WebSocketClient);
    function Clone(const aClientUri: RawUtf8): TWebSocketProtocol; override;
  end;

  /// WebSocket Client Component - FIXED with proper encryption coordination
  TmORMot2WebSocketClient = class(TComponent)
  private
    fClient: THttpClientWebSockets;
    fProtocol: TWebSocketClientProtocol;
    fHost: string;
    fPort: Integer;
    fURI: string;
    fConnected: Boolean;
    fConnecting: Boolean;
    fLastError: string;
    fConnectionThread: TThread;
    fCurrentState: TWebSocketConnectionState;

    // Additional properties
    fActive: Boolean;
    fConnectionTimeout: Integer;
    fDescription: string;
    fKeepAlive: Boolean;
    fLogLevel: Integer;
    fMessageReceived: Boolean;
    fMessageSent: Boolean;
    fName: string;
    fNoDelay: Boolean;
    fReceiveBufferSize: Integer;
    fReconnectStrategy: TWebSocketReconnectStrategy;
    fSendBufferSize: Integer;
    fThreadPoolSize: Integer;
    fTotalBytesReceived: Int64;
    fTotalBytesSent: Int64;
    fVersion: string;

    // Encryption properties
    fEncryptionEnabled: Boolean;
    fEncryptionKey: string;
    fEncryptionMode: TAESMode;
    fEncryptionKeySize: TAESKeySize;
    // Crypto fields — derived key cached for thread-safe on-the-fly changes
    fCryptoLock: TCriticalSection;
    fDerivedKey: THash256;
    fKeyDerived: Boolean;

    // Reconnection properties
    fAutoReconnect: Boolean;
    fReconnectInterval: Integer;
    fReconnectTimer: TTimer;
    fReconnectAttempts: Integer;
    fMaxReconnectAttempts: Integer;
    fReconnecting: Boolean;
    fUserDisconnected: Boolean;

    // FIXED Events - OnDataReceived/OnDataSent are for STATISTICS ONLY!
    fOnConnect: TWebSocketConnectEvent;
    fOnDataReceived: TWebSocketDataReceivedEvent;  // FOR STATISTICS/LOGGING ONLY!
    fOnDataSent: TWebSocketDataSentEvent;  // FOR STATISTICS/LOGGING ONLY!
    fOnDisconnect: TWebSocketDisconnectEvent;
    fOnError: TWebSocketErrorEvent;
    fOnHandleCommand: TWebSocketHandleCommandEvent;  // THIS IS WHERE YOU PROCESS DATA!
    fOnStateChange: TWebSocketStateChangeEvent;
    fOnReconnecting: TWebSocketReconnectingEvent;
    fOnReconnectFailed: TWebSocketReconnectFailedEvent;

    // Property setters
    procedure SetActive(const Value: Boolean);
    procedure SetConnectionTimeout(const Value: Integer);
    procedure SetDescription(const Value: string);
    procedure SetKeepAlive(const Value: Boolean);
    procedure SetLogLevel(const Value: Integer);
    procedure SetName(const Value: string);
    procedure SetNoDelay(const Value: Boolean);
    procedure SetReceiveBufferSize(const Value: Integer);
    procedure SetReconnectStrategy(const Value: TWebSocketReconnectStrategy);
    procedure SetSendBufferSize(const Value: Integer);
    procedure SetThreadPoolSize(const Value: Integer);
    procedure SetVersion(const Value: string);

    procedure SetConnected(const Value: Boolean);
    procedure SetHost(const Value: string);
    procedure SetPort(const Value: Integer);
    procedure SetURI(const Value: string);
    procedure SetAutoReconnect(const Value: Boolean);
    procedure SetReconnectInterval(const Value: Integer);
    procedure SetMaxReconnectAttempts(const Value: Integer);

    // Encryption helpers
    procedure DeriveEncryptionKey;
    procedure SetEncryptionEnabled(const Value: Boolean);
    procedure SetEncryptionKey(const Value: string);
    procedure SetEncryptionMode(const Value: TAESMode);
    procedure SetEncryptionKeySize(const Value: TAESKeySize);
    function GetEncryptionInfo: string;

    // Helper methods for state management
    function ConnectionStateToString(State: TWebSocketConnectionState): string;
    function GetConnectionStateDescription: string;

    // FIXED encryption methods with proper coordination
    function EncryptData(const Data: TBytes): TBytes;
    function DecryptData(const Data: TBytes): TBytes;

    procedure DoError(const ErrorMsg: string);
    procedure DoDataReceived(const Data: TBytes);
    procedure DoDataSent(const Data: TBytes);
    procedure DoHandleCommand(const Command: TBytes);
    procedure DoConnect;
    procedure DoDisconnect;
    procedure DoStateChange(NewState: TWebSocketConnectionState);
    procedure HandleWebSocketsClosed(Sender: TObject);
    procedure HandleConnectionComplete(Success: Boolean; const ErrorMsg: string);
    procedure OnReconnectTimer(Sender: TObject);
    procedure StartReconnectTimer;
    procedure StopReconnectTimer;
    procedure DoReconnecting(AttemptNumber: Integer);
    procedure DoReconnectFailed(AttemptNumber: Integer; const ErrorMsg: string);
    procedure UpdateBytesReceived(const Bytes: Int64);
    procedure UpdateBytesSent(const Bytes: Int64);

  protected
    procedure InternalConnect;
    procedure InternalDisconnect;
    procedure AsyncConnect;
    procedure HandleUnexpectedDisconnection;

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    /// Connect to WebSocket server (async - non-blocking)
    function Connect: Boolean;
    /// Connect to WebSocket server (sync - blocking, for compatibility)
    function ConnectSync: Boolean;
    /// Disconnect from WebSocket server
    procedure Disconnect;
    /// Send command to server - FIXED: auto-encrypts with proper coordination
    function SendCommand(const Command: TBytes): Boolean;
    /// Check if currently connected
    function IsConnected: Boolean;
    /// Check if currently attempting to connect
    function IsConnecting: Boolean;
    /// Check if currently attempting to reconnect
    function IsReconnecting: Boolean;
    /// Get last error message
    function GetLastError: string;
    /// Get current reconnection attempt count
    function GetReconnectAttempts: Integer;
    /// Reset reconnection attempt counter
    procedure ResetReconnectAttempts;
    /// Get total bytes received
    function GetTotalBytesReceived: Int64;
    /// Get total bytes sent
    function GetTotalBytesSent: Int64;
    /// Reset byte counters
    procedure ResetByteCounters;
    /// Get the server's hostname/IP address
    function GetServerIP: string;
    /// Get current connection state as string
    function GetConnectionStateAsString: string;

  published
    /// Active state (alternative to Connected)
    property Active: Boolean read fActive write SetActive default False;
    /// Server hostname or IP address
    property Host: string read fHost write SetHost;
    /// Server port number
    property Port: Integer read fPort write SetPort default 80;
    /// WebSocket URI path
    property URI: string read fURI write SetURI;
    /// Connection state
    property Connected: Boolean read fConnected write SetConnected default False;
    /// Connection timeout in milliseconds
    property ConnectionTimeout: Integer read fConnectionTimeout write SetConnectionTimeout default 30000;
    /// Component description
    property Description: string read fDescription write SetDescription;

    /// Enable keep-alive
    property KeepAlive: Boolean read fKeepAlive write SetKeepAlive default True;
    /// Logging level (0=none, 1=errors, 2=info, 3=debug)
    property LogLevel: Integer read fLogLevel write SetLogLevel default 1;
    /// Component name
    property Name: string read fName write SetName;
    /// TCP No Delay option
    property NoDelay: Boolean read fNoDelay write SetNoDelay default True;
    /// Receive buffer size in bytes
    property ReceiveBufferSize: Integer read fReceiveBufferSize write SetReceiveBufferSize default 8192;
    /// Thread pool size
    property ThreadPoolSize: Integer read fThreadPoolSize write SetThreadPoolSize default 4;
    /// Component version
    property Version: string read fVersion write SetVersion;
    /// Send buffer size in bytes
    property SendBufferSize: Integer read fSendBufferSize write SetSendBufferSize default 8192;

    // FIXED ENCRYPTION PROPERTIES
    /// Enable/disable encryption - FIXED with coordination!
    property EncryptionEnabled: Boolean read fEncryptionEnabled write SetEncryptionEnabled default False;
    /// Encryption key/password - FIXED with coordination!
    property EncryptionKey: string read fEncryptionKey write SetEncryptionKey;
    /// AES encryption mode - FIXED with coordination!
    property EncryptionMode: TAESMode read fEncryptionMode write SetEncryptionMode default amCBC;
    /// AES key size - FIXED with coordination!
    property EncryptionKeySize: TAESKeySize read fEncryptionKeySize write SetEncryptionKeySize default aks256;
    /// Read-only encryption info string - FIXED!
    property EncryptionInfo: string read GetEncryptionInfo;

    /// Read-only connecting state
    property Connecting: Boolean read fConnecting;
    /// Enable automatic reconnection on connection loss
    property AutoReconnect: Boolean read fAutoReconnect write SetAutoReconnect default False;
    /// Interval in milliseconds between reconnection attempts
    property ReconnectInterval: Integer read fReconnectInterval write SetReconnectInterval default 5000;
    /// Maximum number of reconnection attempts (0 = unlimited)
    property MaxReconnectAttempts: Integer read fMaxReconnectAttempts write SetMaxReconnectAttempts default 0;
    /// Reconnection strategy
    property ReconnectStrategy: TWebSocketReconnectStrategy read fReconnectStrategy write SetReconnectStrategy default rsLinear;
    /// Read-only reconnecting state
    property Reconnecting: Boolean read fReconnecting;
    /// Read-only message received flag
    property MessageReceived: Boolean read fMessageReceived;
    /// Read-only message sent flag
    property MessageSent: Boolean read fMessageSent;
    /// Read-only total bytes received
    property TotalBytesReceived: Int64 read fTotalBytesReceived;
    /// Read-only total bytes sent
    property TotalBytesSent: Int64 read fTotalBytesSent;
    /// Read-only current connection state
    property ConnectionState: TWebSocketConnectionState read fCurrentState;
    /// Read-only connection state description
    property ConnectionStateDescription: string read GetConnectionStateDescription;

    // FIXED Events - OnDataReceived/OnDataSent are for STATISTICS ONLY!
    property OnConnect: TWebSocketConnectEvent read fOnConnect write fOnConnect;
    property OnDataReceived: TWebSocketDataReceivedEvent read fOnDataReceived write fOnDataReceived;  // FOR STATISTICS/LOGGING ONLY!
    property OnDataSent: TWebSocketDataSentEvent read fOnDataSent write fOnDataSent;  // FOR STATISTICS/LOGGING ONLY!
    property OnDisconnect: TWebSocketDisconnectEvent read fOnDisconnect write fOnDisconnect;
    property OnError: TWebSocketErrorEvent read fOnError write fOnError;
    property OnHandleCommand: TWebSocketHandleCommandEvent read fOnHandleCommand write fOnHandleCommand;  // THIS IS WHERE YOU PROCESS DATA!
    property OnStateChange: TWebSocketStateChangeEvent read fOnStateChange write fOnStateChange;
    property OnReconnecting: TWebSocketReconnectingEvent read fOnReconnecting write fOnReconnecting;
    property OnReconnectFailed: TWebSocketReconnectFailedEvent read fOnReconnectFailed write fOnReconnectFailed;
  end;

procedure Register;

implementation

// =============================================================================
// STATE HELPER METHODS
// =============================================================================

function TmORMot2WebSocketClient.ConnectionStateToString(State: TWebSocketConnectionState): string;
begin
  case State of
    wsDisconnected: Result := 'Disconnected';
    wsConnecting: Result := 'Connecting';
    wsConnected: Result := 'Connected';
    wsReconnecting: Result := 'Reconnecting';
    wsDisconnecting: Result := 'Disconnecting';
    wsError: Result := 'Error';
  else
    Result := 'Unknown';
  end;
end;

function TmORMot2WebSocketClient.GetConnectionStateDescription: string;
begin
  case fCurrentState of
    wsDisconnected: Result := Format('Disconnected from %s:%d', [fHost, fPort]);
    wsConnecting: Result := Format('Connecting to %s:%d%s', [fHost, fPort, fURI]);
    wsConnected: Result := Format('Connected to %s:%d%s', [fHost, fPort, fURI]);
    wsReconnecting: Result := Format('Reconnecting to %s:%d%s (attempt %d)', [fHost, fPort, fURI, fReconnectAttempts + 1]);
    wsDisconnecting: Result := Format('Disconnecting from %s:%d', [fHost, fPort]);
    wsError: Result := Format('Connection error with %s:%d - %s', [fHost, fPort, fLastError]);
  else
    Result := 'Unknown connection state';
  end;
end;

function TmORMot2WebSocketClient.GetConnectionStateAsString: string;
begin
  Result := ConnectionStateToString(fCurrentState);
end;

// =============================================================================
// FIXED ENCRYPTION METHODS WITH PROPER COORDINATION
// =============================================================================

function TmORMot2WebSocketClient.GetEncryptionInfo: string;
const
  ModeNames: array[TAESMode] of string = (
    'ECB', 'CBC', 'CFB', 'OFB', 'CTR', 'GCM', 'CFC', 'OFC', 'CTC'
  );
  KeySizeNames: array[TAESKeySize] of string = ('128', '192', '256');
begin
  if fEncryptionEnabled and (fEncryptionKey <> '') then
    Result := Format('AES-%s-%s', [KeySizeNames[fEncryptionKeySize], ModeNames[fEncryptionMode]])
  else if fEncryptionEnabled then
    Result := 'ENABLED BUT NO KEY SET'
  else
    Result := 'DISABLED';
end;

function TmORMot2WebSocketClient.EncryptData(const Data: TBytes): TBytes;
var
  DKey: THash256;
  DMode: TAESMode;
  DKeySize: TAESKeySize;
  DataStr, EncryptedData: RawByteString;
  AES: TAesAbstract;
  KeySizeBits: Integer;
  Header: array[0..ENCRYPTION_HEADER_SIZE-1] of Byte;
  FinalResult: TBytes;
begin
  Result := Data;

  fCryptoLock.Enter;
  try
    if not fKeyDerived then Exit;
    DKey     := fDerivedKey;
    DMode    := fEncryptionMode;
    DKeySize := fEncryptionKeySize;
  finally
    fCryptoLock.Leave;
  end;

  try
    SetLength(DataStr, Length(Data));
    if Length(Data) > 0 then
      Move(Data[0], DataStr[1], Length(Data));

    case DKeySize of
      aks128: KeySizeBits := 128;
      aks192: KeySizeBits := 192;
    else
      KeySizeBits := 256;
    end;

    case DMode of
      amECB: AES := TAesEcb.Create(DKey, KeySizeBits);
      amCBC: AES := TAesCbc.Create(DKey, KeySizeBits);
      amCFB: AES := TAesCfb.Create(DKey, KeySizeBits);
      amOFB: AES := TAesOfb.Create(DKey, KeySizeBits);
      amCTR: AES := TAesCtr.Create(DKey, KeySizeBits);
      amGCM: AES := TAesGcm.Create(DKey, KeySizeBits);
      amCFC: AES := TAesCfc.Create(DKey, KeySizeBits);
      amOFC: AES := TAesOfc.Create(DKey, KeySizeBits);
      amCTC: AES := TAesCtc.Create(DKey, KeySizeBits);
    else
      AES := TAesCbc.Create(DKey, KeySizeBits);
    end;

    try
      EncryptedData := AES.EncryptPkcs7(DataStr, True);
    finally
      AES.Free;
    end;

    FillChar(Header, SizeOf(Header), 0);
    PCardinal(@Header[0])^  := ENCRYPTION_MAGIC;
    PCardinal(@Header[4])^  := Cardinal(DMode);
    PCardinal(@Header[8])^  := Cardinal(DKeySize);
    PCardinal(@Header[12])^ := 0;

    SetLength(FinalResult, ENCRYPTION_HEADER_SIZE + Length(EncryptedData));
    Move(Header[0], FinalResult[0], ENCRYPTION_HEADER_SIZE);
    if Length(EncryptedData) > 0 then
      Move(EncryptedData[1], FinalResult[ENCRYPTION_HEADER_SIZE], Length(EncryptedData));

    Result := FinalResult;
  except
    Result := Data;
  end;
end;

function TmORMot2WebSocketClient.DecryptData(const Data: TBytes): TBytes;
var
  DKey: THash256;
  DataStr, Decrypted: RawByteString;
  AES: TAesAbstract;
  KeySizeBits: Integer;
  ReceivedMode: TAESMode;
  ReceivedKeySize: TAESKeySize;
  EncryptedPart: TBytes;
begin
  Result := Data;

  if Length(Data) < ENCRYPTION_HEADER_SIZE then
    Exit;

  if PCardinal(@Data[0])^ <> ENCRYPTION_MAGIC then
    Exit;

  fCryptoLock.Enter;
  try
    if not fKeyDerived then Exit;
    DKey := fDerivedKey;
  finally
    fCryptoLock.Leave;
  end;

  ReceivedMode    := TAESMode(PCardinal(@Data[4])^);
  ReceivedKeySize := TAESKeySize(PCardinal(@Data[8])^);

  SetLength(EncryptedPart, Length(Data) - ENCRYPTION_HEADER_SIZE);
  if Length(EncryptedPart) > 0 then
    Move(Data[ENCRYPTION_HEADER_SIZE], EncryptedPart[0], Length(EncryptedPart));

  SetLength(DataStr, Length(EncryptedPart));
  if Length(EncryptedPart) > 0 then
    Move(EncryptedPart[0], DataStr[1], Length(EncryptedPart));

  case ReceivedKeySize of
    aks128: KeySizeBits := 128;
    aks192: KeySizeBits := 192;
  else
    KeySizeBits := 256;
  end;

  try
    case ReceivedMode of
      amECB: AES := TAesEcb.Create(DKey, KeySizeBits);
      amCBC: AES := TAesCbc.Create(DKey, KeySizeBits);
      amCFB: AES := TAesCfb.Create(DKey, KeySizeBits);
      amOFB: AES := TAesOfb.Create(DKey, KeySizeBits);
      amCTR: AES := TAesCtr.Create(DKey, KeySizeBits);
      amGCM: AES := TAesGcm.Create(DKey, KeySizeBits);
      amCFC: AES := TAesCfc.Create(DKey, KeySizeBits);
      amOFC: AES := TAesOfc.Create(DKey, KeySizeBits);
      amCTC: AES := TAesCtc.Create(DKey, KeySizeBits);
    else
      AES := TAesCbc.Create(DKey, KeySizeBits);
    end;

    try
      Decrypted := AES.DecryptPkcs7(DataStr, True);
      SetLength(Result, Length(Decrypted));
      if Length(Decrypted) > 0 then
        Move(Decrypted[1], Result[0], Length(Decrypted));
    finally
      AES.Free;
    end;
  except
    Result := Data;
  end;
end;

{ TWebSocketClientProtocol }

constructor TWebSocketClientProtocol.Create(AOwner: TmORMot2WebSocketClient);
begin
  fOwner := AOwner;
  inherited Create('');
end;

function TWebSocketClientProtocol.Clone(const aClientUri: RawUtf8): TWebSocketProtocol;
begin
  result := TWebSocketClientProtocol.Create(fOwner);
end;

// FIXED: Proper event handling - OnDataReceived for statistics, OnHandleCommand for processing
procedure TWebSocketClientProtocol.ProcessIncomingFrame(Sender: TWebSocketProcess;
  var Request: TWebSocketFrame; const Info: RawUtf8);
var
  rawBytes: TBytes;
  decryptedBytes: TBytes;
begin
  if (fOwner <> nil) and (Sender <> nil) then
  begin
    case Request.opcode of
      focContinuation:
        begin
          // Connection established
          if not fOwner.fConnected then
          begin
            fOwner.fConnected := True;
            fOwner.fReconnectAttempts := 0;
            fOwner.fReconnecting := False;
            fOwner.StopReconnectTimer;
            fOwner.DoStateChange(wsConnected);
            fOwner.DoConnect;
          end;
        end;
      focConnectionClose:
        begin
          if fOwner.fConnected then
          begin
            fOwner.fConnected := False;
            fOwner.DoStateChange(wsDisconnected);
            fOwner.DoDisconnect;
            fOwner.HandleUnexpectedDisconnection;
          end;
        end;
      focText:
        begin
          // Convert payload to TBytes
          SetLength(rawBytes, Length(Request.payload));
          if Length(rawBytes) > 0 then
            Move(Request.payload[1], rawBytes[0], Length(rawBytes));

          // FIXED: Fire OnDataReceived FIRST for statistics (with raw encrypted data)
          fOwner.DoDataReceived(rawBytes);

          decryptedBytes := rawBytes;
          if fOwner.fEncryptionEnabled then
            decryptedBytes := fOwner.DecryptData(rawBytes);

          // FIXED: Fire OnHandleCommand for actual data processing (with decrypted data)
          fOwner.DoHandleCommand(decryptedBytes);
        end;
    end;
  end;
  inherited ProcessIncomingFrame(Sender, Request, Info);
end;

{ TmORMot2WebSocketClient }

constructor TmORMot2WebSocketClient.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  fHost := 'localhost';
  fPort := 80;
  fURI := '/';
  fConnected := False;
  fConnecting := False;
  fClient := nil;
  fProtocol := nil;
  fConnectionThread := nil;
  fCurrentState := wsDisconnected;

  // Initialize properties
  fActive := False;
  fConnectionTimeout := 30000;
  fDescription := '';
  fKeepAlive := True;
  fLogLevel := 1;
  fName := '';
  fNoDelay := True;
  fReceiveBufferSize := 8192;
  fSendBufferSize := 8192;
  fThreadPoolSize := 4;
  fVersion := '2.0.6';  // Updated version for fixed event handling
  fMessageReceived := False;
  fMessageSent := False;
  fTotalBytesReceived := 0;
  fTotalBytesSent := 0;

  // Encryption initialization
  fEncryptionEnabled := False;
  fEncryptionKey     := '';
  fEncryptionMode    := amCBC;
  fEncryptionKeySize := aks256;
  fCryptoLock := TCriticalSection.Create;
  fKeyDerived := False;
  FillChar(fDerivedKey, SizeOf(fDerivedKey), 0);

  // Initialize reconnection
  fAutoReconnect := False;
  fReconnectInterval := 5000;
  fMaxReconnectAttempts := 0;
  fReconnectAttempts := 0;
  fReconnectStrategy := rsLinear;
  fReconnecting := False;
  fUserDisconnected := False;

  fReconnectTimer := TTimer.Create(Self);
  fReconnectTimer.Enabled := False;
  fReconnectTimer.OnTimer := OnReconnectTimer;
end;

destructor TmORMot2WebSocketClient.Destroy;
var
  timeoutCounter: Integer;
begin
  try
    // Stop reconnection timer first
    try
      StopReconnectTimer;
    except
      // Ignore timer errors
    end;

    // Force disconnect
    if fConnected or fConnecting then
    begin
      try
        InternalDisconnect;
      except
        // Force cleanup even if disconnect fails
        fConnected := False;
        fConnecting := False;
        fReconnecting := False;
      end;
    end;

    // Wait for connection thread with timeout
    if fConnectionThread <> nil then
    begin
      try
        fConnectionThread.Terminate;

        // Manual timeout (2 seconds)
        timeoutCounter := 0;
        while (not fConnectionThread.Finished) and (timeoutCounter < 200) do
        begin
          Sleep(10);
          Inc(timeoutCounter);
        end;

        if fConnectionThread.Finished then
        begin
          try
            fConnectionThread.WaitFor;
          except
            // Ignore wait errors
          end;
        end;

        // Force free regardless
        try
          FreeAndNil(fConnectionThread);
        except
          fConnectionThread := nil;
        end;
      except
        fConnectionThread := nil;
      end;
    end;

    // Force cleanup client
    if fClient <> nil then
    begin
      try
        fClient.OnWebSocketsClosed := nil;
        FreeAndNil(fClient);
      except
        fClient := nil;
      end;
    end;

  except
    // Ignore ALL destructor errors - force cleanup
    fClient           := nil;
    fConnectionThread := nil;
  end;

  FillChar(fDerivedKey, SizeOf(fDerivedKey), 0);
  FreeAndNil(fCryptoLock);

  inherited Destroy;
end;

function TmORMot2WebSocketClient.GetServerIP: string;
begin
  if fHost = 'localhost' then
    Result := '127.0.0.1'
  else
    Result := fHost;
end;

// Property setters
procedure TmORMot2WebSocketClient.SetActive(const Value: Boolean);
begin
  if fActive <> Value then
  begin
    fActive := Value;
    SetConnected(Value);
  end;
end;

procedure TmORMot2WebSocketClient.SetConnectionTimeout(const Value: Integer);
begin
  if Value >= 1000 then
    fConnectionTimeout := Value;
end;

procedure TmORMot2WebSocketClient.SetDescription(const Value: string);
begin
  fDescription := Value;
end;

procedure TmORMot2WebSocketClient.SetKeepAlive(const Value: Boolean);
begin
  fKeepAlive := Value;
end;

procedure TmORMot2WebSocketClient.SetLogLevel(const Value: Integer);
begin
  if (Value >= 0) and (Value <= 3) then
    fLogLevel := Value;
end;

procedure TmORMot2WebSocketClient.SetName(const Value: string);
begin
  // FIXED: Actually set the component's Name, not just the internal field
  if (Value <> '') and (Value <> Name) then
  begin
    inherited Name := Value;  // This is the key fix!
    fName := Value;  // Keep internal field in sync
  end;
end;

procedure TmORMot2WebSocketClient.SetNoDelay(const Value: Boolean);
begin
  fNoDelay := Value;
end;

procedure TmORMot2WebSocketClient.SetReceiveBufferSize(const Value: Integer);
begin
  if Value >= 1024 then
    fReceiveBufferSize := Value;
end;

procedure TmORMot2WebSocketClient.SetReconnectStrategy(const Value: TWebSocketReconnectStrategy);
begin
  fReconnectStrategy := Value;
end;

procedure TmORMot2WebSocketClient.SetSendBufferSize(const Value: Integer);
begin
  if Value >= 1024 then
    fSendBufferSize := Value;
end;

procedure TmORMot2WebSocketClient.SetThreadPoolSize(const Value: Integer);
begin
  if Value >= 1 then
    fThreadPoolSize := Value;
end;

procedure TmORMot2WebSocketClient.SetVersion(const Value: string);
begin
  fVersion := Value;
end;

procedure TmORMot2WebSocketClient.DeriveEncryptionKey;
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

procedure TmORMot2WebSocketClient.SetEncryptionEnabled(const Value: Boolean);
begin
  fCryptoLock.Enter;
  try
    fEncryptionEnabled := Value;
    DeriveEncryptionKey;
  finally
    fCryptoLock.Leave;
  end;
end;

procedure TmORMot2WebSocketClient.SetEncryptionKey(const Value: string);
begin
  fCryptoLock.Enter;
  try
    fEncryptionKey := Value;
    DeriveEncryptionKey;
  finally
    fCryptoLock.Leave;
  end;
end;

procedure TmORMot2WebSocketClient.SetEncryptionMode(const Value: TAESMode);
begin
  fCryptoLock.Enter;
  try
    fEncryptionMode := Value;
    DeriveEncryptionKey;
  finally
    fCryptoLock.Leave;
  end;
end;

procedure TmORMot2WebSocketClient.SetEncryptionKeySize(const Value: TAESKeySize);
begin
  fCryptoLock.Enter;
  try
    fEncryptionKeySize := Value;
    DeriveEncryptionKey;
  finally
    fCryptoLock.Leave;
  end;
end;

procedure TmORMot2WebSocketClient.UpdateBytesReceived(const Bytes: Int64);
begin
  Inc(fTotalBytesReceived, Bytes);
  fMessageReceived := True;
end;

procedure TmORMot2WebSocketClient.UpdateBytesSent(const Bytes: Int64);
begin
  Inc(fTotalBytesSent, Bytes);
  fMessageSent := True;
end;

function TmORMot2WebSocketClient.GetTotalBytesReceived: Int64;
begin
  Result := fTotalBytesReceived;
end;

function TmORMot2WebSocketClient.GetTotalBytesSent: Int64;
begin
  Result := fTotalBytesSent;
end;

procedure TmORMot2WebSocketClient.ResetByteCounters;
begin
  fTotalBytesReceived := 0;
  fTotalBytesSent := 0;
  fMessageReceived := False;
  fMessageSent := False;
end;

procedure TmORMot2WebSocketClient.SetConnected(const Value: Boolean);
begin
  if fConnected <> Value then
  begin
    if Value then
      Connect
    else
      Disconnect;
  end;
end;

procedure TmORMot2WebSocketClient.SetHost(const Value: string);
begin
  if fHost <> Value then
  begin
    if not (fConnected or fConnecting) then
      fHost := Value;
  end;
end;

procedure TmORMot2WebSocketClient.SetPort(const Value: Integer);
begin
  if fPort <> Value then
  begin
    if not (fConnected or fConnecting) then
      fPort := Value;
  end;
end;

procedure TmORMot2WebSocketClient.SetURI(const Value: string);
begin
  if fURI <> Value then
  begin
    if not (fConnected or fConnecting) then
      fURI := Value;
  end;
end;

procedure TmORMot2WebSocketClient.SetAutoReconnect(const Value: Boolean);
begin
  if fAutoReconnect <> Value then
  begin
    fAutoReconnect := Value;
    if not Value then
      StopReconnectTimer;
  end;
end;

procedure TmORMot2WebSocketClient.SetReconnectInterval(const Value: Integer);
begin
  if Value >= 1000 then
  begin
    fReconnectInterval := Value;
    if fReconnectTimer.Enabled then
      fReconnectTimer.Interval := fReconnectInterval;
  end;
end;

procedure TmORMot2WebSocketClient.SetMaxReconnectAttempts(const Value: Integer);
begin
  if Value >= 0 then
    fMaxReconnectAttempts := Value;
end;

// CONNECTION METHODS
function TmORMot2WebSocketClient.Connect: Boolean;
begin
  Result := False;
  if fConnected or fConnecting then
    Exit;

  try
    fUserDisconnected := False;
    DoStateChange(wsConnecting);
    AsyncConnect;
    Result := True;
  except
    on E: Exception do
    begin
      fLastError := E.Message;
      DoStateChange(wsError);
      DoError(E.Message);
    end;
  end;
end;

function TmORMot2WebSocketClient.ConnectSync: Boolean;
begin
  Result := False;
  try
    fUserDisconnected := False;
    DoStateChange(wsConnecting);
    InternalConnect;
    Result := True;
  except
    on E: Exception do
    begin
      fLastError := E.Message;
      DoStateChange(wsError);
      DoError(E.Message);
    end;
  end;
end;

procedure TmORMot2WebSocketClient.AsyncConnect;
begin
  if fConnectionThread <> nil then
  begin
    fConnectionThread.Terminate;
    fConnectionThread.WaitFor;
    FreeAndNil(fConnectionThread);
  end;

  fConnecting := True;

  fConnectionThread := TThread.CreateAnonymousThread(
    procedure
    var
      success: Boolean;
      errorMsg: string;
    begin
      success := False;
      errorMsg := '';
      try
        InternalConnect;
        success := True;
      except
        on E: Exception do
        begin
          errorMsg := E.Message;
          fLastError := E.Message;
        end;
      end;

      TThread.Synchronize(nil,
        procedure
        begin
          HandleConnectionComplete(success, errorMsg);
        end);
    end);

  fConnectionThread.Start;
end;

procedure TmORMot2WebSocketClient.HandleConnectionComplete(Success: Boolean; const ErrorMsg: string);
begin
  fConnecting := False;

  if Success then
  begin
    // SUCCESS HANDLED IN ProcessIncomingFrame
  end
  else
  begin
    fConnected := False;
    DoStateChange(wsError);
    if ErrorMsg <> '' then
      DoError(ErrorMsg);

    if fReconnecting then
    begin
      Inc(fReconnectAttempts);
      DoReconnectFailed(fReconnectAttempts, ErrorMsg);

      if fAutoReconnect and
         ((fMaxReconnectAttempts = 0) or (fReconnectAttempts < fMaxReconnectAttempts)) then
      begin
        DoStateChange(wsReconnecting);
        StartReconnectTimer;
      end
      else
      begin
        fReconnecting := False;
        DoStateChange(wsDisconnected);
      end;
    end
    else
    begin
      DoStateChange(wsDisconnected);
    end;
  end;

  if fConnectionThread <> nil then
    fConnectionThread := nil;
end;

procedure TmORMot2WebSocketClient.InternalConnect;
var
  error: RawUtf8;
begin
  if fConnected then
    Exit;

  try
    // ALWAYS CREATE FRESH OBJECTS
    if fClient <> nil then
    begin
      try
        fClient.OnWebSocketsClosed := nil;
        FreeAndNil(fClient);
      except
        fClient := nil;
      end;
    end;

    fClient := THttpClientWebSockets.Create;
    fClient.OnWebSocketsClosed := HandleWebSocketsClosed;

    try
      fClient.Open(StringToUtf8(fHost), IntToStr(fPort));
    except
      on E: Exception do
      begin
        fLastError := 'Cannot connect to server: ' + E.Message;
        try
          FreeAndNil(fClient);
        except
          fClient := nil;
        end;
        raise Exception.Create(fLastError);
      end;
    end;

    fProtocol := TWebSocketClientProtocol.Create(Self);

    error := fClient.WebSocketsUpgrade(StringToUtf8(fURI), '', True, [], fProtocol, '');
    { fClient takes ownership of fProtocol via its internal protocol list.
      Null our reference — freeing fClient will free the protocol. }
    fProtocol := nil;

    if error <> '' then
    begin
      fLastError := 'WebSocket upgrade failed: ' + Utf8ToString(error);
      try FreeAndNil(fClient); except fClient := nil; end;
      raise Exception.Create(fLastError);
    end;

  except
    on E: Exception do
    begin
      fLastError := 'Connection failed: ' + E.Message;
      if fClient <> nil then
        try fClient.OnWebSocketsClosed := nil; FreeAndNil(fClient); except fClient := nil; end;
      raise;
    end;
  end;
end;

procedure TmORMot2WebSocketClient.Disconnect;
begin
  fUserDisconnected := True;
  StopReconnectTimer;
  DoStateChange(wsDisconnecting);
  InternalDisconnect;
end;

procedure TmORMot2WebSocketClient.InternalDisconnect;
var
  closeFrame: TWebSocketFrame;
begin
  if not fConnected and not fConnecting then
    Exit;

  try
    fConnected := False;
    fConnecting := False;
    fReconnecting := False;

    // Terminate and cleanup connection thread
    if fConnectionThread <> nil then
    begin
      try
        fConnectionThread.Terminate;
        fConnectionThread.WaitFor;
      except
        // Ignore thread termination errors
      end;
      fConnectionThread := nil;
    end;

    // Send close frame if connection still active
    if (fClient <> nil) and (fClient.WebSockets <> nil) then
    begin
      try
        closeFrame.opcode := focConnectionClose;
        closeFrame.content := [];
        closeFrame.payload := '';
        fClient.WebSockets.SendFrame(closeFrame);
        Sleep(10);
      except
        // Ignore close frame errors
      end;
    end;

    // Cleanup client
    if fClient <> nil then
    begin
      try
        fClient.OnWebSocketsClosed := nil;
        FreeAndNil(fClient);
      except
        fClient := nil;
      end;
    end;

    // Set final state and fire disconnect event
    DoStateChange(wsDisconnected);
    try
      DoDisconnect;
    except
      // Ignore event errors
    end;

  except
    on E: Exception do
    begin
      fLastError := 'Disconnect error: ' + E.Message;
      // Force cleanup even on error
      fClient := nil;
      fProtocol := nil;
      fConnected := False;
      fConnecting := False;
      fReconnecting := False;
    end;
  end;
end;

procedure TmORMot2WebSocketClient.HandleUnexpectedDisconnection;
begin
  if fAutoReconnect and
     not fUserDisconnected and
     ((fMaxReconnectAttempts = 0) or (fReconnectAttempts < fMaxReconnectAttempts)) then
  begin
    fReconnecting := True;
    DoStateChange(wsReconnecting);
    StartReconnectTimer;
  end;
end;

procedure TmORMot2WebSocketClient.StartReconnectTimer;
begin
  if fReconnectTimer <> nil then
  begin
    fReconnectTimer.Interval := fReconnectInterval;
    fReconnectTimer.Enabled := True;
  end;
end;

procedure TmORMot2WebSocketClient.StopReconnectTimer;
begin
  if fReconnectTimer <> nil then
    fReconnectTimer.Enabled := False;
end;

procedure TmORMot2WebSocketClient.OnReconnectTimer(Sender: TObject);
var
  actualInterval: Integer;
begin
  StopReconnectTimer;
  Inc(fReconnectAttempts);

  // Calculate interval based on strategy
  case fReconnectStrategy of
    rsLinear:
      actualInterval := fReconnectInterval;
    rsExponential:
      actualInterval := fReconnectInterval * (1 shl Min(fReconnectAttempts - 1, 10)); // Cap at 2^10
  else
    actualInterval := fReconnectInterval;
  end;

  // Update timer for next attempt if this one fails
  if fReconnectTimer <> nil then
    fReconnectTimer.Interval := actualInterval;

  DoReconnecting(fReconnectAttempts);
  AsyncConnect;
end;

// STATUS METHODS
function TmORMot2WebSocketClient.IsConnected: Boolean;
begin
  Result := fConnected and (fClient <> nil) and (fClient.WebSockets <> nil);
end;

function TmORMot2WebSocketClient.IsConnecting: Boolean;
begin
  Result := fConnecting;
end;

function TmORMot2WebSocketClient.IsReconnecting: Boolean;
begin
  Result := fReconnecting;
end;

function TmORMot2WebSocketClient.GetLastError: string;
begin
  Result := fLastError;
end;

function TmORMot2WebSocketClient.GetReconnectAttempts: Integer;
begin
  Result := fReconnectAttempts;
end;

procedure TmORMot2WebSocketClient.ResetReconnectAttempts;
begin
  fReconnectAttempts := 0;
end;

// FIXED EVENT METHODS - OnDataReceived for statistics, OnHandleCommand for processing
procedure TmORMot2WebSocketClient.DoError(const ErrorMsg: string);
begin
  if Assigned(fOnError) then
    fOnError(Self, ErrorMsg);
end;

// FIXED: OnDataReceived is for STATISTICS/LOGGING ONLY!
procedure TmORMot2WebSocketClient.DoDataReceived(const Data: TBytes);
begin
  UpdateBytesReceived(Length(Data));
  if Assigned(fOnDataReceived) then
    fOnDataReceived(Self, Data);
end;

// FIXED: OnDataSent is for STATISTICS/LOGGING ONLY!
procedure TmORMot2WebSocketClient.DoDataSent(const Data: TBytes);
begin
  UpdateBytesSent(Length(Data));
  if Assigned(fOnDataSent) then
    fOnDataSent(Self, Data);
end;

// FIXED: OnHandleCommand is for ACTUAL DATA PROCESSING!
procedure TmORMot2WebSocketClient.DoHandleCommand(const Command: TBytes);
begin
  if Assigned(fOnHandleCommand) then
    fOnHandleCommand(Self, Command);
end;

procedure TmORMot2WebSocketClient.DoConnect;
begin
  if Assigned(fOnConnect) then
    fOnConnect(Self);
end;

procedure TmORMot2WebSocketClient.DoDisconnect;
begin
  if Assigned(fOnDisconnect) then
    fOnDisconnect(Self);
end;

procedure TmORMot2WebSocketClient.DoStateChange(NewState: TWebSocketConnectionState);
var
  OldState: TWebSocketConnectionState;
begin
  OldState := fCurrentState;
  if OldState <> NewState then
  begin
    fCurrentState := NewState;
    if Assigned(fOnStateChange) then
      fOnStateChange(Self, OldState, NewState, GetConnectionStateDescription);
  end;
end;

procedure TmORMot2WebSocketClient.DoReconnecting(AttemptNumber: Integer);
begin
  if Assigned(fOnReconnecting) then
    fOnReconnecting(Self, AttemptNumber);
end;

procedure TmORMot2WebSocketClient.DoReconnectFailed(AttemptNumber: Integer; const ErrorMsg: string);
begin
  if Assigned(fOnReconnectFailed) then
    fOnReconnectFailed(Self, AttemptNumber, ErrorMsg);
end;

// FIXED SEND METHOD - auto-encrypts with proper coordination
function TmORMot2WebSocketClient.SendCommand(const Command: TBytes): Boolean;
var
  frame: TWebSocketFrame;
  payloadStr: RawByteString;
  dataToSend: TBytes;
begin
  Result := False;
  if not IsConnected or (Length(Command) = 0) then
    Exit;

  try
    dataToSend := Command;

    if fEncryptionEnabled then
      dataToSend := EncryptData(Command);

    SetLength(payloadStr, Length(dataToSend));
    Move(dataToSend[0], payloadStr[1], Length(dataToSend));

    frame.opcode := focText;
    frame.content := [];
    frame.tix := 0;
    frame.payload := payloadStr;
    Result := fClient.WebSockets.SendFrame(frame);

    if Result then
      DoDataSent(dataToSend); // Send the encrypted data size for statistics
  except
    on E: Exception do
    begin
      fLastError := E.Message;
      DoError(E.Message);
    end;
  end;
end;

procedure TmORMot2WebSocketClient.HandleWebSocketsClosed(Sender: TObject);
begin
  // Only handle this if we're still connected (not during manual disconnect)
  if fConnected then
  begin
    fConnected := False;
    try
      if fClient <> nil then
      begin
        try
          fClient.OnWebSocketsClosed := nil;
        except
          // Ignore event handler removal errors
        end;
      end;
    except
      // Ignore all cleanup errors in this handler
    end;

    try
      DoStateChange(wsDisconnected);
      DoDisconnect;
    except
      // Ignore event firing errors
    end;

    try
      HandleUnexpectedDisconnection;
    except
      // Ignore reconnection logic errors
    end;
  end;
end;

procedure Register;
begin
  RegisterComponents('mORMot2 WebSocket', [TmORMot2WebSocketClient]);
end;

end.
