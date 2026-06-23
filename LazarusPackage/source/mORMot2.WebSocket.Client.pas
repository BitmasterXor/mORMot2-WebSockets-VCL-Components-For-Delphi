{$mode delphi}
unit mORMot2.WebSocket.Client;

interface

uses
  Classes,
  SysUtils,
  SyncObjs,
  ExtCtrls,
  Math,
  TypInfo,
  Forms,
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
  ENCRYPTION_HEADER_SIZE = 16;
  ENCRYPTION_MAGIC       = $4D4F524D;

type
  TWebSocketConnectionState = (
    wsDisconnected,
    wsConnecting,
    wsConnected,
    wsReconnecting,
    wsDisconnecting,
    wsError
  );

  TWebSocketReconnectStrategy = (rsLinear, rsExponential);

  TAESMode    = (amECB, amCBC, amCFB, amOFB, amCTR, amGCM, amCFC, amOFC, amCTC);
  TAESKeySize = (aks128, aks192, aks256);

  TmORMot2WebSocketClient = class;

  TWebSocketConnectEvent        = procedure(Sender: TObject) of object;
  TWebSocketDataReceivedEvent   = procedure(Sender: TObject; const Data: TBytes) of object;
  TWebSocketDataSentEvent       = procedure(Sender: TObject; const Data: TBytes) of object;
  TWebSocketDisconnectEvent     = procedure(Sender: TObject) of object;
  TWebSocketErrorEvent          = procedure(Sender: TObject; const ErrorMsg: string) of object;
  TWebSocketHandleCommandEvent  = procedure(Sender: TObject; const Command: TBytes) of object;
  TWebSocketReconnectingEvent   = procedure(Sender: TObject; AttemptNumber: Integer) of object;
  TWebSocketReconnectFailedEvent= procedure(Sender: TObject; AttemptNumber: Integer; const ErrorMsg: string) of object;
  TWebSocketStateChangeEvent    = procedure(Sender: TObject; OldState, NewState: TWebSocketConnectionState; const StateDescription: string) of object;

  TWebSocketClientProtocol = class(TWebSocketProtocolJson)
  protected
    fOwner: TmORMot2WebSocketClient;
    procedure ProcessIncomingFrame(Sender: TWebSocketProcess;
      var Request: TWebSocketFrame; const Info: RawUtf8); override;
  public
    constructor Create(AOwner: TmORMot2WebSocketClient);
    function Clone(const aClientUri: RawUtf8): TWebSocketProtocol; override;
  end;

  TmORMot2WebSocketClient = class(TComponent)
  private
    fClient:          THttpClientWebSockets;
    fProtocol:        TWebSocketClientProtocol;
    fHost:            string;
    fPort:            Integer;
    fURI:             string;
    fConnected:       Boolean;
    fConnecting:      Boolean;
    fLastError:       string;
    fConnectionThread: TThread;
    fCurrentState:    TWebSocketConnectionState;
    fActive:          Boolean;
    fConnectionTimeout: Integer;
    fDescription:     string;
    fKeepAlive:       Boolean;
    fLogLevel:        Integer;
    fMessageReceived: Boolean;
    fMessageSent:     Boolean;
    fNoDelay:         Boolean;
    fReceiveBufferSize: Integer;
    fReconnectStrategy: TWebSocketReconnectStrategy;
    fSendBufferSize:  Integer;
    fThreadPoolSize:  Integer;
    fTotalBytesReceived: Int64;
    fTotalBytesSent:     Int64;
    fVersion:         string;
    fEncryptionEnabled: Boolean;
    fEncryptionKey:     string;
    fEncryptionMode:    TAESMode;
    fEncryptionKeySize: TAESKeySize;
    { Pre-derived AES key — refreshed whenever EncryptionKey/Mode/KeySize changes }
    fCryptoLock:  TCriticalSection;
    fDerivedKey:  THash256;
    fKeyDerived:  Boolean;
    fAutoReconnect:       Boolean;
    fReconnectInterval:   Integer;
    fReconnectTimer:      TTimer;
    fReconnectAttempts:   Integer;
    fMaxReconnectAttempts: Integer;
    fReconnecting:    Boolean;
    fUserDisconnected: Boolean;
    fIsDestroying:    Boolean;
    fSyncLock:        TCriticalSection;
    fSyncRawBytes:    TBytes;
    fSyncDecBytes:    TBytes;
    fOnConnect:           TWebSocketConnectEvent;
    fOnDataReceived:      TWebSocketDataReceivedEvent;
    fOnDataSent:          TWebSocketDataSentEvent;
    fOnDisconnect:        TWebSocketDisconnectEvent;
    fOnError:             TWebSocketErrorEvent;
    fOnHandleCommand:     TWebSocketHandleCommandEvent;
    fOnStateChange:       TWebSocketStateChangeEvent;
    fOnReconnecting:      TWebSocketReconnectingEvent;
    fOnReconnectFailed:   TWebSocketReconnectFailedEvent;

    procedure SetActive(const Value: Boolean);
    procedure SetConnectionTimeout(const Value: Integer);
    procedure SetDescription(const Value: string);
    procedure SetKeepAlive(const Value: Boolean);
    procedure SetLogLevel(const Value: Integer);
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
    procedure SetEncryptionEnabled(const Value: Boolean);
    procedure SetEncryptionKey(const Value: string);
    procedure SetEncryptionMode(const Value: TAESMode);
    procedure SetEncryptionKeySize(const Value: TAESKeySize);
    procedure DeriveEncryptionKey;
    function  GetEncryptionInfo: string;
    function  ConnectionStateToString(State: TWebSocketConnectionState): string;
    function  GetConnectionStateDescription: string;
    function  EncryptData(const Data: TBytes): TBytes;
    function  DecryptData(const Data: TBytes): TBytes;
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
    { Synchronized event dispatchers — always called on the main thread }
    procedure SyncFireConnect;
    procedure SyncFireDisconnect;
    procedure SyncFireData;

  protected
    procedure InternalConnect;
    procedure InternalDisconnect;
    procedure AsyncConnect;
    procedure HandleUnexpectedDisconnection;

  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;
    function  Connect: Boolean;
    function  ConnectSync: Boolean;
    procedure Disconnect;
    function  SendCommand(const Command: TBytes): Boolean;
    function  IsConnected: Boolean;
    function  IsConnecting: Boolean;
    function  IsReconnecting: Boolean;
    function  GetLastError: string;
    function  GetReconnectAttempts: Integer;
    procedure ResetReconnectAttempts;
    function  GetTotalBytesReceived: Int64;
    function  GetTotalBytesSent: Int64;
    procedure ResetByteCounters;
    function  GetServerIP: string;
    function  GetConnectionStateAsString: string;

  published
    property Active:             Boolean  read fActive            write SetActive            default False;
    property Host:               string   read fHost              write SetHost;
    property Port:               Integer  read fPort              write SetPort              default 80;
    property URI:                string   read fURI               write SetURI;
    property Connected:          Boolean  read fConnected         write SetConnected         default False;
    property ConnectionTimeout:  Integer  read fConnectionTimeout write SetConnectionTimeout default 30000;
    property Description:        string   read fDescription       write SetDescription;
    property KeepAlive:          Boolean  read fKeepAlive         write SetKeepAlive         default True;
    property LogLevel:           Integer  read fLogLevel          write SetLogLevel          default 1;
    property NoDelay:            Boolean  read fNoDelay           write SetNoDelay           default True;
    property ReceiveBufferSize:  Integer  read fReceiveBufferSize write SetReceiveBufferSize default 8192;
    property SendBufferSize:     Integer  read fSendBufferSize    write SetSendBufferSize    default 8192;
    property ThreadPoolSize:     Integer  read fThreadPoolSize    write SetThreadPoolSize    default 4;
    property Version:            string   read fVersion           write SetVersion;
    property EncryptionEnabled:  Boolean    read fEncryptionEnabled  write SetEncryptionEnabled  default False;
    property EncryptionKey:      string     read fEncryptionKey      write SetEncryptionKey;
    property EncryptionMode:     TAESMode   read fEncryptionMode     write SetEncryptionMode     default amCBC;
    property EncryptionKeySize:  TAESKeySize read fEncryptionKeySize write SetEncryptionKeySize  default aks256;
    property EncryptionInfo:     string     read GetEncryptionInfo;
    property Connecting:             Boolean  read fConnecting;
    property AutoReconnect:          Boolean  read fAutoReconnect         write SetAutoReconnect         default False;
    property ReconnectInterval:      Integer  read fReconnectInterval     write SetReconnectInterval     default 5000;
    property MaxReconnectAttempts:   Integer  read fMaxReconnectAttempts  write SetMaxReconnectAttempts  default 0;
    property ReconnectStrategy:      TWebSocketReconnectStrategy read fReconnectStrategy write SetReconnectStrategy default rsLinear;
    property Reconnecting:           Boolean  read fReconnecting;
    property MessageReceived:        Boolean  read fMessageReceived;
    property MessageSent:            Boolean  read fMessageSent;
    property TotalBytesReceived:     Int64    read fTotalBytesReceived;
    property TotalBytesSent:         Int64    read fTotalBytesSent;
    property ConnectionState:        TWebSocketConnectionState read fCurrentState;
    property ConnectionStateDescription: string read GetConnectionStateDescription;
    property OnConnect:          TWebSocketConnectEvent         read fOnConnect         write fOnConnect;
    property OnDataReceived:     TWebSocketDataReceivedEvent    read fOnDataReceived    write fOnDataReceived;
    property OnDataSent:         TWebSocketDataSentEvent        read fOnDataSent        write fOnDataSent;
    property OnDisconnect:       TWebSocketDisconnectEvent      read fOnDisconnect      write fOnDisconnect;
    property OnError:            TWebSocketErrorEvent           read fOnError           write fOnError;
    property OnHandleCommand:    TWebSocketHandleCommandEvent   read fOnHandleCommand   write fOnHandleCommand;
    property OnStateChange:      TWebSocketStateChangeEvent     read fOnStateChange     write fOnStateChange;
    property OnReconnecting:     TWebSocketReconnectingEvent    read fOnReconnecting    write fOnReconnecting;
    property OnReconnectFailed:  TWebSocketReconnectFailedEvent read fOnReconnectFailed write fOnReconnectFailed;
  end;

procedure Register;

implementation

{ TAsyncConnectThread }

type
  TAsyncConnectThread = class(TThread)
  private
    fOwner:    TmORMot2WebSocketClient;
    fSuccess:  Boolean;
    fErrorMsg: string;
    procedure SyncComplete;
  protected
    procedure Execute; override;
  public
    constructor Create(AOwner: TmORMot2WebSocketClient);
  end;

constructor TAsyncConnectThread.Create(AOwner: TmORMot2WebSocketClient);
begin
  fOwner          := AOwner;
  fSuccess        := False;
  fErrorMsg       := '';
  FreeOnTerminate := True;
  inherited Create(True);
end;

procedure TAsyncConnectThread.Execute;
begin
  fSuccess  := False;
  fErrorMsg := '';
  try
    fOwner.InternalConnect;
    fSuccess := True;
  except
    on E: Exception do
    begin
      fErrorMsg        := E.Message;
      fOwner.fLastError := E.Message;
    end;
  end;
  if not Terminated then
    Synchronize(SyncComplete);
end;

procedure TAsyncConnectThread.SyncComplete;
begin
  fOwner.HandleConnectionComplete(fSuccess, fErrorMsg);
end;

{ TWebSocketClientProtocol }

constructor TWebSocketClientProtocol.Create(AOwner: TmORMot2WebSocketClient);
begin
  fOwner := AOwner;
  inherited Create('');
end;

function TWebSocketClientProtocol.Clone(const aClientUri: RawUtf8): TWebSocketProtocol;
begin
  Result := TWebSocketClientProtocol.Create(fOwner);
end;

procedure TWebSocketClientProtocol.ProcessIncomingFrame(
  Sender: TWebSocketProcess; var Request: TWebSocketFrame; const Info: RawUtf8);
var
  rawBytes:       TBytes;
  decryptedBytes: TBytes;
begin
  if (fOwner = nil) or fOwner.fIsDestroying or (Sender = nil) then
  begin
    inherited ProcessIncomingFrame(Sender, Request, Info);
    Exit;
  end;

  case Request.opcode of
    focContinuation:
      begin
        if not fOwner.fConnected then
        begin
          fOwner.fReconnectAttempts := 0;
          fOwner.fReconnecting      := False;
          { SyncFireConnect sets fConnected=True, stops timer, fires events on main thread }
          if GetCurrentThreadID = MainThreadID then
            fOwner.SyncFireConnect
          else
            TThread.Synchronize(nil, fOwner.SyncFireConnect);
        end;
      end;

    focConnectionClose:
      begin
        if fOwner.fConnected then
        begin
          { SyncFireDisconnect sets fConnected=False, fires events on main thread }
          if GetCurrentThreadID = MainThreadID then
            fOwner.SyncFireDisconnect
          else
            TThread.Synchronize(nil, fOwner.SyncFireDisconnect);
        end;
      end;

    focText:
      begin
        SetLength(rawBytes, Length(Request.payload));
        if Length(rawBytes) > 0 then
          Move(Request.payload[1], rawBytes[0], Length(rawBytes));

        decryptedBytes := rawBytes;
        if fOwner.fEncryptionEnabled then
          decryptedBytes := fOwner.DecryptData(rawBytes);

        fOwner.fSyncLock.Enter;
        try
          fOwner.fSyncRawBytes := rawBytes;
          fOwner.fSyncDecBytes := decryptedBytes;
        finally
          fOwner.fSyncLock.Leave;
        end;

        { SyncFireData fires DoDataReceived + DoHandleCommand on main thread }
        if GetCurrentThreadID = MainThreadID then
          fOwner.SyncFireData
        else
          TThread.Synchronize(nil, fOwner.SyncFireData);
      end;
  end;

  inherited ProcessIncomingFrame(Sender, Request, Info);
end;

{ TmORMot2WebSocketClient }

constructor TmORMot2WebSocketClient.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  fIsDestroying     := False;
  fSyncLock         := TCriticalSection.Create;
  fCryptoLock       := TCriticalSection.Create;
  fHost             := 'localhost';
  fPort             := 80;
  fURI              := '/';
  fConnected        := False;
  fConnecting       := False;
  fClient           := nil;
  fProtocol         := nil;
  fConnectionThread := nil;
  fCurrentState     := wsDisconnected;
  fActive           := False;
  fConnectionTimeout := 30000;
  fDescription      := '';
  fKeepAlive        := True;
  fLogLevel         := 1;
  fNoDelay          := True;
  fReceiveBufferSize := 8192;
  fSendBufferSize   := 8192;
  fThreadPoolSize   := 4;
  fVersion          := '2.0.7';
  fMessageReceived  := False;
  fMessageSent      := False;
  fTotalBytesReceived := 0;
  fTotalBytesSent     := 0;
  fEncryptionEnabled  := False;
  fEncryptionKey      := '';
  fEncryptionMode     := amCBC;
  fEncryptionKeySize  := aks256;
  fKeyDerived         := False;
  FillChar(fDerivedKey, SizeOf(fDerivedKey), 0);
  fAutoReconnect        := False;
  fReconnectInterval    := 5000;
  fMaxReconnectAttempts := 0;
  fReconnectAttempts    := 0;
  fReconnectStrategy    := rsLinear;
  fReconnecting         := False;
  fUserDisconnected     := False;
  fReconnectTimer          := TTimer.Create(Self);
  fReconnectTimer.Enabled  := False;
  fReconnectTimer.OnTimer  := OnReconnectTimer;
end;

destructor TmORMot2WebSocketClient.Destroy;
begin
  fIsDestroying := True;
  try
    try StopReconnectTimer; except end;
    if fConnectionThread <> nil then
    begin
      try fConnectionThread.Terminate; except end;
      if fClient <> nil then
        try fClient.Close; except end;
      fConnectionThread := nil;
    end;
    if fClient <> nil then
    begin
      try
        fClient.OnWebSocketsClosed := nil;
        FreeAndNil(fClient);  { fClient owns fProtocol — freed here }
      except
        fClient := nil;
      end;
    end;
  except
    fClient           := nil;
    fConnectionThread := nil;
  end;
  FillChar(fDerivedKey, SizeOf(fDerivedKey), 0);
  FreeAndNil(fCryptoLock);
  FreeAndNil(fSyncLock);
  inherited Destroy;
end;

{ Synchronized event dispatchers — always run on the main thread }

procedure TmORMot2WebSocketClient.SyncFireConnect;
begin
  if fIsDestroying then Exit;
  fConnected := True;
  StopReconnectTimer;
  DoStateChange(wsConnected);
  DoConnect;
end;

procedure TmORMot2WebSocketClient.SyncFireDisconnect;
begin
  if fIsDestroying then Exit;
  if not fConnected then Exit;
  fConnected := False;
  try
    if fClient <> nil then
      try fClient.OnWebSocketsClosed := nil; except end;
  except end;
  DoStateChange(wsDisconnected);
  DoDisconnect;
  HandleUnexpectedDisconnection;
end;

procedure TmORMot2WebSocketClient.SyncFireData;
var
  raw, dec: TBytes;
begin
  if fIsDestroying then Exit;
  fSyncLock.Enter;
  try
    raw := fSyncRawBytes;
    dec := fSyncDecBytes;
  finally
    fSyncLock.Leave;
  end;
  DoDataReceived(raw);
  DoHandleCommand(dec);
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

function TmORMot2WebSocketClient.GetEncryptionInfo: string;
const
  ModeNames:    array[TAESMode]    of string = ('ECB','CBC','CFB','OFB','CTR','GCM','CFC','OFC','CTC');
  KeySizeNames: array[TAESKeySize] of string = ('128','192','256');
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
    Result := Data;
  end;
  FillChar(DKey, SizeOf(DKey), 0);
end;

function TmORMot2WebSocketClient.DecryptData(const Data: TBytes): TBytes;
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
  Move(Data[0], Header[0], ENCRYPTION_HEADER_SIZE);
  Magic := PCardinal(@Header[0])^;
  if Magic <> ENCRYPTION_MAGIC then Exit;
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
    Result := Data;  { wrong key or corrupt — return raw bytes silently }
  end;
  FillChar(DKey, SizeOf(DKey), 0);
end;

function TmORMot2WebSocketClient.ConnectionStateToString(
  State: TWebSocketConnectionState): string;
begin
  case State of
    wsDisconnected:  Result := 'Disconnected';
    wsConnecting:    Result := 'Connecting';
    wsConnected:     Result := 'Connected';
    wsReconnecting:  Result := 'Reconnecting';
    wsDisconnecting: Result := 'Disconnecting';
    wsError:         Result := 'Error';
  else
    Result := 'Unknown';
  end;
end;

function TmORMot2WebSocketClient.GetConnectionStateDescription: string;
begin
  case fCurrentState of
    wsDisconnected:  Result := Format('Disconnected from %s:%d', [fHost, fPort]);
    wsConnecting:    Result := Format('Connecting to %s:%d%s', [fHost, fPort, fURI]);
    wsConnected:     Result := Format('Connected to %s:%d%s', [fHost, fPort, fURI]);
    wsReconnecting:  Result := Format('Reconnecting to %s:%d%s (attempt %d)', [fHost, fPort, fURI, fReconnectAttempts + 1]);
    wsDisconnecting: Result := Format('Disconnecting from %s:%d', [fHost, fPort]);
    wsError:         Result := Format('Connection error with %s:%d - %s', [fHost, fPort, fLastError]);
  else
    Result := 'Unknown connection state';
  end;
end;

function TmORMot2WebSocketClient.GetConnectionStateAsString: string;
begin
  Result := ConnectionStateToString(fCurrentState);
end;

function TmORMot2WebSocketClient.GetServerIP: string;
begin
  if fHost = 'localhost' then
    Result := '127.0.0.1'
  else
    Result := fHost;
end;

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
  if Value >= 1000 then fConnectionTimeout := Value;
end;

procedure TmORMot2WebSocketClient.SetDescription(const Value: string);
begin fDescription := Value; end;

procedure TmORMot2WebSocketClient.SetKeepAlive(const Value: Boolean);
begin fKeepAlive := Value; end;

procedure TmORMot2WebSocketClient.SetLogLevel(const Value: Integer);
begin
  if (Value >= 0) and (Value <= 3) then fLogLevel := Value;
end;

procedure TmORMot2WebSocketClient.SetNoDelay(const Value: Boolean);
begin fNoDelay := Value; end;

procedure TmORMot2WebSocketClient.SetReceiveBufferSize(const Value: Integer);
begin
  if Value >= 1024 then fReceiveBufferSize := Value;
end;

procedure TmORMot2WebSocketClient.SetReconnectStrategy(
  const Value: TWebSocketReconnectStrategy);
begin fReconnectStrategy := Value; end;

procedure TmORMot2WebSocketClient.SetSendBufferSize(const Value: Integer);
begin
  if Value >= 1024 then fSendBufferSize := Value;
end;

procedure TmORMot2WebSocketClient.SetThreadPoolSize(const Value: Integer);
begin
  if Value >= 1 then fThreadPoolSize := Value;
end;

procedure TmORMot2WebSocketClient.SetVersion(const Value: string);
begin fVersion := Value; end;

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
begin Result := fTotalBytesReceived; end;

function TmORMot2WebSocketClient.GetTotalBytesSent: Int64;
begin Result := fTotalBytesSent; end;

procedure TmORMot2WebSocketClient.ResetByteCounters;
begin
  fTotalBytesReceived := 0;
  fTotalBytesSent     := 0;
  fMessageReceived    := False;
  fMessageSent        := False;
end;

procedure TmORMot2WebSocketClient.SetConnected(const Value: Boolean);
begin
  if fConnected <> Value then
  begin
    if Value then Connect else Disconnect;
  end;
end;

procedure TmORMot2WebSocketClient.SetHost(const Value: string);
begin
  if (fHost <> Value) and not (fConnected or fConnecting) then
    fHost := Value;
end;

procedure TmORMot2WebSocketClient.SetPort(const Value: Integer);
begin
  if (fPort <> Value) and not (fConnected or fConnecting) then
    fPort := Value;
end;

procedure TmORMot2WebSocketClient.SetURI(const Value: string);
begin
  if (fURI <> Value) and not (fConnected or fConnecting) then
    fURI := Value;
end;

procedure TmORMot2WebSocketClient.SetAutoReconnect(const Value: Boolean);
begin
  if fAutoReconnect <> Value then
  begin
    fAutoReconnect := Value;
    if not Value then StopReconnectTimer;
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
  if Value >= 0 then fMaxReconnectAttempts := Value;
end;

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
  { FreeOnTerminate=True means we must never call WaitFor/FreeAndNil.
    Terminate + nil the reference; the thread frees itself when done. }
  if fConnectionThread <> nil then
  begin
    fConnectionThread.Terminate;
    { Close the socket so any blocking TCP call in the thread returns immediately }
    if fClient <> nil then
      try fClient.Close; except end;
    fConnectionThread := nil;
  end;
  fConnecting       := True;
  fConnectionThread := TAsyncConnectThread.Create(Self);
  fConnectionThread.Start;
end;

procedure TmORMot2WebSocketClient.HandleConnectionComplete(Success: Boolean;
  const ErrorMsg: string);
begin
  { Always called on the main thread via Synchronize.
    Guard against destruction or an already-aborted connect sequence. }
  fConnectionThread := nil;
  if fIsDestroying then Exit;
  if not fConnecting then Exit;

  fConnecting := False;
  if not Success then
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
      DoStateChange(wsDisconnected);
  end;
end;

procedure TmORMot2WebSocketClient.InternalConnect;
var
  error: RawUtf8;
begin
  if fConnected then Exit;
  try
    { Clean up any stale client from a previous connection }
    if fClient <> nil then
      try fClient.OnWebSocketsClosed := nil; FreeAndNil(fClient); except fClient := nil; end;

    fClient := THttpClientWebSockets.Create;
    fClient.OnWebSocketsClosed := HandleWebSocketsClosed;
    try
      fClient.Open(StringToUtf8(fHost), IntToStr(fPort));
    except
      on E: Exception do
      begin
        fLastError := 'Cannot connect to server: ' + E.Message;
        try FreeAndNil(fClient); except fClient := nil; end;
        raise Exception.Create(fLastError);
      end;
    end;

    fProtocol := TWebSocketClientProtocol.Create(Self);
    error     := fClient.WebSocketsUpgrade(StringToUtf8(fURI), '', True, [], fProtocol, '');
    { fClient.WebSocketsUpgrade adds fProtocol to its internal protocol list and
      takes ownership. We must never free fProtocol independently after this point;
      freeing fClient will free the protocol. Null our reference immediately. }
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
    fConnected    := False;
    fConnecting   := False;
    fReconnecting := False;

    { Terminate the connection thread without WaitFor to avoid Synchronize deadlock.
      FreeOnTerminate=True — thread will free itself after it finishes. }
    if fConnectionThread <> nil then
    begin
      try fConnectionThread.Terminate; except end;
      fConnectionThread := nil;
    end;

    if (fClient <> nil) and (fClient.WebSockets <> nil) then
    begin
      try
        closeFrame.opcode  := focConnectionClose;
        closeFrame.content := [];
        closeFrame.payload := '';
        fClient.WebSockets.SendFrame(closeFrame);
        Sleep(10);
      except
      end;
    end;

    if fClient <> nil then
    begin
      try
        fClient.OnWebSocketsClosed := nil;
        FreeAndNil(fClient);  { fClient owns fProtocol — freed here }
      except
        fClient := nil;
      end;
    end;

    DoStateChange(wsDisconnected);
    try DoDisconnect; except end;
  except
    on E: Exception do
    begin
      fLastError    := 'Disconnect error: ' + E.Message;
      fClient       := nil;
      fConnected    := False;
      fConnecting   := False;
      fReconnecting := False;
    end;
  end;
end;

procedure TmORMot2WebSocketClient.HandleUnexpectedDisconnection;
begin
  if fAutoReconnect and not fUserDisconnected and
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
    fReconnectTimer.Enabled  := True;
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
  case fReconnectStrategy of
    rsLinear:      actualInterval := fReconnectInterval;
    rsExponential: actualInterval := fReconnectInterval * (1 shl Min(fReconnectAttempts - 1, 10));
  else
    actualInterval := fReconnectInterval;
  end;
  if fReconnectTimer <> nil then
    fReconnectTimer.Interval := actualInterval;
  DoReconnecting(fReconnectAttempts);
  AsyncConnect;
end;

function TmORMot2WebSocketClient.IsConnected: Boolean;
begin
  Result := fConnected and (fClient <> nil) and (fClient.WebSockets <> nil);
end;

function TmORMot2WebSocketClient.IsConnecting: Boolean;
begin Result := fConnecting; end;

function TmORMot2WebSocketClient.IsReconnecting: Boolean;
begin Result := fReconnecting; end;

function TmORMot2WebSocketClient.GetLastError: string;
begin Result := fLastError; end;

function TmORMot2WebSocketClient.GetReconnectAttempts: Integer;
begin Result := fReconnectAttempts; end;

procedure TmORMot2WebSocketClient.ResetReconnectAttempts;
begin fReconnectAttempts := 0; end;

procedure TmORMot2WebSocketClient.DoError(const ErrorMsg: string);
begin
  if Assigned(fOnError) then fOnError(Self, ErrorMsg);
end;

procedure TmORMot2WebSocketClient.DoDataReceived(const Data: TBytes);
begin
  UpdateBytesReceived(Length(Data));
  if Assigned(fOnDataReceived) then fOnDataReceived(Self, Data);
end;

procedure TmORMot2WebSocketClient.DoDataSent(const Data: TBytes);
begin
  UpdateBytesSent(Length(Data));
  if Assigned(fOnDataSent) then fOnDataSent(Self, Data);
end;

procedure TmORMot2WebSocketClient.DoHandleCommand(const Command: TBytes);
begin
  if Assigned(fOnHandleCommand) then fOnHandleCommand(Self, Command);
end;

procedure TmORMot2WebSocketClient.DoConnect;
begin
  if Assigned(fOnConnect) then fOnConnect(Self);
end;

procedure TmORMot2WebSocketClient.DoDisconnect;
begin
  if Assigned(fOnDisconnect) then fOnDisconnect(Self);
end;

procedure TmORMot2WebSocketClient.DoStateChange(
  NewState: TWebSocketConnectionState);
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
  if Assigned(fOnReconnecting) then fOnReconnecting(Self, AttemptNumber);
end;

procedure TmORMot2WebSocketClient.DoReconnectFailed(AttemptNumber: Integer;
  const ErrorMsg: string);
begin
  if Assigned(fOnReconnectFailed) then fOnReconnectFailed(Self, AttemptNumber, ErrorMsg);
end;

{ HandleWebSocketsClosed is called from mORMot2's network thread.
  Delegate to SyncFireDisconnect which safely transitions state on the main thread. }
procedure TmORMot2WebSocketClient.HandleWebSocketsClosed(Sender: TObject);
begin
  if fConnected and not fIsDestroying then
  begin
    if GetCurrentThreadID = MainThreadID then
      SyncFireDisconnect
    else
      TThread.Synchronize(nil, SyncFireDisconnect);
  end;
end;

function TmORMot2WebSocketClient.SendCommand(const Command: TBytes): Boolean;
var
  frame:      TWebSocketFrame;
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
    frame.opcode  := focText;
    frame.content := [];
    frame.tix     := 0;
    frame.payload := payloadStr;
    Result := fClient.WebSockets.SendFrame(frame);
    if Result then
      DoDataSent(dataToSend);
  except
    on E: Exception do
    begin
      fLastError := E.Message;
      DoError(E.Message);
    end;
  end;
end;

procedure Register;
begin
  RegisterComponents('mORMot2 WebSocket', [TmORMot2WebSocketClient]);
end;

end.
