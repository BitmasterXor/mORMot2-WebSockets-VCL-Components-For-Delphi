{$mode delphi}
unit Unit1;

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs,
  StdCtrls, ExtCtrls, Spin,
  mORMot2.WebSocket.Server,
  mORMot2.WebSocket.Client;

type

  { TForm1 }

  TForm1 = class(TForm)
    GroupBoxServer:     TGroupBox;
    LabelPort:          TLabel;
    LabelServerStatus:  TLabel;
    LabelClients:       TLabel;
    LabelMaxConn:       TLabel;
    LabelEncKey:        TLabel;
    SpinEditPort:       TSpinEdit;
    BtnStartServer:     TButton;
    BtnStopServer:      TButton;
    SpinEditMaxConn:    TSpinEdit;
    CheckEncryptServer: TCheckBox;
    EditServerEncKey:   TEdit;
    EditBroadcastMsg:   TEdit;
    BtnBroadcast:       TButton;
    BtnResetStats:      TButton;
    GroupBoxClient:     TGroupBox;
    LabelHost:          TLabel;
    LabelClientPort:    TLabel;
    LabelURI:           TLabel;
    LabelClientStatus:  TLabel;
    LabelBytesRcvd:     TLabel;
    LabelBytesSent:     TLabel;
    EditHost:           TEdit;
    SpinEditClientPort: TSpinEdit;
    EditURI:            TEdit;
    BtnConnect:         TButton;
    BtnDisconnect:      TButton;
    CheckEncryptClient: TCheckBox;
    EditClientEncKey:   TEdit;
    CheckAutoReconnect: TCheckBox;
    EditSendMsg:        TEdit;
    BtnSend:            TButton;
    GroupBoxLog:        TGroupBox;
    MemoLog:            TMemo;
    BtnClearLog:        TButton;
    TimerStats:         TTimer;
    // Non-visual components dropped from the palette
    WebSocketServer:    TmORMot2WebSocketServer;
    WebSocketClient:    TmORMot2WebSocketClient;
    // Form events
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    // Server button handlers
    procedure BtnStartServerClick(Sender: TObject);
    procedure BtnStopServerClick(Sender: TObject);
    procedure BtnBroadcastClick(Sender: TObject);
    procedure BtnResetStatsClick(Sender: TObject);
    // Client button handlers
    procedure BtnConnectClick(Sender: TObject);
    procedure BtnDisconnectClick(Sender: TObject);
    procedure BtnSendClick(Sender: TObject);
    // Log
    procedure BtnClearLogClick(Sender: TObject);
    // Timer
    procedure TimerStatsTimer(Sender: TObject);
    // Server component events (wired in LFM / Object Inspector)
    procedure ServerClientConnected(Sender: TObject; ClientID: Integer);
    procedure ServerClientDisconnected(Sender: TObject; ClientID: Integer);
    procedure ServerHandleCommand(Sender: TObject; ClientID: Integer; const Command: TBytes);
    procedure ServerError(Sender: TObject; const ErrorMsg: string);
    procedure ServerStateChange(Sender: TObject; OldState, NewState: TWebSocketServerState;
                                const StateDescription: string);
    // Client component events (wired in LFM / Object Inspector)
    procedure ClientConnect(Sender: TObject);
    procedure ClientDisconnect(Sender: TObject);
    procedure ClientHandleCommand(Sender: TObject; const Command: TBytes);
    procedure ClientError(Sender: TObject; const ErrorMsg: string);
    procedure ClientStateChange(Sender: TObject; OldState, NewState: TWebSocketConnectionState;
                                const StateDescription: string);
    procedure ClientReconnecting(Sender: TObject; AttemptNumber: Integer);
    procedure WebSocketServerDataReceived(Sender: TObject; ClientID: Integer;
      const Data: TBytes);
  private
    procedure Log(const Msg: string);
    procedure UpdateServerUI;
    procedure UpdateClientUI;
  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

// ---------------------------------------------------------------------------
// Form lifecycle — components configured by the LFM / Object Inspector
// ---------------------------------------------------------------------------

procedure TForm1.FormCreate(Sender: TObject);
begin
  TimerStats.Enabled := True;
  Log('mORMot2 WebSocket Demo started (Lazarus)');
  Log('Server and Client components ready.');
  UpdateServerUI;
  UpdateClientUI;
end;

procedure TForm1.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  TimerStats.Enabled := False;
  if WebSocketClient.IsConnected then WebSocketClient.Disconnect;
  if WebSocketServer.Active      then WebSocketServer.Stop;
end;

// ---------------------------------------------------------------------------
// Helpers
// ---------------------------------------------------------------------------

procedure TForm1.Log(const Msg: string);
begin
  MemoLog.Lines.Add('[' + FormatDateTime('hh:nn:ss', Now) + '] ' + Msg);
  MemoLog.SelStart  := Length(MemoLog.Text);
  MemoLog.SelLength := 0;
end;

procedure TForm1.UpdateServerUI;
begin
  BtnStartServer.Enabled := not WebSocketServer.Active;
  BtnStopServer.Enabled  := WebSocketServer.Active;
  BtnBroadcast.Enabled   := WebSocketServer.Active;
  LabelServerStatus.Caption := 'Status: ' + WebSocketServer.GetServerStateAsString;
  LabelClients.Caption      := Format('Clients: %d', [WebSocketServer.GetClientCount]);
end;

procedure TForm1.UpdateClientUI;
begin
  BtnConnect.Enabled    := not (WebSocketClient.IsConnected or WebSocketClient.IsConnecting);
  BtnDisconnect.Enabled := WebSocketClient.IsConnected or WebSocketClient.IsConnecting;
  BtnSend.Enabled       := WebSocketClient.IsConnected;
  LabelClientStatus.Caption := 'Status: ' + WebSocketClient.GetConnectionStateAsString;
  LabelBytesRcvd.Caption    := Format('Bytes Received: %d', [WebSocketClient.GetTotalBytesReceived]);
  LabelBytesSent.Caption    := Format('Bytes Sent: %d',     [WebSocketClient.GetTotalBytesSent]);
end;

// ---------------------------------------------------------------------------
// Server buttons
// ---------------------------------------------------------------------------

procedure TForm1.BtnStartServerClick(Sender: TObject);
begin
  WebSocketServer.Port           := SpinEditPort.Value;
  WebSocketServer.MaxConnections := SpinEditMaxConn.Value;
  WebSocketServer.EncryptionEnabled := CheckEncryptServer.Checked;
  if CheckEncryptServer.Checked then
    WebSocketServer.EncryptionKey := EditServerEncKey.Text;
  if WebSocketServer.Start then
    Log(Format('Server started on port %d', [WebSocketServer.Port]))
  else
    Log('Server failed to start: ' + WebSocketServer.GetLastError);
  UpdateServerUI;
end;

procedure TForm1.BtnStopServerClick(Sender: TObject);
begin
  WebSocketServer.Stop;
  Log('Server stopped');
  UpdateServerUI;
end;

procedure TForm1.BtnBroadcastClick(Sender: TObject);
var
  Msg: string;
begin
  Msg := EditBroadcastMsg.Text;
  if Msg = '' then Exit;
  WebSocketServer.BroadcastCommand(TEncoding.UTF8.GetBytes(Msg));
  Log('Broadcast sent: ' + Msg);
end;

procedure TForm1.BtnResetStatsClick(Sender: TObject);
begin
  WebSocketServer.ResetStatistics;
  Log('Server statistics reset');
end;

// ---------------------------------------------------------------------------
// Client buttons
// ---------------------------------------------------------------------------

procedure TForm1.BtnConnectClick(Sender: TObject);
begin
  WebSocketClient.Host              := EditHost.Text;
  WebSocketClient.Port              := SpinEditClientPort.Value;
  WebSocketClient.URI               := EditURI.Text;
  WebSocketClient.EncryptionEnabled := CheckEncryptClient.Checked;
  if CheckEncryptClient.Checked then
    WebSocketClient.EncryptionKey   := EditClientEncKey.Text;
  WebSocketClient.AutoReconnect     := CheckAutoReconnect.Checked;
  Log(Format('Connecting to %s:%d%s ...', [WebSocketClient.Host, WebSocketClient.Port, WebSocketClient.URI]));
  WebSocketClient.Connect;
  UpdateClientUI;
end;

procedure TForm1.BtnDisconnectClick(Sender: TObject);
begin
  WebSocketClient.Disconnect;
end;

procedure TForm1.BtnSendClick(Sender: TObject);
var
  Msg: string;
begin
  Msg := EditSendMsg.Text;
  if Msg = '' then Exit;
  if WebSocketClient.SendCommand(TEncoding.UTF8.GetBytes(Msg)) then
    Log('Sent: ' + Msg)
  else
    Log('Send failed: ' + WebSocketClient.GetLastError);
end;

// ---------------------------------------------------------------------------
// Log
// ---------------------------------------------------------------------------

procedure TForm1.BtnClearLogClick(Sender: TObject);
begin
  MemoLog.Clear;
end;

// ---------------------------------------------------------------------------
// Timer
// ---------------------------------------------------------------------------

procedure TForm1.TimerStatsTimer(Sender: TObject);
begin
  UpdateServerUI;
  UpdateClientUI;
end;

// ---------------------------------------------------------------------------
// Server component events
// ---------------------------------------------------------------------------

procedure TForm1.ServerClientConnected(Sender: TObject; ClientID: Integer);
begin
  Log(Format('Client #%d connected from %s', [ClientID, WebSocketServer.GetClientIP(ClientID)]));
  UpdateServerUI;
end;

procedure TForm1.ServerClientDisconnected(Sender: TObject; ClientID: Integer);
begin
  Log(Format('Client #%d disconnected', [ClientID]));
  UpdateServerUI;
end;

procedure TForm1.ServerHandleCommand(Sender: TObject; ClientID: Integer;
  const Command: TBytes);
var
  Msg: string;
begin
  Msg := TEncoding.UTF8.GetString(Command);
  Log(Format('Server received from Client #%d: %s', [ClientID, Msg]));
  WebSocketServer.SendCommandToClient(ClientID, TEncoding.UTF8.GetBytes('[Echo] ' + Msg));
end;

procedure TForm1.ServerError(Sender: TObject; const ErrorMsg: string);
begin
  Log('SERVER ERROR: ' + ErrorMsg);
end;

procedure TForm1.ServerStateChange(Sender: TObject;
  OldState, NewState: TWebSocketServerState; const StateDescription: string);
begin
  Log('Server state: ' + StateDescription);
  UpdateServerUI;
end;

// ---------------------------------------------------------------------------
// Client component events
// ---------------------------------------------------------------------------

procedure TForm1.ClientConnect(Sender: TObject);
begin
  Log('Client connected successfully');
  UpdateClientUI;
end;

procedure TForm1.ClientDisconnect(Sender: TObject);
begin
  Log('Client disconnected');
  UpdateClientUI;
end;

procedure TForm1.ClientHandleCommand(Sender: TObject; const Command: TBytes);
begin
  Log('Client received: ' + TEncoding.UTF8.GetString(Command));
  UpdateClientUI;
end;

procedure TForm1.ClientError(Sender: TObject; const ErrorMsg: string);
begin
  Log('CLIENT ERROR: ' + ErrorMsg);
  UpdateClientUI;
end;

procedure TForm1.ClientStateChange(Sender: TObject;
  OldState, NewState: TWebSocketConnectionState; const StateDescription: string);
begin
  Log('Client state: ' + StateDescription);
  UpdateClientUI;
end;

procedure TForm1.ClientReconnecting(Sender: TObject; AttemptNumber: Integer);
begin
  Log(Format('Client reconnecting... attempt #%d', [AttemptNumber]));
end;

procedure TForm1.WebSocketServerDataReceived(Sender: TObject;
  ClientID: Integer; const Data: TBytes);
begin
  log('Got message from client: ' + TEncoding.UTF8.GetString(Data));
end;

end.
