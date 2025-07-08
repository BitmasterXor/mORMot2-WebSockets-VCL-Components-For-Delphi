unit Unit1;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs,
  Vcl.StdCtrls, Vcl.ExtCtrls, Vcl.ComCtrls, Vcl.Imaging.jpeg,
  Vcl.Imaging.pngimage, System.JSON, System.NetEncoding,
  mORMot2.WebSocket.Server, mORMot2.WebSocket.Client;

type
  // Message types for protocol
  TMessageType = (mtText, mtScreenshot, mtFile, mtCommand);

  // Message structure
  TWebSocketMessage = record
    MessageType: TMessageType;
    ClientID: Integer;
    Timestamp: TDateTime;
    Data: TBytes;
    Text: string;
    FileName: string;
  end;

  TForm1 = class(TForm)
    // Components
    Server: TmORMot2WebSocketServer;
    Client: TmORMot2WebSocketClient;

    // UI Layout
    PageControl1: TPageControl;
    TabServer: TTabSheet;
    TabClient: TTabSheet;
    TabSettings: TTabSheet;

    // Server Panel
    GroupBoxServer: TGroupBox;
    ButtonStartServer: TButton;
    ButtonStopServer: TButton;
    ButtonBroadcast: TButton;
    LabelServerStatus: TLabel;
    LabelClientCount: TLabel;
    MemoServerLog: TMemo;
    ListBoxClients: TListBox;
    ButtonSendToSelected: TButton;
    EditServerMessage: TEdit;

    // Client Panel
    GroupBoxClient: TGroupBox;
    ButtonConnect: TButton;
    ButtonDisconnect: TButton;
    ButtonSendText: TButton;
    ButtonSendScreenshot: TButton;
    EditClientMessage: TEdit;
    EditServerIP: TEdit;
    LabelClientStatus: TLabel;
    MemoClientLog: TMemo;
    ImageReceived: TImage;

    // Settings Panel
    GroupBoxEncryption: TGroupBox;
    CheckBoxServerEncryption: TCheckBox;
    CheckBoxClientEncryption: TCheckBox;
    ComboBoxServerKeySize: TComboBox;
    ComboBoxClientKeySize: TComboBox;
    ComboBoxServerMode: TComboBox;
    ComboBoxClientMode: TComboBox;
    EditServerKey: TEdit;
    EditClientKey: TEdit;
    LabelClientMode: TLabel;
    LabelClientKey: TLabel;

    // Statistics
    GroupBoxStats: TGroupBox;
    LabelTotalConnections: TLabel;
    LabelBytesReceived: TLabel;
    LabelBytesSent: TLabel;
    LabelMessagesReceived: TLabel;
    LabelMessagesSent: TLabel;
    TimerStats: TTimer;

    // Port settings
    EditServerPort: TEdit;
    EditClientPort: TEdit;
    LabelServerPort: TLabel;
    LabelClientPort: TLabel;
    CheckBox1: TCheckBox;
    LabelServerKey: TLabel;
    Label1: TLabel;

    // Event handlers
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);

    // Server events
    procedure ButtonStartServerClick(Sender: TObject);
    procedure ButtonStopServerClick(Sender: TObject);
    procedure ButtonBroadcastClick(Sender: TObject);
    procedure ButtonSendToSelectedClick(Sender: TObject);
    procedure ServerServerStateChange(Sender: TObject;
      OldState, NewState: TWebSocketServerState;
      const StateDescription: string);
    procedure ServerClientConnected(Sender: TObject; ClientID: Integer);
    procedure ServerClientDisconnected(Sender: TObject; ClientID: Integer);
    procedure ServerDataReceived(Sender: TObject; ClientID: Integer;
      const Data: TBytes);
    procedure ServerError(Sender: TObject; const ErrorMsg: string);

    // Client events
    procedure ButtonConnectClick(Sender: TObject);
    procedure ButtonDisconnectClick(Sender: TObject);
    procedure ButtonSendTextClick(Sender: TObject);
    procedure ButtonSendScreenshotClick(Sender: TObject);
    procedure ClientStateChange(Sender: TObject;
      OldState, NewState: TWebSocketConnectionState;
      const StateDescription: string);
    procedure ClientConnect(Sender: TObject);
    procedure ClientDisconnect(Sender: TObject);
    procedure ClientDataReceived(Sender: TObject; const Data: TBytes);
    procedure ClientError(Sender: TObject; const ErrorMsg: string);

    // Settings events
    procedure CheckBoxServerEncryptionClick(Sender: TObject);
    procedure CheckBoxClientEncryptionClick(Sender: TObject);
    procedure TimerStatsTimer(Sender: TObject);
    procedure CheckBox1Click(Sender: TObject);

  private
    // Screenshot functionality
    function CaptureScreenshot: TBytes;
    function BytesToBitmap(const Data: TBytes): TBitmap;

    // Message protocol
    function CreateMessage(MsgType: TMessageType; const Text: string = '';
      const Data: TBytes = nil; const FileName: string = ''): TBytes;
    function ParseMessage(const Data: TBytes): TWebSocketMessage;

    // UI helpers
    procedure LogServer(const Msg: string);
    procedure LogClient(const Msg: string);
    procedure UpdateServerStatus;
    procedure UpdateClientStatus;
    procedure UpdateClientList;
    procedure UpdateStatistics;
    procedure ApplyServerEncryption;
    procedure ApplyClientEncryption;

  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.FormCreate(Sender: TObject);
begin
  // Initialize UI
  PageControl1.ActivePageIndex := 0;

  // Server settings
  EditServerPort.Text := '8080';
  EditServerMessage.Text := 'Hello from server!';

  // Client settings
  EditServerIP.Text := 'localhost';
  EditClientPort.Text := '8080';
  EditClientMessage.Text := 'Hello from client!';
  client.AutoReconnect:=true;

  // Encryption settings
  EditServerKey.Text := 'MySecretKey123!';
  EditClientKey.Text := 'MySecretKey123!';

  // Key size options
  ComboBoxServerKeySize.Items.AddStrings(['128-bit', '192-bit', '256-bit']);
  ComboBoxClientKeySize.Items.AddStrings(['128-bit', '192-bit', '256-bit']);
  ComboBoxServerKeySize.ItemIndex := 2; // 256-bit default
  ComboBoxClientKeySize.ItemIndex := 2; // 256-bit default

  // Encryption mode options
  ComboBoxServerMode.Items.AddStrings(['CBC', 'CFB', 'CFC', 'CTC', 'CTR', 'ECB',
    'GCM', 'OFB', 'OFC']);
  ComboBoxClientMode.Items.AddStrings(['CBC', 'CFB', 'CFC', 'CTC', 'CTR', 'ECB',
    'GCM', 'OFB', 'OFC']);
  ComboBoxServerMode.ItemIndex := 0; // CBC default
  ComboBoxClientMode.ItemIndex := 0; // CBC default

  // Start statistics timer
  TimerStats.Interval := 1000;
  TimerStats.Enabled := True;

  // Initialize status
  UpdateServerStatus;
  UpdateClientStatus;

  LogServer('WebSocket Demo initialized');
  LogClient('Ready to connect');
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  if Server.Active then
    Server.Active := False;
  if Client.Connected then
    Client.Disconnect;
end;

// =============================================================================
// SERVER EVENTS
// =============================================================================

procedure TForm1.ButtonStartServerClick(Sender: TObject);
begin
  try
    Server.Port := StrToInt(EditServerPort.Text);
    ApplyServerEncryption;
    Server.Active := True;
    LogServer(Format('Starting server on port %d...', [Server.Port]));
  except
    on E: Exception do
      LogServer('Failed to start server: ' + E.Message);
  end;
end;

procedure TForm1.ButtonStopServerClick(Sender: TObject);
begin
  Server.Active := False;
  LogServer('Server stopped');
end;

procedure TForm1.ButtonBroadcastClick(Sender: TObject);
var
  Data: TBytes;
begin
  if not Server.Active then
  begin
    LogServer('Server not active');
    Exit;
  end;

  Data := CreateMessage(mtText, EditServerMessage.Text);
  Server.BroadcastCommand(Data);
  LogServer(Format('Broadcast sent to %d clients: %s', [Server.GetClientCount,
    EditServerMessage.Text]));
end;

procedure TForm1.ButtonSendToSelectedClick(Sender: TObject);
var
  Data: TBytes;
  ClientID: Integer;
begin
  if ListBoxClients.ItemIndex = -1 then
  begin
    LogServer('No client selected');
    Exit;
  end;

  ClientID := Integer(ListBoxClients.Items.Objects[ListBoxClients.ItemIndex]);
  Data := CreateMessage(mtText, EditServerMessage.Text);

  if Server.SendCommandToClient(ClientID, Data) then
    LogServer(Format('Message sent to client %d: %s',
      [ClientID, EditServerMessage.Text]))
  else
    LogServer(Format('Failed to send message to client %d', [ClientID]));
end;

procedure TForm1.ServerServerStateChange(Sender: TObject;
  OldState, NewState: TWebSocketServerState; const StateDescription: string);
begin
if Newstate = ssidle then
begin
   listboxclients.clear;//server is offline no need to show any connected clients as they are OFFLINE!
end;
  LogServer(Format('Server state: %s', [StateDescription]));
  UpdateServerStatus;
end;

procedure TForm1.ServerClientConnected(Sender: TObject; ClientID: Integer);
begin
  LogServer(Format('Client %d connected from %s',
    [ClientID, Server.GetClientIP(ClientID)]));
  UpdateClientList;
end;

procedure TForm1.ServerClientDisconnected(Sender: TObject; ClientID: Integer);
var
  I: Integer;
begin
  // Find and remove the specific client from the list
  for I := ListBoxClients.Items.Count - 1 downto 0 do
  begin
    if Integer(ListBoxClients.Items.Objects[I]) = ClientID then
    begin
      ListBoxClients.Items.Delete(I);
      Break;
    end;
  end;
  LogServer(Format('Client %d disconnected', [ClientID]));
end;

procedure TForm1.ServerDataReceived(Sender: TObject; ClientID: Integer;
  const Data: TBytes);
var
  Msg: TWebSocketMessage;
  ResponseData: TBytes;
begin
  try
    Msg := ParseMessage(Data);

    case Msg.MessageType of
      mtText:
        begin
          LogServer(Format('Text from client %d: %s', [ClientID, Msg.Text]));
          // Echo the message back
          ResponseData := CreateMessage(mtText, 'Echo: ' + Msg.Text);
          Server.SendCommandToClient(ClientID, ResponseData);
        end;

      mtScreenshot:
        begin
          LogServer(Format('Screenshot received from client %d (%d bytes)',
            [ClientID, Length(Msg.Data)]));
          // Forward screenshot to all OTHER clients (not back to sender)
          ResponseData := CreateMessage(mtScreenshot,
            Format('Screenshot from Client %d', [ClientID]), Msg.Data,
            'client_screenshot.jpg');
          // Broadcast to all clients except the sender
          Server.BroadcastCommand(ResponseData);
          LogServer('Screenshot forwarded to all clients');
        end;

      mtFile:
        LogServer(Format('File received from client %d: %s (%d bytes)',
          [ClientID, Msg.FileName, Length(Msg.Data)]));

      mtCommand:
        LogServer(Format('Command from client %d: %s', [ClientID, Msg.Text]));
    end;
  except
    on E: Exception do
      LogServer('Error parsing message from client ' + IntToStr(ClientID) + ': '
        + E.Message);
  end;
end;

procedure TForm1.ServerError(Sender: TObject; const ErrorMsg: string);
begin
  LogServer('Server Error: ' + ErrorMsg);
end;

// =============================================================================
// CLIENT EVENTS
// =============================================================================

procedure TForm1.ButtonConnectClick(Sender: TObject);
begin
  try
    Client.Host := EditServerIP.Text;
    Client.Port := StrToInt(EditClientPort.Text);
    Client.ReconnectStrategy := rsExponential;
    Client.MaxReconnectAttempts := 10;

    ApplyClientEncryption;
    Client.Connect;
    LogClient(Format('Connecting to %s:%s...',
      [Client.Host, EditClientPort.Text]));
  except
    on E: Exception do
      LogClient('Failed to connect: ' + E.Message);
  end;
end;

procedure TForm1.ButtonDisconnectClick(Sender: TObject);
begin
  Client.Disconnect;
  LogClient('Disconnecting...');
end;

procedure TForm1.ButtonSendTextClick(Sender: TObject);
var
  Data: TBytes;
begin
  if not Client.Connected then
  begin
    LogClient('Not connected to server');
    Exit;
  end;

  Data := CreateMessage(mtText, EditClientMessage.Text);
  if Client.SendCommand(Data) then
    LogClient('Text sent: ' + EditClientMessage.Text)
  else
    LogClient('Failed to send text message');
end;

procedure TForm1.ButtonSendScreenshotClick(Sender: TObject);
var
  ScreenData: TBytes;
  Data: TBytes;
begin
  if not Client.Connected then
  begin
    LogClient('Not connected to server');
    Exit;
  end;

  try
    LogClient('Capturing screenshot...');
    ScreenData := CaptureScreenshot;
    Data := CreateMessage(mtScreenshot, '', ScreenData, 'screenshot.jpg');

    if Client.SendCommand(Data) then
      LogClient(Format('Screenshot sent (%d bytes)', [Length(ScreenData)]))
    else
      LogClient('Failed to send screenshot');
  except
    on E: Exception do
      LogClient('Screenshot error: ' + E.Message);
  end;
end;

procedure TForm1.ClientStateChange(Sender: TObject;
  OldState, NewState: TWebSocketConnectionState;
  const StateDescription: string);
begin
  LogClient(Format('Client state: %s', [StateDescription]));
  UpdateClientStatus;
end;

procedure TForm1.ClientConnect(Sender: TObject);
begin
  LogClient(Format('Connected to server at %s', [Client.GetServerIP]));
  UpdateClientStatus;
end;

procedure TForm1.ClientDisconnect(Sender: TObject);
begin
  LogClient('Disconnected from server');
  UpdateClientStatus;
end;

procedure TForm1.ClientDataReceived(Sender: TObject; const Data: TBytes);
var
  Msg: TWebSocketMessage;
  Bitmap: TBitmap;
begin
  try
    Msg := ParseMessage(Data);

    case Msg.MessageType of
      mtText:
        LogClient('Server says: ' + Msg.Text);

      mtScreenshot:
        begin
          LogClient(Format('Screenshot received from server (%d bytes)',
            [Length(Msg.Data)]));
          try
            Bitmap := BytesToBitmap(Msg.Data);
            try
              // Clear any previous image
              ImageReceived.Picture := nil;
              // Assign the new screenshot
              ImageReceived.Picture.Assign(Bitmap);
              LogClient('Screenshot displayed successfully');
            finally
              Bitmap.Free;
            end;
          except
            on E: Exception do
              LogClient('Error displaying screenshot: ' + E.Message);
          end;
        end;

      mtFile:
        LogClient(Format('File received: %s (%d bytes)',
          [Msg.FileName, Length(Msg.Data)]));

      mtCommand:
        LogClient('Server command: ' + Msg.Text);
    end;
  except
    on E: Exception do
      LogClient('Error parsing server message: ' + E.Message);
  end;
end;

procedure TForm1.ClientError(Sender: TObject; const ErrorMsg: string);
begin
  LogClient('Client Error: ' + ErrorMsg);
end;

// =============================================================================
// SETTINGS EVENTS
// =============================================================================

procedure TForm1.CheckBoxServerEncryptionClick(Sender: TObject);
begin
  ApplyServerEncryption;
end;

procedure TForm1.CheckBox1Click(Sender: TObject);
begin
  if CheckBox1.Checked = True then
  begin
    self.Client.AutoReconnect := True;
  end                                       //For those of you who want less code you could just tie the boolean directly to the checkbox checked boolean value ;)
  else
  begin
    self.Client.AutoReconnect := False;
  end;
end;

procedure TForm1.CheckBoxClientEncryptionClick(Sender: TObject);
begin
  ApplyClientEncryption;
end;

procedure TForm1.TimerStatsTimer(Sender: TObject);
begin
  UpdateStatistics;
end;

// =============================================================================
// PRIVATE METHODS
// =============================================================================

function TForm1.CaptureScreenshot: TBytes;
var
  Bitmap: TBitmap;
  JpegImage: TJPEGImage;
  MemStream: TMemoryStream;
  DC: HDC;
begin
  Bitmap := TBitmap.Create;
  JpegImage := TJPEGImage.Create;
  MemStream := TMemoryStream.Create;
  try
    // Capture screen
    DC := GetDC(0);
    try
      Bitmap.Width := GetSystemMetrics(SM_CXSCREEN);
      Bitmap.Height := GetSystemMetrics(SM_CYSCREEN);
      BitBlt(Bitmap.Canvas.Handle, 0, 0, Bitmap.Width, Bitmap.Height, DC, 0,
        0, SRCCOPY);
    finally
      ReleaseDC(0, DC);
    end;

    // Convert to JPEG for compression
    JpegImage.Assign(Bitmap);
    JpegImage.CompressionQuality := 70;
    JpegImage.SaveToStream(MemStream);

    // Convert to TBytes
    SetLength(Result, MemStream.Size);
    MemStream.Position := 0;
    MemStream.ReadBuffer(Result[0], MemStream.Size);
  finally
    Bitmap.Free;
    JpegImage.Free;
    MemStream.Free;
  end;
end;

function TForm1.BytesToBitmap(const Data: TBytes): TBitmap;
var
  MemStream: TMemoryStream;
  JpegImage: TJPEGImage;
begin
  Result := TBitmap.Create;
  MemStream := TMemoryStream.Create;
  JpegImage := TJPEGImage.Create;
  try
    if Length(Data) = 0 then
    begin
      LogClient('Error: Empty image data received');
      Exit;
    end;

    MemStream.WriteBuffer(Data[0], Length(Data));
    MemStream.Position := 0;

    try
      JpegImage.LoadFromStream(MemStream);
      Result.Assign(JpegImage);
    except
      on E: Exception do
      begin
        LogClient('Error loading JPEG image: ' + E.Message);
        // Create a simple error bitmap
        Result.Width := 200;
        Result.Height := 100;
        Result.Canvas.Brush.Color := clRed;
        Result.Canvas.FillRect(Result.Canvas.ClipRect);
        Result.Canvas.Font.Color := clWhite;
        Result.Canvas.TextOut(10, 10, 'Image Error');
      end;
    end;
  finally
    MemStream.Free;
    JpegImage.Free;
  end;
end;

function TForm1.CreateMessage(MsgType: TMessageType; const Text: string = '';
  const Data: TBytes = nil; const FileName: string = ''): TBytes;
var
  JSON: TJSONObject;
  JSONStr: string;
  DataBase64: string;
begin
  JSON := TJSONObject.Create;
  try
    JSON.AddPair('type', TJSONNumber.Create(Ord(MsgType)));
    JSON.AddPair('timestamp', TJSONString.Create(DateTimeToStr(Now)));
    JSON.AddPair('text', TJSONString.Create(Text));
    JSON.AddPair('filename', TJSONString.Create(FileName));

    if Data <> nil then
    begin
      DataBase64 := TNetEncoding.Base64.EncodeBytesToString(Data);
      JSON.AddPair('data', TJSONString.Create(DataBase64));
    end
    else
      JSON.AddPair('data', TJSONString.Create(''));

    JSONStr := JSON.ToString;
    Result := TEncoding.UTF8.GetBytes(JSONStr);
  finally
    JSON.Free;
  end;
end;

function TForm1.ParseMessage(const Data: TBytes): TWebSocketMessage;
var
  JSONStr: string;
  JSON: TJSONObject;
  DataBase64: string;
begin
  JSONStr := TEncoding.UTF8.GetString(Data);
  JSON := TJSONObject.ParseJSONValue(JSONStr) as TJSONObject;
  try
    Result.MessageType := TMessageType(JSON.GetValue<Integer>('type'));
    Result.Text := JSON.GetValue<string>('text');
    Result.FileName := JSON.GetValue<string>('filename');

    DataBase64 := JSON.GetValue<string>('data');
    if DataBase64 <> '' then
      Result.Data := TNetEncoding.Base64.DecodeStringToBytes(DataBase64)
    else
      Result.Data := nil;
  finally
    JSON.Free;
  end;
end;

procedure TForm1.LogServer(const Msg: string);
begin
  MemoServerLog.Lines.Add(Format('[%s] %s', [TimeToStr(Now), Msg]));
  MemoServerLog.SelStart := Length(MemoServerLog.Text);
  MemoServerLog.SelLength := 0;
  SendMessage(MemoServerLog.Handle, EM_SCROLLCARET, 0, 0);
end;

procedure TForm1.LogClient(const Msg: string);
begin
  MemoClientLog.Lines.Add(Format('[%s] %s', [TimeToStr(Now), Msg]));
  MemoClientLog.SelStart := Length(MemoClientLog.Text);
  MemoClientLog.SelLength := 0;
  SendMessage(MemoClientLog.Handle, EM_SCROLLCARET, 0, 0);
end;

procedure TForm1.UpdateServerStatus;
begin
  if Server.Active then
  begin
    LabelServerStatus.Caption := Format('Status: Listening on port %d',
      [Server.Port]);
    LabelServerStatus.Font.Color := clGreen;
    ButtonStartServer.Enabled := False;
    ButtonStopServer.Enabled := True;
  end
  else
  begin
    LabelServerStatus.Caption := 'Status: Stopped';
    LabelServerStatus.Font.Color := clRed;
    ButtonStartServer.Enabled := True;
    ButtonStopServer.Enabled := False;
  end;

  LabelClientCount.Caption := Format('Connected Clients: %d',
    [Server.GetClientCount]);
end;

procedure TForm1.UpdateClientStatus;
begin
  if Client.Connected then
  begin
    LabelClientStatus.Caption := Format('Status: Connected to %s:%d',
      [Client.Host, Client.Port]);
    LabelClientStatus.Font.Color := clGreen;
    ButtonConnect.Enabled := False;
    ButtonDisconnect.Enabled := True;
    ButtonSendText.Enabled := True;
    ButtonSendScreenshot.Enabled := True;
  end
  else
  begin
    LabelClientStatus.Caption := 'Status: Disconnected';
    LabelClientStatus.Font.Color := clRed;
    ButtonConnect.Enabled := True;
    ButtonDisconnect.Enabled := False;
    ButtonSendText.Enabled := False;
    ButtonSendScreenshot.Enabled := False;
  end;
end;

procedure TForm1.UpdateClientList;
var
  I: Integer;
begin
  ListBoxClients.Clear;
  for I := 1 to Server.GetClientCount do
  begin
    ListBoxClients.Items.AddObject(Format('Client %d (127.0.0.1)', [I]), TObject(I));
  end;
end;

procedure TForm1.UpdateStatistics;
begin
  // Server stats
  LabelTotalConnections.Caption := Format('Total Connections Made: %d',
    [Server.TotalConnections]);
  LabelBytesReceived.Caption := Format('Bytes Received: %d',
    [Server.TotalBytesReceived]);
  LabelBytesSent.Caption := Format('Bytes Sent: %d', [Server.TotalBytesSent]);
  LabelMessagesReceived.Caption := Format('Messages Received: %d',
    [Server.TotalMessagesReceived]);
  LabelMessagesSent.Caption := Format('Messages Sent: %d',
    [Server.TotalMessagesSent]);
end;

procedure TForm1.ApplyServerEncryption;
begin
  Server.EncryptionEnabled := CheckBoxServerEncryption.Checked;

  if CheckBoxServerEncryption.Checked then
  begin
    Server.EncryptionKey := EditServerKey.Text;

    // Set key size with fully qualified names
    case ComboBoxServerKeySize.ItemIndex of
      0:
        Server.EncryptionKeySize :=
          mORMot2.WebSocket.Server.TAESKeySize(aks128);
      1:
        Server.EncryptionKeySize :=
          mORMot2.WebSocket.Server.TAESKeySize(aks192);
      2:
        Server.EncryptionKeySize :=
          mORMot2.WebSocket.Server.TAESKeySize(aks256);
    else
      Server.EncryptionKeySize := mORMot2.WebSocket.Server.TAESKeySize(aks256);
    end;

    // Set encryption mode with fully qualified names
    case ComboBoxServerMode.ItemIndex of
      0:
        Server.EncryptionMode := mORMot2.WebSocket.Server.TAESMode(amCBC);
      1:
        Server.EncryptionMode := mORMot2.WebSocket.Server.TAESMode(amCFB);
      2:
        Server.EncryptionMode := mORMot2.WebSocket.Server.TAESMode(amCFC);
      3:
        Server.EncryptionMode := mORMot2.WebSocket.Server.TAESMode(amCTC);
      4:
        Server.EncryptionMode := mORMot2.WebSocket.Server.TAESMode(amCTR);
      5:
        Server.EncryptionMode := mORMot2.WebSocket.Server.TAESMode(amECB);
      6:
        Server.EncryptionMode := mORMot2.WebSocket.Server.TAESMode(amGCM);
      7:
        Server.EncryptionMode := mORMot2.WebSocket.Server.TAESMode(amOFB);
      8:
        Server.EncryptionMode := mORMot2.WebSocket.Server.TAESMode(amOFC);
    else
      Server.EncryptionMode := mORMot2.WebSocket.Server.TAESMode(amCBC);
    end;
  end;
end;

procedure TForm1.ApplyClientEncryption;
begin
  Client.EncryptionEnabled := CheckBoxClientEncryption.Checked;

  if CheckBoxClientEncryption.Checked then
  begin
    Client.EncryptionKey := EditClientKey.Text;

    // Set key size with fully qualified names
    case ComboBoxClientKeySize.ItemIndex of
      0:
        Client.EncryptionKeySize :=
          mORMot2.WebSocket.Client.TAESKeySize(aks128);
      1:
        Client.EncryptionKeySize :=
          mORMot2.WebSocket.Client.TAESKeySize(aks192);
      2:
        Client.EncryptionKeySize :=
          mORMot2.WebSocket.Client.TAESKeySize(aks256);
    else
      Client.EncryptionKeySize := mORMot2.WebSocket.Client.TAESKeySize(aks256);
    end;

    // Set encryption mode with fully qualified names
    case ComboBoxClientMode.ItemIndex of
      0:
        Client.EncryptionMode := mORMot2.WebSocket.Client.TAESMode(amCBC);
      1:
        Client.EncryptionMode := mORMot2.WebSocket.Client.TAESMode(amCFB);
      2:
        Client.EncryptionMode := mORMot2.WebSocket.Client.TAESMode(amCFC);
      3:
        Client.EncryptionMode := mORMot2.WebSocket.Client.TAESMode(amCTC);
      4:
        Client.EncryptionMode := mORMot2.WebSocket.Client.TAESMode(amCTR);
      5:
        Client.EncryptionMode := mORMot2.WebSocket.Client.TAESMode(amECB);
      6:
        Client.EncryptionMode := mORMot2.WebSocket.Client.TAESMode(amGCM);
      7:
        Client.EncryptionMode := mORMot2.WebSocket.Client.TAESMode(amOFB);
      8:
        Client.EncryptionMode := mORMot2.WebSocket.Client.TAESMode(amOFC);
    else
      Client.EncryptionMode := mORMot2.WebSocket.Client.TAESMode(amCBC);
    end;
  end;
end;

end.
