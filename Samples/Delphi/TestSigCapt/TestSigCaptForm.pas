(*************************************************************************
  TestSigCaptForm.pas
   
  Delphi Signature Capture

  Demonstrates Signature Capture using Delphi
  
  Copyright (c) 2015 Wacom GmbH. All rights reserved.
  
***************************************************************************)
unit TestSigCaptForm;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics,
  Controls, Forms, Dialogs, FLSIGCTLLib_TLB, StdCtrls,
  ExtCtrls,pngimage, Vcl.AppEvnts, Vcl.Menus, Data.DB, MemDS, VirtualTable,
  Vcl.Grids, Vcl.DBGrids,IOUtils;

type
  TForm1 = class(TForm)
    Image1: TImage;
    Memo1: TMemo;
    btnSign: TButton;
    Image2: TImage;
    Label1: TLabel;
    ApplicationEvents1: TApplicationEvents;
    TrayIcon1: TTrayIcon;
    PopupMenu1: TPopupMenu;
    Minimize: TMenuItem;
    Maximize: TMenuItem;
    Close1: TMenuItem;
    Timer1: TTimer;
    DBGrid1: TDBGrid;
    dsRequests: TDataSource;
    Button1: TButton;
    Button2: TButton;
    QRequests: TVirtualTable;
    QRequestsID: TLargeintField;
    QRequestsSANAD: TLargeintField;
    QRequestsUSER_SESSION: TStringField;
    QRequestsUSER_ID: TIntegerField;
    QRequestsDATETIME: TDateTimeField;
    QRequestsWHO: TWideStringField;
    QRequestsWHY: TWideStringField;
    QRequestsSIGNATURE_ID: TLargeintField;
    procedure ApplicationEvents1Minimize(Sender: TObject);
    procedure ApplicationEvents1Restore(Sender: TObject);
    procedure btnSignClick(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Close1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure MinimizeClick(Sender: TObject);
    procedure MaximizeClick(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure TrayIcon1DblClick(Sender: TObject);
  private
    { Private declarations }
    sigCtl: TSigCtl;

    procedure DoCaptureOld();
    procedure LoadNewRequest();
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation
   uses RESTRequest4D,RESTRequest4D.Request.Client,JsonDataObjects, Device.Licence,
  Device.Command;

{$R *.dfm}

procedure TForm1.ApplicationEvents1Minimize(Sender: TObject);
begin
  TrayIcon1.Visible:=True;
  if self.Visible then  self.Hide;
  Timer1.Enabled:=true;
end;

procedure TForm1.ApplicationEvents1Restore(Sender: TObject);
begin
  TrayIcon1.Visible:=False;
  if not self.Visible then  self.Show;
end;

procedure TForm1.btnSignClick(Sender: TObject);
begin
 //DoCaptureOld();

 if (not QRequests.Active) or QRequests.IsEmpty then
     LoadNewRequest;

 if not QRequests.IsEmpty then
 begin
    var r:=TSignatureUtils.Capture(QRequestsWHO.Value,QRequestsWHY.Value);
    Memo1.Lines.Add(CaptureResultStr(r));

    if r=crOk then
    begin
       var  LFileName:='temp\sign'+QRequestsID.AsString+'.png';
       TSignatureUtils.SaveToPNG(LFileName);

       Image2.Picture.LoadFromFile(LFileName);
    end;
 end;
end;

procedure TForm1.Button1Click(Sender: TObject);
begin
  LoadNewRequest();
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
 ShowMessage(QRequests.FieldByName('WHY').AsWideString);
end;

procedure TForm1.Close1Click(Sender: TObject);
begin
 self.Close;
end;

procedure TForm1.DoCaptureOld;
var
  res: CaptureResult;
  sigObj1: SigObj;
  fileName: String;
begin


  Memo1.Lines.Add('btnSign was pressed');
  if not Assigned(sigCtl) then
  begin
    sigCtl := TSigCtl.Create(Self);
    sigCtl.Licence:= WACOM_LICENCE;
  end;
  res := sigCtl.Capture('who : Mr Misam Abdollah', 'Why:Received 2000 USD');
  if res = CaptureOK then
  begin

    Memo1.Lines.Add('Signature captured successfully');
    var Flags:=RenderOutputFilename or RenderColor32BPP or RenderEncodeData;

    fileName := 'sig1.bmp';
    sigObj1 := SigObj(sigCtl.Signature);
    sigObj1.ExtraData['AdditionalData'] := 'Delphi test: Additional data';
    sigObj1.RenderBitmap(fileName, 200, 150, 'image/bmp', 0.5, $ff0000, $ffffff, -1.0, -1.0, Flags);
    Image1.Picture.LoadFromFile(fileName);

    //save png too.
    sigObj1.RenderBitmap('sig1.png', 200, 150, 'image/png', 0.5, $ff0000, $ffffff, 0, 0, Flags);
    Image2.Picture.LoadFromFile('sig1.png');  //uses pngimage

    //
    if self.WindowState=wsMinimized then //for test
       Timer1.Enabled:=true;
  end
  else
  begin
    Memo1.Lines.Add('Signature capture error res='+IntToStr(res));
    case res of
    CaptureCancel: begin Memo1.Lines.Add('Signature cancelled'); end;
    CaptureError: begin Memo1.Lines.Add('No capture service available'); end;
    CapturePadError: begin Memo1.Lines.Add('Signing device error'); end;
    else begin Memo1.Lines.Add('Unexpected error code'); end;
    end;
  end;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  Image1.Canvas.Create;
  Memo1.Clear;
  Caption:= IOUtils.TDirectory.GetCurrentDirectory;
  ForceDirectories(Caption+'\temp');
end;

procedure TForm1.LoadNewRequest;
var
  LResponse: IResponse;
begin

  var LUrl:='http://localhost:8080/rest/default/signature_request/new';
  LResponse := TRequest.New.BaseURL(LUrl).Get;

  Memo1.Lines.Text := LResponse.Content;
  QRequests.Open;
  QRequests.Clear;
  QRequests.DisableControls;
  try
      var j:=TJsonObject.ParseUtf8(LResponse.Content) as TJsonArray;
      try
        for var r in j do
        begin
          QRequests.Append;
          QRequestsID.Value:=r.L['ID'];
          QRequestsSANAD.Value:=r.L['SANAD'];
          QRequestsUSER_SESSION.Value:=r.S['USER_SESSION'];
          QRequestsWHO.Value:=r.S['WHO'];
          QRequestsWHY.Value:=r.S['WHY'];
          QRequestsDATETIME.Value:=r.D['DATETIME'];
          QRequestsUSER_ID.Value:=r.I['USER_ID'];
          QRequestsSIGNATURE_ID.Value:=r.L['SIGNATURE_ID'];
          QRequests.Post;
        end;
      finally
       j.Free;
      end;
  finally
    QRequests.EnableControls;
  end;


  //lblStatusCode.Caption := LResponse.StatusCode.ToString;
end;

procedure TForm1.MinimizeClick(Sender: TObject);
begin
  self.WindowState:=TWindowState.wsMinimized;
end;

procedure TForm1.MaximizeClick(Sender: TObject);
begin
  self.WindowState:=TWindowState.wsNormal;

end;

procedure TForm1.Timer1Timer(Sender: TObject);
begin
  TrayIcon1.ShowBalloonHint;

  Timer1.Enabled:=false;
  btnSign.Click;
  //MaximizeClick(nil);

end;

procedure TForm1.TrayIcon1DblClick(Sender: TObject);
begin
  if self.WindowState=wsMinimized then //for test
     self.WindowState:=TWindowState.wsMaximized
end;

end.
