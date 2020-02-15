unit ufmmain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  StdCtrls, Buttons, Spin, UPoints;

const
  CrossSize=3;
  CrossWidth=2;

type

  { TfmMain }

  TfmMain = class(TForm)
    bnUpdateImage: TButton;
    bnChangeImage: TBitBtn;
    bnSetXaxis: TButton;
    bnSaveToClipboard: TButton;
    bnClearAll: TButton;
    bnDeleteLastPoint: TButton;
    bnChangeScale: TButton;
    edEndX: TEdit;
    edStartY: TEdit;
    edAxisY2: TEdit;
    edEndY: TEdit;
    edDPI: TEdit;
    edStartX: TEdit;
    edXpoint: TEdit;
    edX: TEdit;
    edYpoint: TEdit;
    edY: TEdit;
    GroupBox1: TGroupBox;
    GroupBox2: TGroupBox;
    GroupBox3: TGroupBox;
    Image1: TImage;
    Label1: TLabel;
    Label10: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    OpenDlg: TOpenDialog;
    sbImage: TScrollBox;
    sedScale: TSpinEdit;
    UnitsGroup: TRadioGroup;
    procedure bnChangeImageClick(Sender: TObject);
    procedure bnChangeScaleClick(Sender: TObject);
    procedure bnClearAllClick(Sender: TObject);
    procedure bnDeleteLastPointClick(Sender: TObject);
    procedure bnSaveToClipboardClick(Sender: TObject);
    procedure bnSetXaxisClick(Sender: TObject);
    procedure bnUpdateImageClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure Image1Click(Sender: TObject);
    procedure Image1MouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure UnitsGroupClick(Sender: TObject);
  private
    fChangeAxis: integer;
    fEndX: integer;
    fEndY: integer;
    fIsPictureLoaded: boolean;
    fPointslist: TDigPointsList;
    CursorX, CursorY: integer;
    fScale: byte;
    fStartX: integer;
    fStartY: integer;
    OldCursor: TCursor;
    SaveEndX, SaveEndY: integer;
    fMyPicture: TPicture;
    function GetEndX: integer;
    function GetEndY: integer;
    function GetStartX: integer;
    function GetStartY: integer;
    procedure SetEndX(AValue: integer);
    procedure SetEndY(AValue: integer);
    procedure SetMyPicture(AValue: TPicture);
    procedure SetPointsList(AValue: TDigPointsList);
    procedure SetStartX(AValue: integer);
    procedure SetStartY(AValue: integer);
    procedure DrawCross(ix, iy: integer);
  protected
    procedure UpdateImage;
    procedure ChangePictureScale;

    property MyPicture: TPicture read FMyPicture write SetMyPicture;
    property Scale: byte read fScale;
  public
    { public declarations }
    property PointsList: TDigPointsList read fPointsList write SetPointsList;
    property ChangeAxis:integer read fChangeAxis write fChangeAxis;
    property IsPictureLoaded: boolean read fIsPictureLoaded;
    {Эти свойства хранят начало и конец оси Х. Причем соотвествующие поля (fXStart,
     fXend и т.п. хранятся в пикселах, соответствующих масшиабу 1. А свойства
     Xstart ... возвращают значения умножая на масштаб, а при присваивании записывают
     в поле разделив на масштаб. Это необходимо, чтобы при изменении масштаба ось
     Х не меняла своего положения}
    property StartX: integer read GetStartX write SetStartX;
    property EndX: integer read GetEndX write SetEndX;
    property StartY: integer read GetStartY write SetStartY;
    property EndY: integer read GetEndY write SetEndY;
  end;

var
  fmMain: TfmMain;

implementation
uses
  Clipbrd;

{$R *.lfm}

{ TfmMain }

procedure TfmMain.FormCreate(Sender: TObject);
begin
  inherited;
  PointsList:=TDigPointsList.Create;
  fIsPictureLoaded:=false;
  MyPicture := TPicture.Create;
end;

procedure TfmMain.FormDestroy(Sender: TObject);
begin
  PointsList:=nil;
  inherited;
end;

procedure TfmMain.bnChangeImageClick(Sender: TObject);
begin
  if OpenDlg.Execute then
  begin
    MyPicture := TPicture.Create;
    MyPicture.LoadFromFile(OpenDlg.FileName);
    Image1.Picture.Clear;
    fScale := 1; // Принудительно 1, иначе начальные значения поделит на Scale
    StartX:=0;
    EndX:=MyPicture.Width;
    StartY:=MyPicture.Height;
    EndY:=StartY;
    ChangePictureScale;
    fIsPictureLoaded:=true;
    UpdateImage;
    bnSetXaxis.Enabled:=true;
  end;
end;

procedure TfmMain.bnChangeScaleClick(Sender: TObject);
begin
  ChangePictureScale;
  UpdateImage;
end;

procedure TfmMain.ChangePictureScale;
begin
  fScale := sedScale.Value;
  Image1.Picture.Clear;
  Image1.Width:=MyPicture.Width*Scale;
  Image1.Height:=MyPicture.Height*Scale;
  case UnitsGroup.ItemIndex of
    0: PointsList.Scale:=1/Self.Scale;
    1: PointsList.Scale:=25.4/StrToInt(edDPI.text)/self.Scale;
  end;
  PointsList.Clear;
  edStartX.Text:=IntToStr(StartX);
  edStartY.Text:=IntToStr(StartY);
  edEndX.Text:=IntToStr(EndX);
  edEndY.Text:=IntToStr(EndY);
  PointsList.SetXaxis(StartX, StartY, EndX, EndY);
end;

procedure TfmMain.UpdateImage;
var
  i: integer;
begin
  if not IsPictureLoaded then exit;
  with Image1.Canvas do
  begin
    StretchDraw(Image1.DestRect,MyPicture.Graphic);
    Pen.Mode:=pmCopy;
    Pen.Color:=clBlue;
    MoveTo(StartX,StartY);
    LineTo(EndX,EndY);
    Pen.Color:=clRed;
  end;
  for i:=0 to PointsList.Count-1 do
  begin
    DrawCross(round(PointsList.Points[i].X),round(PointsList.Points[i].Y));
  end;
  Image1.Repaint;
end;

procedure TfmMain.bnClearAllClick(Sender: TObject);
begin
  PointsList.Clear;
  UpdateImage;
end;

procedure TfmMain.bnDeleteLastPointClick(Sender: TObject);
begin
  if PointsList.Count>0 then
  begin
    PointsList.Delete(PointsList.Count-1);
    UpdateImage;
  end;
end;

procedure TfmMain.bnSaveToClipboardClick(Sender: TObject);
var
  List: TStringList;
begin
  List:=PointsList.SaveToText;
  try
    Clipboard.AsText:=List.Text;
  finally
    List.Free;
  end;
end;

procedure TfmMain.bnSetXaxisClick(Sender: TObject);
begin
  OldCursor:=Image1.Cursor;
  ChangeAxis:=2;
  Image1.Cursor:=crHandPoint;
end;

procedure TfmMain.bnUpdateImageClick(Sender: TObject);
begin
  UpdateImage;
end;

procedure TfmMain.Image1Click(Sender: TObject);
begin
  if not IsPictureLoaded then exit;
  if ChangeAxis=2 then
  begin
    StartX := CursorX;
    StartY := CursorY;
    Image1.Canvas.MoveTo(CursorX,CursorY);
    SaveEndX:=CursorX;
    SaveEndY:=CursorY;
    Image1.Canvas.Pen.Color:=clBlue;
    Image1.Canvas.Pen.Mode:=pmNot;
    ChangeAxis:=1;
  end
  else if ChangeAxis=1 then
  begin
    EndX := CursorX;
    EndY := CursorY;
    PointsList.SetXaxis(StartX,StartY,EndX,EndY);
    Image1.Cursor:=OldCursor;
    Image1.Canvas.Pen.Color:=clRed;
    Image1.Canvas.Pen.Mode:=pmCopy;
    ChangeAxis:=0;
  end
  else
  begin
    PointsList.AddPoint(CursorX, CursorY);
    DrawCross(CursorX,CursorY);
  end;
end;


procedure TfmMain.Image1MouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
begin
  CursorX:=X;
  CursorY:=Y;
  PointsList.Current.Change(X,Y);
  edX.Text:=IntToStr(CursorX);
  edY.Text:=IntToStr(CursorY);
  if not IsPictureLoaded then exit;
  if ChangeAxis=1 then
  with Image1.Canvas do
    begin
      MoveTo(StartX,StartY);
      LineTo(SaveEndX,SaveEndY);
      MoveTo(StartX,StartY);
      LineTo(CursorX,CursorY);
      SaveEndX:=CursorX;
      SaveEndY:=CursorY;
      exit;
    end;
  edXPoint.Text:=FormatFloat('#0.00',PointsList.CurrentX);
  edYPoint.Text:=FormatFloat('#0.00',PointsList.CurrentY);
end;

procedure TfmMain.UnitsGroupClick(Sender: TObject);
begin
  ChangePictureScale;
  UpdateImage;
end;

procedure TfmMain.SetPointsList(AValue: TDigPointsList);
begin
  if fPointsList=AValue then Exit;
  PointsList.free;
  fPointsList:=AValue;
end;

procedure TfmMain.SetEndX(AValue: integer);
begin
  fEndX:=AValue div Scale;
  edEndX.Text:=IntToStr(EndX);
end;

function TfmMain.GetEndX: integer;
begin
  result := fEndX*Scale;
end;

procedure TfmMain.SetEndY(AValue: integer);
begin
  fEndY:=AValue div Scale;
  edEndY.Text:=IntToStr(EndY);
end;

function TfmMain.GetEndY: integer;
begin
  result := fEndY*Scale;
end;

procedure TfmMain.SetStartX(AValue: integer);
begin
  fStartX:=AValue div Scale;
  edStartX.Text:=IntToStr(StartX);
end;

function TfmMain.GetStartX: integer;
begin
  result := fStartX*Scale;
end;

procedure TfmMain.SetStartY(AValue: integer);
begin
  fStartY:=AValue div Scale;
  edStartY.Text:=IntToStr(StartY);
end;

function TfmMain.GetStartY: integer;
begin
  result := fStartY*Scale;
end;

procedure TfmMain.SetMyPicture(AValue: TPicture);
begin
  if FMyPicture=AValue then Exit;
  FreeAndNil(fMyPicture);
  FMyPicture:=AValue;
end;

procedure TfmMain.DrawCross(ix, iy: integer);
begin
  with Image1.Canvas do
  begin
    Pen.Mode:=pmCopy;
    Pen.Color:=clRed;
    Pen.Width:=CrossWidth;
    MoveTo(ix-CrossSize,iy);
    LineTo(ix+CrossSize,iy);
    MoveTo(ix,iy-CrossSize);
    LineTo(ix,iy+CrossSize);
  end;
  Image1.Repaint;
end;


end.

