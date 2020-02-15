unit UPoints;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Contnrs;

type

  { TDigPoint }

  TDigPoint=class
  private
    fX: double;
    fY: double;
  public
    constructor Create( ix, iy: integer);
    procedure   Change(ix, iy: integer);

    property X: double read fX;
    property Y: double read fY;
  end;

  { TDigPointsList }

  TDigPointsList=class(TObjectList)
  private
    fCurrent: TDigPoint;
    fScale: double;
    StartX, StartY, EndX, EndY: double;
    CosFI, SinFI: double;
    function GetCurrentX: double;
    function GetCurrentY: double;
    function CalcX(Point: TDigPoint):double;
    function CalcY(Point: TDigPoint):double;
    function GetPoints(i: integer): TDigPoint;
    function GetX(i: integer): double;
    function GetY(i: integer): double;
    procedure SetCurrent(AValue: TDigPoint);
  public

    constructor Create;
    destructor Destroy; override;

    procedure AddPoint(ix, iy: integer);
    procedure SetXaxis( SX, SY, EX, EY: integer);
    function SaveToText:TStringList;

    property Current: TDigPoint read fCurrent write SetCurrent;
    property CurrentX: double read GetCurrentX;
    property CurrentY: double read GetCurrentY;
    property Points[i: integer]: TDigPoint read GetPoints;
    property X[i:integer]: double read GetX;
    property Y[i:integer]: double read GetY;
    property Scale:double read fScale write fScale;
  end;

implementation

{ TDigPointsList }

constructor TDigPointsList.Create;
begin
  inherited Create(true);
  Current:=TDigPoint.Create(0,0);
  Scale:=1;
end;

destructor TDigPointsList.Destroy;
begin
  Current.free;
  inherited Destroy;
end;

function TDigPointsList.GetCurrentX: double;
begin
  result:=CalcX(Current);
end;

function TDigPointsList.GetCurrentY: double;
begin
  result:=CalcY(Current);
end;

function TDigPointsList.CalcX(Point: TDigPoint): double;
var
  DX, DY: double;
begin
  DX := Point.X-StartX;
  DY := StartY-Point.Y;
  result:=(DX*CosFI+DY*sinFi)*Scale;
end;

function TDigPointsList.CalcY(Point: TDigPoint): double;
var
  DX, DY: double;
begin
  DX := Point.X-StartX;
  DY := StartY-Point.Y;
  result:=(-DX*SinFI+DY*CosFi)*Scale;
end;

function TDigPointsList.GetPoints(i: integer): TDigPoint;
begin
  result:=Items[i] as TDigPoint;
end;

function TDigPointsList.GetX(i: integer): double;
begin
  result:=CalcX(Points[i]);
end;

function TDigPointsList.GetY(i: integer): double;
begin
  result:=CalcY(Points[i]);
end;

procedure TDigPointsList.SetCurrent(AValue: TDigPoint);
begin
  if fCurrent=AValue then Exit;
  Current.free;
  fCurrent:=AValue;
end;

procedure TDigPointsList.AddPoint(ix, iy: integer);
begin
  Add(TDigPoint.Create(ix, iy));
end;

procedure TDigPointsList.SetXaxis(SX, SY, EX, EY: integer);
var
  L: double;
begin
  StartX:=SX;
  StartY:=SY;
  EndX:=EX;
  EndY:=EY;
  L:=sqrt(sqr(EX-SX)+sqr(EY-SY));
  CosFI:=(EX-SX)/L;
  SinFI:=(SY-EY)/L;    // на картинке ось Y идет вниз, а нам надо вверх
end;

function TDigPointsList.SaveToText: TStringList;
var
  S: TStringList;
  i: integer;
begin
  result:=nil;
  if Count=0 then exit;
  S:=TStringList.Create;
  for i:=0 to count-1 do
  begin
    S.Add(FormatFloat('0.00',X[i])+' , '+FormatFloat('0.00',Y[i]));
  end;
  result:=S;
end;

{ TDigPoint }

constructor TDigPoint.Create(ix, iy: integer);
begin
  fX:=ix;
  fY:=iy;
end;

procedure TDigPoint.Change(ix, iy: integer);
begin
  fX:=ix;
  fY:=iy;
end;

end.

