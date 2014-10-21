unit uPlotDemo;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Controls, Graphics, ExtCtrls,
  uPlotClass, uPlotRect, uPlotTemplates, uPlotAxis, uPlotSeries, uPlotStyles, uPlotDataTypes,
  useriesmarkers;

// This file is used to facilitate demo plot creation
// Please check comments in TPLotDemo3D.MakePlot for information

type

  { TPlotDemo }

  TPlotDemo = class(TWinControl) // use lower class
  private
    FAlpha: Integer;
    FPLot: TPlot;
    function MakePlot: TPlot;virtual;
    procedure SetAlpha(AValue: Integer); virtual;
  protected
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure UpdateDemoData; virtual;

    property Plot: TPlot read FPLot;
    property Alpha: Integer read FAlpha write SetAlpha;
  end;

  { TPLotDemo2D }

  TPLotDemo2D = class(TPlotDemo)
  private
    function MakePlot: TPlot;override;
  protected
  public
    procedure UpdateDemoData; override;
  end;


  { TPLotDemo3D }

  TPLotDemo3D = class(TPlotDemo)
  private
    function MakePlot: TPlot;override;
  protected
  public
    procedure UpdateDemoData; override;
  end;

  { TPLotDemo2Dspectrum }

  TPLotDemo2Dspectrum = class(TPlotDemo)
  private
    FCounter: Integer;
    FTimer: TTimer;
    function MakePlot: TPlot; override;
    procedure SetAlpha(AValue: Integer); override;
  protected
    procedure _GenerateData(out AXYLine: TXYLine);
    procedure OnTimer(Sender: TObject); virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure UpdateDemoData; override;
  end;

  { TPLotDemo2Dwaterfall }

  TPLotDemo2Dwaterfall = class(TPLotDemo2Dspectrum)
  private
    function MakePlot: TPlot; override;
  protected
    procedure OnTimer(Sender: TObject); override;
  public
  end;

  { TPLotDemo3Dwaterfall }

  TPLotDemo3Dwaterfall = class(TPLotDemo2Dspectrum)
  private
    function MakePlot: TPlot; override;
  protected
    procedure OnTimer(Sender: TObject); override;
  public
  end;

implementation

{ TPLotDemo3Dwaterfall }  //####################################################

function TPLotDemo3Dwaterfall.MakePlot: TPlot;
var
  vBorderAbs, vBorderRel : TBorder;
  vIndex : integer;
  vAxisList : TList;
  vPoint: TPoint;
  //vViewRange: TValueRange;
  vCloneParams: TCloneAxisParams;
  vPlot: TPlot;
begin
vPoint.X := 0; vPoint.Y := 0;

   vPlot := TPlot.Create(self);
  //TmplPlotRect(plot border legend colorscale);
  // 1st PlotRect --------------------------------------------------------
   vBorderRel.Left := 0;
   vBorderRel.Right := 00;
   vBorderRel.Top := 00;
   vBorderRel.Bottom := 00;
   vBorderAbs.Left := 06;
   vBorderAbs.Right := 06;
   vBorderAbs.Top := 06;
   vBorderAbs.Bottom := 06;
     vIndex := TmplPlotRect(vPlot, vBorderAbs, vBorderRel, FALSE, FALSE);
     TPlotRect(vPlot.PlotRect[vIndex]).Title := 'Spectrum (FFT)';
     //TPlotRect(vPlot.PlotRect[vIndex]).ShowFrame:=true;
     TPlotRect(vPlot.PlotRect[vIndex]).Style.Color:=TColor($303030);
   // plot color
   vPlot.BackgroundColor:=TColor($303030); ;
     // 1st set of axes
   try
     vAxisList := TmplAxes(vPlot, TPlotRect(vPlot.PlotRect[vIndex]), 3); // 2 axes
     TPlotAxis(vPlot.Axis[0]).AxisLabel := 'Frequency';
     TPlotAxis(vPlot.Axis[0]).NumberFormat := nfEngineeringPrefix;

     TPlotAxis(vPlot.Axis[1]).AxisLabel := 'Amplitude';
     TPlotAxis(vPlot.Axis[1]).NumberFormat := nfEngineering;

     TPlotAxis(vPlot.Axis[2]).AxisLabel := 'Time';
     TPlotAxis(vPlot.Axis[2]).NumberFormat := nfEngineering;

     TPlotAxis(vPlot.Axis[0]).AddInnerGridAxis(2);  // axis 0 draws inner ticks relative to axis 1
     TPlotAxis(vPlot.Axis[0]).Set3DGridAxis(1);
     TPlotAxis(vPlot.Axis[2]).AddInnerGridAxis(0);  // and vice versa
     //
     //TPlotAxis(vPlot.Axis[0]).InnerSubTicks:=FALSE;  // dont clutter the axis with too much subtcks
     //TPlotAxis(vPlot.Axis[1]).InnerSubTicks:=TRUE; // and vice versa

     // ColorScale
     TPlotRect(vPlot.PlotRect[vIndex]).ShowColorScale:=true;
     TPlotRect(vPlot.PlotRect[vIndex]).ColorScaleRect.ScaleAxisIndex := 1;

     // 1st set of series FFTA
     vIndex := TmplSeries(vPlot, vAxisList, stWF3D);  // 2 dimensions, waterfall=FALSE

     TPlotSeries(vPlot.Series[vIndex]).AddUnit('x', '[Hz]');
     TPlotSeries(vPlot.Series[vIndex]).AddUnit('y', '[dB]');
     TPlotSeries(vPlot.Series[vIndex]).AddUnit('z', '[sec]');
     TPlotSeries(vPlot.Series[vIndex]).Caption := 'Spectrum';
     TPlotSeries(vPlot.Series[vIndex]).AutoScaleMode := asFit; //asFitNextMargined;
     TXYWF3DPlotSeries(vPlot.Series[vIndex]).TimePerLine:=1e-2;

     TXYWFPlotSeries(vPlot.Series[vIndex]).Interpolate := false;

     TXYWF3DPlotSeries(vPlot.Series[vIndex]).Style := TSeriesStylePoints.Create;
     TXYWF3DPlotSeries(vPlot.Series[vIndex]).LineWidth := 1;
     TSeriesStylePoints(vPlot.Series[vIndex].Style).Diameter:=3;

     vPoint.X := 0; vPoint.Y := 25;
     with TPlotAxis(vPlot.Axis[0]) do
     begin
       Visible := true;
       OriginMode:=omRel;
       DrawOriginRel := vPoint;
       //AutoMode := lmRelToBorder;
       AutoMode := lmRelToWidth;
       DrawAngle := -18;
       //DrawLength := 100;
       DrawLength := 60;
       TAxisStyle(Style).Color:=clGray;
       TAxisStyle(TickStyle).Color:=clGray;
       TAxisStyle(SubTickStyle).Color:=clGray;
     end;
     with TPlotAxis(vPlot.Axis[1]) do
     begin
       OriginMode:=omRel;
       Visible := true;
       DrawOriginRel := vPoint;
       AutoMode := lmRelToHeight;
       DrawAngle := 90;
       DrawLength := 40;
       TAxisStyle(Style).Color:=clGray;
       TAxisStyle(TickStyle).Color:=clGray;
       TAxisStyle(SubTickStyle).Color:=clGray;
     end;
     with TPlotAxis(vPlot.Axis[2]) do
     begin
       OriginMode:=omRel;
       Visible := false; //TRUE;
       DrawAngle := 45;
       AutoMode := lmRelToHeight; // border ?
       DrawOriginRel := vPoint;
       DrawLength := 84.8;
       TAxisStyle(Style).Color:=clGray;
       TAxisStyle(TickStyle).Color:=clGray;
       TAxisStyle(SubTickStyle).Color:=clGray;
       //AxisMode := amValuePerPixel;
     end;

     // set AutoPLaceFill
     TPlotRect(vPlot.PlotRect[0]).ShowFrame:=true;// debug only, otherwise not usefull except for rectangular axes
     TPlotRect(vPlot.PlotRect[0]).AxisAutoPlaceFill:=true;
     TPlotRect(vPlot.PlotRect[0]).AxisAutoPlaceFillConstraints[0]:=1; // relative X axis length
     TPlotRect(vPlot.PlotRect[0]).AxisAutoPlaceFillConstraints[1]:=0; // 0 means variable (need one variable axis for solving)
     TPlotRect(vPlot.PlotRect[0]).AxisAutoPlaceFillConstraints[2]:=1.4; // relative Z axis length (Z=2*X)


     vCloneParams.VisualParams := TPlotAxis(vPlot.Axis[2]).VisualParams;
     vCloneParams.ShiftAxis := vPlot.Axis[0] ; // typecast to TPLotAxis
     vCloneParams.ShiftLengthRel := 100;
     vCloneParams.VisualParams.TickAngle:=taNegative;
     vCloneParams.VisualParams.Visible:=true;
     TPlotAxis(vPlot.Axis[2]).AddCloneAxis(vCloneParams);


     //vPlot.Series[vIndex].Style := TSeriesStyleLines.Create;
     vPlot.Series[vIndex].Style := TSeriesStylePoints.Create;
     TSeriesStylePoints(vPlot.Series[vIndex].Style).Diameter := 1;
     //TSeriesStyleLines(vPlot.Series[vIndex].Style).Color := clBlack;
     //TSeriesStyleLines(vPlot.Series[vIndex].Style).LineWidth := 3;


   finally
     // so gehts aufräumen der Axis List
     IF vAxisList <> nil THEN  begin
       while vAxisList.Count > 0 do begin // vAxisList.Remove(vAxisList.Items[0]);
         Freemem(vAxisList.Items[0]);
         vAxisList.Delete(0);
       end;
       vAxisList.Free;
     end;
   end;

  Result := vPlot;
end;

procedure TPLotDemo3Dwaterfall.OnTimer(Sender: TObject);
var
  vXYLine: TXYLine;
begin
  FTimer.Enabled:=false;
  _GenerateData(vXYLine);
  TXYWF3DPlotSeries(Plot.Series[0]).AddLine(vXYLine);
  setlength(vXYLine, 0);
  FTimer.Enabled:=true;
end;

{ TPLotDemo2Dwaterfall }  //####################################################

function TPLotDemo2Dwaterfall.MakePlot: TPlot;
var
  vBorderAbs, vBorderRel : TBorder;
  vIndex : integer;
  vAxisList : TList;
  vPoint: TPoint;
  //vViewRange: TValueRange;
  //vCloneParams: TCloneAxisParams;
  vPlot: TPlot;
begin
vPoint.X := 0; vPoint.Y := 0;

   vPlot := TPlot.Create(self);
  //TmplPlotRect(plot border legend colorscale);
  // 1st PlotRect --------------------------------------------------------
   vBorderRel.Left := 0;
   vBorderRel.Right := 0;
   vBorderRel.Top := 0;
   vBorderRel.Bottom := 0;
   vBorderAbs.Left := 04;
   vBorderAbs.Right := 04;
   vBorderAbs.Top := 04;
   vBorderAbs.Bottom := 04;
     vIndex := TmplPlotRect(vPlot, vBorderAbs, vBorderRel, FALSE, FALSE);
     TPlotRect(vPlot.PlotRect[vIndex]).Title := 'Spectrum (FFT)';

     // 1st set of axes
   try
     vAxisList := TmplAxes(vPlot, TPlotRect(vPlot.PlotRect[vIndex]), 3); // 2 axes
     TPlotAxis(vPlot.Axis[0]).AxisLabel := 'Frequency';
     TPlotAxis(vPlot.Axis[0]).NumberFormat := nfEngineeringPrefix;

     TPlotAxis(vPlot.Axis[1]).AxisLabel := 'Amplitude';
     TPlotAxis(vPlot.Axis[1]).NumberFormat := nfEngineering;

     TPlotAxis(vPlot.Axis[2]).AxisLabel := 'Time';
     TPlotAxis(vPlot.Axis[2]).NumberFormat := nfEngineering;

     TPlotAxis(vPlot.Axis[0]).AddInnerGridAxis(2);  // axis 0 draws inner ticks relative to axis 2
     TPlotAxis(vPlot.Axis[2]).AddInnerGridAxis(0);  // and vice versa
     //
     //TPlotAxis(vPlot.Axis[0]).InnerSubTicks:=FALSE;  // dont clutter the axis with too much subtcks
     //TPlotAxis(vPlot.Axis[1]).InnerSubTicks:=TRUE; // and vice versa

     // ColorScale
     TPlotRect(vPlot.PlotRect[vIndex]).ShowColorScale:=true;
     TPlotRect(vPlot.PlotRect[vIndex]).ColorScaleRect.ScaleAxisIndex := 1;

     // 1st set of series FFTA
     vIndex := TmplSeries(vPlot, vAxisList, stWF2D);  // 2 dimensions, waterfall=FALSE

     TPlotSeries(vPlot.Series[vIndex]).AddUnit('x', '[Hz]');
     TPlotSeries(vPlot.Series[vIndex]).AddUnit('y', '[dB]');
     TPlotSeries(vPlot.Series[vIndex]).AddUnit('z', '[sec]');
     TPlotSeries(vPlot.Series[vIndex]).Caption := 'Spectrum';
     TPlotSeries(vPlot.Series[vIndex]).AutoScaleMode := asFit; //asFitNextMargined;
     TXYWFPlotSeries(vPlot.Series[vIndex]).TimePerLine:=1e-2;

     TXYWFPlotSeries(vPlot.Series[vIndex]).LineWidth := 1;
     TXYWFPlotSeries(vPlot.Series[vIndex]).Interpolate := true;

     TXYWFPlotSeries(vPlot.Series[vIndex]).Style := TSeriesStylePoints.Create;
     TSeriesStylePoints(vPlot.Series[vIndex].Style).Diameter:=1;

     vPoint.X := 0; vPoint.Y := 0;
     with TPlotAxis(vPlot.Axis[0]) do
     begin
       Visible := true;
       OriginMode:=omRel;
       DrawOriginRel := vPoint;
       DrawAngle := 0;
       DrawLength := 100;
     end;
     with TPlotAxis(vPlot.Axis[1]) do
     begin
       OriginMode:=omRel;
       Visible := false;
       DrawOriginRel := vPoint;
       DrawAngle := 90;
       DrawLength := 100;
     end;
     with TPlotAxis(vPlot.Axis[2]) do
     begin
        OriginMode:=omRel;
       Visible := TRUE;
       DrawAngle := 90;
       DrawOriginRel := vPoint;
       DrawLength := 100;
       //AxisMode := amValuePerPixel;
     end;


   finally
     // so gehts aufräumen der Axis List
     IF vAxisList <> nil THEN  begin
       while vAxisList.Count > 0 do begin // vAxisList.Remove(vAxisList.Items[0]);
         Freemem(vAxisList.Items[0]);
         vAxisList.Delete(0);
       end;
       vAxisList.Free;
     end;
   end;

  Result := vPlot;
end;

procedure TPLotDemo2Dwaterfall.OnTimer(Sender: TObject);
var
  vXYLine: TXYLine;
begin
  FTimer.Enabled:=false;
  _GenerateData(vXYLine);
  TXYWFPlotSeries(Plot.Series[0]).AddLine(vXYLine);
  setlength(vXYLine, 0);
  FTimer.Enabled:=true;
end;

{ TPLotDemo3D } //##############################################################

function TPLotDemo3D.MakePlot: TPlot;
var
  vBorderAbs, vBorderRel : TBorder;
  vIndex : integer;
  vAxisList : TList;
  vPoint: TPoint;
  vPlot: TPlot;
  vCloneParams: TCloneAxisParams;
begin
vPoint.X := 0; vPoint.Y := 0;

   // 1. Create a PLot
   vPlot := TPlot.Create(self);

  // 2. Create a PLotrect, first do some border settings
  // basically a PlotRect can be created with TPlotRect.Create(APlot)
  // see the Template below
  // --------------------------------------------------------
   vBorderRel.Left := 0;
   vBorderRel.Right := 0;
   vBorderRel.Top := 0;
   vBorderRel.Bottom := 0;
   vBorderAbs.Left := 04;
   vBorderAbs.Right := 04;
   vBorderAbs.Top := 04;
   vBorderAbs.Bottom := 04;
     vIndex := TmplPlotRect(vPlot, vBorderAbs, vBorderRel, FALSE, FALSE);
     TPlotRect(vPlot.PlotRect[vIndex]).Title := 'Simple 3D plot';

   // 3. create a set of axes, the template does this for us but you can create
   // manually 2 or 3 axes and set all required properties
   try
     vAxisList := TmplAxes(vPlot, TPlotRect(vPlot.PlotRect[vIndex]), 3); // 2 axes
     // you can Create axes manually with TPLotAxis.Create(APlot);
     TPlotAxis(vPlot.Axis[0]).AxisLabel := 'X axis label';                      // give it a label
     TPlotAxis(vPlot.Axis[0]).NumberFormat := nfEngineeringPrefix;              // how to format the numbers

     TPlotAxis(vPlot.Axis[1]).AxisLabel := 'Y label';
     TPlotAxis(vPlot.Axis[1]).NumberFormat := nfEngineering;

     TPlotAxis(vPlot.Axis[2]).AxisLabel := 'Z axis label';
     TPlotAxis(vPlot.Axis[2]).NumberFormat := nfFloat;

     // inner grid axes
     TPlotAxis(vPlot.Axis[0]).AddInnerGridAxis(1);  // axis 0 draws inner ticks relative to axis 1
     TPlotAxis(vPlot.Axis[1]).AddInnerGridAxis(0);  // and vice versa

     TPlotAxis(vPlot.Axis[0]).AddInnerGridAxis(2);  // axis 0 draws inner ticks relative to axis 2
     TPlotAxis(vPlot.Axis[2]).AddInnerGridAxis(0);  // and vice versa

     TPlotAxis(vPlot.Axis[1]).AddInnerGridAxis(2);  // axis 1 draws inner ticks relative to axis 2
     TPlotAxis(vPlot.Axis[2]).AddInnerGridAxis(1);  // and vice versa
     //
     //TPlotAxis(vPlot.Axis[0]).InnerSubTicks:=FALSE;  // dont clutter the axis with too much subticks
     //TPlotAxis(vPlot.Axis[1]).InnerSubTicks:=TRUE;   // and vice versa
     TPlotAxis(vPlot.Axis[2]).Set3DGridAxis(0);


     // below we modify some of the standard axis proeprties
     // to get a nicer looking plot
     // The simple 3D plot however is the least maintained plot as I dont need it at the moment
     // Expect most of the bugs here and most work to make it nice
     vPoint.X := 25; vPoint.Y := 75;
     with TPlotAxis(vPlot.Axis[0]) do
     begin
       Visible := false;
       OriginMode:=omRel;
       DrawOriginRel := vPoint;      // in percent of the plotrect thought in math coordinates
       //AutoMode := lmRelToBorder;
       AutoMode := lmRelToWidth;
       DrawAngle := -18;             // in degrees
       //DrawLength := 100;          // in percent
       DrawLength := 100;
       TickAngle:=taPositive;
       TAxisStyle(Style).Color:=clGray;
       TAxisStyle(TickStyle).Color:=clGray;
       TAxisStyle(SubTickStyle).Color:=clGray;
     end;
     with TPlotAxis(vPlot.Axis[1]) do
     begin
       OriginMode:=omRel;
       Visible := false; // true;
       DrawOriginRel := vPoint;
       AutoMode := lmRelToHeight;
       DrawAngle := 90;
       DrawLength := 100;
       TickAngle:=taPositive;
       TAxisStyle(Style).Color:=clGray;
       TAxisStyle(TickStyle).Color:=clGray;
       TAxisStyle(SubTickStyle).Color:=clGray;
     end;
     with TPlotAxis(vPlot.Axis[2]) do
     begin
       OriginMode:=omRel;
       Visible := false; //TRUE;
       DrawAngle := -135; //45;
       AutoMode := lmRelToBorder; // Height; // border ?
       DrawOriginRel := vPoint;
       DrawLength := 100; //84.8;
       TickAngle:=taNegative;
       TAxisStyle(Style).Color:=clGray;
       TAxisStyle(TickStyle).Color:=clGray;
       TAxisStyle(SubTickStyle).Color:=clGray;
     end;

     // set AutoPLaceFill
     TPlotRect(vPlot.PlotRect[0]).ShowFrame:=true;// debug only, otherwise not usefull except for rectangular axes
     TPlotRect(vPlot.PlotRect[0]).AxisAutoPlaceFill:=true;
     TPlotRect(vPlot.PlotRect[0]).AxisAutoPlaceFillConstraints[0]:=1; // relative X axis length
     TPlotRect(vPlot.PlotRect[0]).AxisAutoPlaceFillConstraints[1]:=0; // 0 means variable (need one variable axis for solving)
     TPlotRect(vPlot.PlotRect[0]).AxisAutoPlaceFillConstraints[2]:=2; // relative Z axis length (Z=2*X)



     // we want some cloned axes for display in fron of our plot
     // as we have the origin at the top left rear, the usual axes will be hidden by the data
     // Cloneaxes are not normally needed. Check these if you feel the plot should look nicer
     vCloneParams.VisualParams := TPlotAxis(vPlot.Axis[1]).VisualParams;
     vCloneParams.ShiftAxis := vPlot.Axis[0];
     vCloneParams.ShiftLengthRel := 100;
     vCloneParams.VisualParams.TickAngle:=taNegative;
     vCloneParams.VisualParams.Visible:=true;
     TPlotAxis(vPlot.Axis[1]).AddCloneAxis(vCloneParams);

     vCloneParams.VisualParams := TPlotAxis(vPlot.Axis[0]).VisualParams;
     vCloneParams.ShiftAxis := vPlot.Axis[2];
     vCloneParams.ShiftLengthRel := 100;
     vCloneParams.VisualParams.TickAngle:=taNegative;
     vCloneParams.VisualParams.Visible:=true;
     TPlotAxis(vPlot.Axis[0]).AddCloneAxis(vCloneParams);

     vCloneParams.VisualParams := TPlotAxis(vPlot.Axis[2]).VisualParams;
     vCloneParams.ShiftAxis := vPlot.Axis[0];
     vCloneParams.ShiftLengthRel := 100;
     vCloneParams.VisualParams.TickAngle:=taPositive;
     vCloneParams.VisualParams.Visible:=true;
     TPlotAxis(vPlot.Axis[2]).AddCloneAxis(vCloneParams);


     //4. Generate one or more series which hold and display out data
     vIndex := TmplSeries(vPlot, vAxisList, stXYZ);  // 2 dimensions, standard XY series
     // you can Create series manually with TPLotSeries.Create(APlot);

     TPlotSeries(vPlot.Series[vIndex]).AddUnit('x', '[unit]');
     TPlotSeries(vPlot.Series[vIndex]).AddUnit('y', '[unit]');
     TPlotSeries(vPlot.Series[vIndex]).AddUnit('z', '[unit]');
     TPlotSeries(vPlot.Series[vIndex]).Caption := 'MyData';
     TPlotSeries(vPlot.Series[vIndex]).AutoScaleMode := asFit; //asFitNextMargined; others available

     TPlotSeries(vPlot.Series[vIndex]).Style := TSeriesStylePoints.Create; // also line sytle available
     TSeriesStyleLines(vPlot.Series[vIndex].Style).Diameter:=7;
     //TSeriesStyleLines(vPlot.Series[vIndex].Style).LineWidth:=1; // only for linestyle

     // there is no interpolation for 3D series at present
     //TPlotSeries(vPlot.Series[vIndex]).Interpolate := false;   // Interpolate connects the datapoints (with lines)

   // this is needed only because of our axis template which generates a TList
   // if you Create axes manually, you wont need such
   finally
     IF vAxisList <> nil THEN  begin
       while vAxisList.Count > 0 do begin
         Freemem(vAxisList.Items[0]);
         vAxisList.Delete(0);
       end;
       vAxisList.Free;
     end;
   end;

  Result := vPlot;
  // remember: The vPlot we hand over now does contain a plotrect, axes and series
  // no need to remember all these elements separately
end;

procedure TPLotDemo3D.UpdateDemoData;
var
  vXLoop: Integer;
  vZLoop: Integer;
  vValue: Extended;
  vRange: TValueRange;
const
  cPoints = 40;
begin
  // generate some demo Data;
  Randomize;
  for vZLoop := -cPoints to cPoints do
    for vXLoop := -cPoints to cPoints do begin
      vValue := vXLoop * cos(Pi()/2/cPoints * vXLoop) * vZLoop * cos(Pi()/2/cPoints * vZLoop);
      TXYZPlotSeries(Plot.Series[0]).AddValue(vXLoop, vValue, vZLoop);
    end;
  vRange.min := -cPoints;
  vRange.max :=  cPoints;
  TPlotAxis(Plot.Axis[TPlotSeries(Plot.Series[0]).XAxis]).ViewRange := vRange;
  TPlotAxis(Plot.Axis[TXYZPlotSeries(Plot.Series[0]).ZAxis]).ViewRange := vRange;

  vRange.min := -(cPoints * cPoints) /8;
  vRange.max :=  (cPoints * cPoints) /8;
  TPlotAxis(Plot.Axis[TPlotSeries(Plot.Series[0]).YAxis]).ViewRange := vRange;
  TPlotSeries(Plot.Series[0]).ColoredAxis := TPlotSeries(Plot.Series[0]).YAxis;
  Plot.Repaint;
end;

{ TPLotDemo2Dspectrum } //##############################################################

function TPLotDemo2Dspectrum.MakePlot: TPlot;
var
  vBorderAbs, vBorderRel : TBorder;
  vIndex : integer;
  vAxisList : TList;
  //vPoint: TPoint;
  vPlot: TPlot;
begin
//vPoint.X := 0; vPoint.Y := 0;

   vPlot := TPlot.Create(self);
  //TmplPlotRect(plot border legend colorscale);
  // 1st PlotRect --------------------------------------------------------
   vBorderRel.Left := 0;
   vBorderRel.Right := 0;
   vBorderRel.Top := 0;
   vBorderRel.Bottom := 0;
   vBorderAbs.Left := 04;
   vBorderAbs.Right := 04;
   vBorderAbs.Top := 04;
   vBorderAbs.Bottom := 04;
     vIndex := TmplPlotRect(vPlot, vBorderAbs, vBorderRel, FALSE, FALSE);
     TPlotRect(vPlot.PlotRect[vIndex]).Title := '2D fast plot';

     // 1st set of axes
   try
     vAxisList := TmplAxes(vPlot, TPlotRect(vPlot.PlotRect[vIndex]), 2); // 2 axes
     TPlotAxis(vPlot.Axis[0]).AxisLabel := 'Time';
     TPlotAxis(vPlot.Axis[0]).NumberFormat := nfEngineeringPrefix;

     TPlotAxis(vPlot.Axis[1]).AxisLabel := 'Amplitude';
     TPlotAxis(vPlot.Axis[1]).NumberFormat := nfEngineering;

     TPlotAxis(vPlot.Axis[0]).AddInnerGridAxis(1);  // axis 0 draws inner ticks relative to axis 2
     TPlotAxis(vPlot.Axis[1]).AddInnerGridAxis(0);  // and vice versa
     //
     //TPlotAxis(vPlot.Axis[0]).InnerSubTicks:=FALSE;  // dont clutter the axis with too much subtcks
     //TPlotAxis(vPlot.Axis[1]).InnerSubTicks:=TRUE; // and vice versa

     // ColorScale
     TPlotRect(vPlot.PlotRect[vIndex]).ShowColorScale:=false;

     // 1st set of series FFTA
     vIndex := TmplSeries(vPlot, vAxisList, stSPECTRUM);  // 2 dimensions, waterfall=FALSE

     TPlotSeries(vPlot.Series[vIndex]).AddUnit('x', '[sec]');
     TPlotSeries(vPlot.Series[vIndex]).AddUnit('y', '[dB]');
     TPlotSeries(vPlot.Series[vIndex]).Caption := 'Scope';
     TPlotSeries(vPlot.Series[vIndex]).AutoScaleMode := asFit; //asFitNextMargined;

     TXYSpectrumPlotSeries(vPlot.Series[vIndex]).Style := TSeriesStylePoints.Create; //TSeriesStyleLines.Create;
     TSeriesStyleLines(vPlot.Series[vIndex].Style).Diameter:=3;
     //TSeriesStyleLines(vPlot.Series[vIndex].Style).LineWidth:=1;

     TXYWFPlotSeries(vPlot.Series[vIndex]).Interpolate := true;

   finally
     IF vAxisList <> nil THEN  begin
       while vAxisList.Count > 0 do begin // vAxisList.Remove(vAxisList.Items[0]);
         Freemem(vAxisList.Items[0]);
         vAxisList.Delete(0);
       end;
       vAxisList.Free;
     end;
   end;

  // add a peak marker just for demonstration
  TPlotSeries(vPlot.Series[0]).MarkerContainer.AddMarker;
  TPlotSeries(vPlot.Series[0]).MarkerContainer.Marker[0].Index:=0;
  TPlotSeries(vPlot.Series[0]).MarkerContainer.Marker[0].MarkerType := mtValue;
  TPlotSeries(vPlot.Series[0]).MarkerContainer.Marker[0].MarkerMode := mmMaxPeak;

  Result := vPlot;
end;

procedure TPLotDemo2Dspectrum.SetAlpha(AValue: Integer);
begin
  inherited SetAlpha(AValue);
  TPlotSeries(Plot.Series[0]).TransParency:=AValue*256;
end;

procedure TPLotDemo2Dspectrum._GenerateData(out AXYLine: TXYLine);
var
  vLoop: Integer;
  vAngle: Extended;
  vValue: Extended;
  vFactor: Extended;
const
  cLEN = 1024;
  cAMP = 100;
begin
  FCounter:= (FCounter+1) mod 360;
  vFactor := cAMP * sin(FCounter/360*2*Pi());
  setlength(AXYLine, cLEN);
  for vLoop := 0 to cLEN-1 do begin
    vAngle := (vLoop / cLEN) * 2 * Pi();
    vValue := vFactor * sin(vAngle);
    AXYLine[vLoop].X := vAngle;
    AXYLine[vLoop].Y := vValue;
  end;
end;

procedure TPLotDemo2Dspectrum.OnTimer(Sender: TObject);
var
  vXYLine: TXYLine;
begin
  FTimer.Enabled:=false;
  _GenerateData(vXYLine);
  TXYSpectrumPlotSeries(Plot.Series[0]).AddLine(vXYLine);
  setlength(vXYLine, 0);
  FTimer.Enabled:=true;
end;

constructor TPLotDemo2Dspectrum.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FTimer := TTimer.Create(Self);
  FTimer.Enabled := false;
  FTimer.OnTimer := @OnTimer;
  FCounter:=0;
end;

destructor TPLotDemo2Dspectrum.Destroy;
begin
  FTimer.Free;
  inherited Destroy;
end;

procedure TPLotDemo2Dspectrum.UpdateDemoData;
var
  vRange: TValueRange;
begin
  FTimer.Interval := 20;
  FTimer.Enabled:=true;
  vRange.min := 0;
  vRange.max := 7;
  TPlotAxis(Plot.Axis[TXYSpectrumPlotSeries(Plot.Series[0]).XAxis]).ViewRange := vRange;
  vRange.min := -100;
  vRange.max := 100;
  TPlotAxis(Plot.Axis[TXYSpectrumPlotSeries(Plot.Series[0]).YAxis]).ViewRange := vRange;
  Plot.Repaint;
end;

{ TPLotDemo2D } //##############################################################

function TPLotDemo2D.MakePlot: TPlot;
var
  vBorderAbs, vBorderRel : TBorder;
  vIndex : integer;
  vAxisList : TList;
  //vPoint: TPoint;
  vPlot: TPlot;
begin
//vPoint.X := 0; vPoint.Y := 0;

   vPlot := TPlot.Create(self);
  //TmplPlotRect(plot border legend colorscale);
  // 1st PlotRect --------------------------------------------------------
   vBorderRel.Left := 0;
   vBorderRel.Right := 0;
   vBorderRel.Top := 0;
   vBorderRel.Bottom := 0;
   vBorderAbs.Left := 04;
   vBorderAbs.Right := 04;
   vBorderAbs.Top := 04;
   vBorderAbs.Bottom := 04;
     vIndex := TmplPlotRect(vPlot, vBorderAbs, vBorderRel, FALSE, FALSE);
     TPlotRect(vPlot.PlotRect[vIndex]).Title := 'Simple 2D plot';

     // 1st set of axes
   try
     vAxisList := TmplAxes(vPlot, TPlotRect(vPlot.PlotRect[vIndex]), 2); // 2 axes
     // you can Create axes manually with TPLotAxis.Create(APlot);
     TPlotAxis(vPlot.Axis[0]).AxisLabel := 'X axis label';
     TPlotAxis(vPlot.Axis[0]).NumberFormat := nfEngineeringPrefix;

     TPlotAxis(vPlot.Axis[1]).AxisLabel := 'Y axis label';
     TPlotAxis(vPlot.Axis[1]).NumberFormat := nfEngineering;

     TPlotAxis(vPlot.Axis[0]).AddInnerGridAxis(1);  // axis 0 draws inner ticks relative to axis 2
     TPlotAxis(vPlot.Axis[1]).AddInnerGridAxis(0);  // and vice versa
     //
     //TPlotAxis(vPlot.Axis[0]).InnerSubTicks:=FALSE;  // dont clutter the axis with too much subtcks
     //TPlotAxis(vPlot.Axis[1]).InnerSubTicks:=TRUE; // and vice versa

     // 1st set of series
     vIndex := TmplSeries(vPlot, vAxisList, stXY);  // 2 dimensions, standard XY series
     // you can Create series manually with TPLotSeries.Create(APlot);

     TPlotSeries(vPlot.Series[vIndex]).AddUnit('x', '[unit]');
     TPlotSeries(vPlot.Series[vIndex]).AddUnit('y', '[unit]');
     TPlotSeries(vPlot.Series[vIndex]).Caption := 'MyData';
     TPlotSeries(vPlot.Series[vIndex]).AutoScaleMode := asFit; //asFitNextMargined;

     TPlotSeries(vPlot.Series[vIndex]).Style := TSeriesStylePoints.Create;
     TSeriesStyleLines(vPlot.Series[vIndex].Style).Diameter:=7;
     //TSeriesStyleLines(vPlot.Series[vIndex].Style).LineWidth:=1; // only for linestyle

     TXYPlotSeries(vPlot.Series[vIndex]).Interpolate := false;   // Interpolate connects the datapoints (with lines)

     // we manually create a second series (could use also the template again...)
     TXYPlotSeries.Create(vPlot);
     vIndex := vPlot.SeriesCount-1;
     with TXYPlotSeries(vPlot.Series[vIndex]) do begin
       XAxis := PInteger(vAxisList.Items[0])^;  // out new series need Xaxis and Yaxis
       YAxis := PInteger(vAxisList.Items[1])^;  // give to it the index of the respective axis
       Style := TSeriesStylePoints.Create;
       TSeriesStylePoints(Style).Diameter := 3;
       TSeriesStylePoints(Style).Color := clRed;
       Interpolate:=true;
       Caption := 'MyOtherData';
     end;

   finally
     IF vAxisList <> nil THEN  begin
       while vAxisList.Count > 0 do begin
         Freemem(vAxisList.Items[0]);
         vAxisList.Delete(0);
       end;
       vAxisList.Free;
     end;
   end;

  Result := vPlot;
end;

procedure TPLotDemo2D.UpdateDemoData;
var
  vXLoop: Integer;
  vValue: Extended;
begin
  // generate some demo Data;
  Randomize;
  for vXLoop := 1 to 40 do begin
    vValue := (Random(1001) - 500) / 100;
    TXYPlotSeries(Plot.Series[0]).AddValue(vXLoop, vValue);
  end;
  //Plot.AutoScalePlotRect(0);
  for vXLoop := 2 to 30 do begin
    vValue := (Random(1001) - 500) / 100;
    TXYPlotSeries(Plot.Series[1]).AddValue(vXLoop, vValue);
  end;
  Plot.AutoScalePlotRect(0);
  //Plot.Repaint;
end;

{ TPlotDemo }  //###############################################################

function TPlotDemo.MakePlot: TPlot;
begin
  Result := nil;
end;

procedure TPlotDemo.SetAlpha(AValue: Integer);
begin
  if FAlpha=AValue then Exit;
  FAlpha:=AValue;
end;

constructor TPlotDemo.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FPLot := MakePlot;
  FPLot.Parent := self;
  FPLot.Align := alClient;
  FPLot.Color := clBtnFace;
end;

destructor TPlotDemo.Destroy;
begin
  FPLot.Free;
  inherited Destroy;
end;

procedure TPlotDemo.UpdateDemoData;
begin

end;

end.

