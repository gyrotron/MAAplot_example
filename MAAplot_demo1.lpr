program MAAplot_demo1;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, uMain, uPlotDemo, uPlotClass, uPlotTemplates,
  uPlotSeries, uPlotOverrides, uPlotInterpolator, uMarkerDrawer, useriesmarkers,
  uPlotRect, uPlot_ResultWriter, uPlotHIDHandler, uPlotAxis, uPlotUtils,
  uPlotStyles, uPlotForms
  { you can add units after this };

{$R *.res}

begin
  RequireDerivedFormResource := True;
  Application.Initialize;
  Application.CreateForm(Tfrm_main, frm_main);
  Application.Run;
end.

