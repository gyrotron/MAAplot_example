unit uMain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  Buttons, Spin, uPlotDemo;

type

  { Tfrm_main }

  Tfrm_main = class(TForm)
    cb_demotype: TComboBox;
    gb_control: TGroupBox;
    gb_plot: TGroupBox;
    gb_options: TGroupBox;
    lbl_alpha: TLabel;
    lbl_demotype: TLabel;
    btn_dodemo: TSpeedButton;
    mem_info: TMemo;
    FPLotDemo: TPlotDemo;
    ed_alpha: TSpinEdit;
    procedure btn_dodemoClick(Sender: TObject);
    procedure cb_demotypeChange(Sender: TObject);
    procedure ed_alphaChange(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
  private
    { private declarations }
    procedure UpdateMemo(AIndex: Integer);
  public
    { public declarations }
  end;

var
  frm_main: Tfrm_main;

implementation

{$R *.lfm}

{ Tfrm_main }

procedure Tfrm_main.FormCreate(Sender: TObject);
begin
  cb_demotype.Text := 'please choose';
  Self.Caption := 'MAAplot demo';

  with mem_info do begin
    Clear;
    Lines.Add('MAAplot is a plotting class for Lazarus/fpc');
    Lines.Add('Its intended use is to display X/Y measurement data');
    Lines.Add('MAAplot evolved from a fpc learning project to a usable component');
    Lines.Add('Measured data can be viewed, exported and imported.');
    Lines.Add('Many proerties of the plot can be customized by built-in context menu.');
    Lines.Add('MAAplot is NOT a function plotter (although it might be used for that)');
  end;
end;

procedure Tfrm_main.UpdateMemo(AIndex: Integer);
begin
  CASE AIndex OF
    0:  with mem_info do begin
          Clear;
          Lines.Add('Above you see a nice 2D plot area with 2 axes and after pressing start also 2 Series.');
          Lines.Add('The area covered by the grid is denoted a >plotrect<.');
          Lines.Add('The data is oragnised in >series< each holding measurement data.');
          Lines.Add('Series can have different appearance like points or circles (use the menu).');
          Lines.Add('Series can be drawn with the datapoints connected (interpolated), see the red one.');
          Lines.Add('Series can be cleared by code or via menu.');
          Lines.Add('Data is added to 2D series via method  AddValue(X,Y)');
          Lines.Add('Please check out the context menu (right mouse button into plotrect)');
          Lines.Add('to modify appearance, axis scaling, etc....');
          Lines.Add('Log sacling is available per axis but log scales for negative values are a not so good idea');
       end;
    1:  with mem_info do begin
          Clear;
          Lines.Add('Above you see a pseudo 3D plot area with 3 axes and some remaining bugs.');
          Lines.Add('Data is added to 3D series via method  AddValue(X,Y,Z)');
          Lines.Add('However for most of the real world data pseudo 3D drawing might not be very useful');
          Lines.Add('Therefore the 3D series would need some internal rework if you really need it');
          Lines.Add('Basically it is a 2D plot with datapoints additionally shifted along the Z axis');
          Lines.Add('which makes the display highly ambiguous.');
          Lines.Add('In the rare cases where you find this sort of display useful, you might');
          Lines.Add('Iconsider coloring the Y axis as in the demo....');
       end;
    2:  with mem_info do begin
          Clear;
          Lines.Add('The fast 2D (or Spectrum) series was the intial idea behind MAAplot .');
          Lines.Add('Data is added to this series as an array via method  AddLine(TXYLine)');
          Lines.Add('This series is reasonably fast and can be used as');
          Lines.Add('Oscilloscope display, real time spectrum display and the like');
          Lines.Add('While plotting in 2D and 3D standard series is done directly to the Canvas');
          Lines.Add('the fast series and waterfalls plot to a memory image and can respect transparency.');
       end;
    3:  with mem_info do begin
          Clear;
          Lines.Add('The 2D waterfall series is meant for spectrogram plotting .');
          Lines.Add('Data is added to this series as an array via method  AddLine(TXYLine) like in the 2D spectrum');
          Lines.Add('Although 3 axes are used (X,Y and time), the time axis is scaled automagically.');
          Lines.Add('Only the newest dataline is plotted after adding,');
          Lines.Add('the rest of the current display is shifted.');
          Lines.Add('Number of shifted lines in the display is arbitrary and does not need to be a integer value');
       end;
    4:  with mem_info do begin
          Clear;
          Lines.Add('The 3D waterfall series is also meant for spectrogram plotting .');
          Lines.Add('Basically it works the same as the 2D waterfall except that the');
          Lines.Add('Y axis is now visible');
          Lines.Add('Viewing angle is arbitrary however no mouse control is implemented for that.');
          Lines.Add('The viewing angle could be modified by changing the drawing angles of the respective axes.');
       end;
  END;
end;

procedure Tfrm_main.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  CloseAction:=caFree;
end;

procedure Tfrm_main.cb_demotypeChange(Sender: TObject);
begin
  // The actual Plot (a class TPLot) is inside our FPlotDemo class
  // This is only to simplify life with this demo
  // a TPlot can also be used directly (FPlot := TPlot.Create(Self) and you can use it).
  IF FPLotDemo <> nil then FPLotDemo.Free;

  CASE cb_demotype.ItemIndex OF
    0: FPLotDemo := TPLotDemo2D.Create(Self);
    1: FPLotDemo := TPLotDemo3D.Create(Self);
    2: FPLotDemo := TPLotDemo2Dspectrum.Create(Self);
    3: FPLotDemo := TPLotDemo2Dwaterfall.Create(Self);
    4: FPLotDemo := TPLotDemo3Dwaterfall.Create(Self);
  END;
  IF FPLotDemo <> nil then begin
    FPLotDemo.Parent := gb_plot;       // we need a Parent component
    FPLotDemo.Align := alClient;       // The TPLot is a TWinControl, so set some alignment
    FPLotDemo.Plot.Repaint;
  end;
  UpdateMemo(cb_demotype.ItemIndex);
end;

procedure Tfrm_main.ed_alphaChange(Sender: TObject);
begin
  FPLotDemo.Alpha := (255 - ed_alpha.Value);
end;

procedure Tfrm_main.btn_dodemoClick(Sender: TObject);
begin
  IF FPLotDemo <> nil then FPLotDemo.UpdateDemoData;
end;

end.

