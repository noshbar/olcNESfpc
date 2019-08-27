unit main;

{$IFDEF FPC}
  {$mode objfpc}{$H+}
{$ENDIF}

interface

uses
  Forms, Classes, olc6502, bus;

type

  { TfrmMain }

  TfrmMain = class(TForm)
    procedure FormCreate(Sender: TObject);
    procedure FormKeyPress(Sender: TObject; var Key: char);
    procedure FormPaint(Sender: TObject);
  private
    { Private declarations }
    nes: TBus;

    procedure drawEverything;
  public
    { Public declarations }
    procedure DrawRam(x, y: integer; nAddr: word; nRows, nColumns: integer);
    procedure DrawCpu(x, y: integer);
    procedure DrawCode(x, y, nLines: integer);
  end;

var
  frmMain: TfrmMain;

implementation

{$R *.lfm}

uses Graphics, SysUtils;

function hex(n: integer; d: byte): string;
var
  i: integer;
  s: string;
begin
  s := StringOfChar('0', d);
  for i := d - 1 downto 0 do
  begin
    s[i + 1] := '0123456789ABCDEF'[(n and $F) + 1];
    n := n shr 4;
  end;
  result := s;
end;

procedure TfrmMain.DrawCode(x, y, nLines: integer);
var
  nLineY: integer;
  nSourceIndex: integer;
begin
  nLineY := (nLines shr 1) * 20 + y;
  nSourceIndex := nes.cpu.mapLines[nes.cpu.pc];
  if (nSourceIndex >= 0) then
  begin
    Canvas.Font.Color := clAqua;
    Canvas.TextOut(x, nLineY, nes.cpu.sourceDump[nSourceIndex]);
    Canvas.Font.Color := clWhite;
    while (nLineY < (nLines * 20) + y) do
    begin
      Inc(nSourceIndex);
      Inc(nLineY, 20);
      if (nSourceIndex < nes.cpu.sourceDump.Count) then
        Canvas.TextOut(x, nLineY, nes.cpu.sourceDump[nSourceIndex]);
    end;
  end;

  nLineY := (nLines shr 1) * 20 + y;
  nSourceIndex := nes.cpu.mapLines[nes.cpu.pc];
  while (nLineY > y) do
  begin
    Dec(nSourceIndex);
    Dec(nLineY, 20);
    if (nSourceIndex >= 0) then
      Canvas.TextOut(x, nLineY, nes.cpu.sourceDump[nSourceIndex]);
  end;
end;

procedure drawCpuStatus(Canvas: TCanvas; x, y: integer; status: char; flag: integer; nes: TBus);
begin
  if (nes.cpu.status and flag) <> 0 then
    Canvas.Font.Color := clLime
  else
    Canvas.Font.Color := clRed;

  Canvas.TextOut(x, y, status);
end;

procedure TfrmMain.DrawCpu(x, y: integer);
begin
  Canvas.TextOut(x, y, 'STATUS:');
  drawCpuStatus(Canvas, x + 100, y, 'N', FLAG_N, nes);
  drawCpuStatus(Canvas, x + 120, y, 'V', FLAG_V, nes);
  drawCpuStatus(Canvas, x + 140, y, '-', FLAG_U, nes);
  drawCpuStatus(Canvas, x + 160, y, 'B', FLAG_B, nes);
  drawCpuStatus(Canvas, x + 180, y, 'D', FLAG_D, nes);
  drawCpuStatus(Canvas, x + 200, y, 'I', FLAG_I, nes);
  drawCpuStatus(Canvas, x + 220, y, 'Z', FLAG_Z, nes);
  drawCpuStatus(Canvas, x + 240, y, 'C', FLAG_C, nes);
  Canvas.Font.Color := clWhite;
  Canvas.TextOut(x, y + 20, 'PC:      $' + hex(nes.cpu.pc, 4));
  Canvas.TextOut(x, y + 40, 'A:       $' + hex(nes.cpu.a, 2) + '  [' + IntToStr(nes.cpu.a) + ']');
  Canvas.TextOut(x, y + 60, 'X:       $' + hex(nes.cpu.x, 2) + '  [' + IntToStr(nes.cpu.x) + ']');
  Canvas.TextOut(x, y + 80, 'Y:       $' + hex(nes.cpu.y, 2) + '  [' + IntToStr(nes.cpu.y) + ']');
  Canvas.TextOut(x, y + 100, 'Stack P: $' + hex(nes.cpu.stkp, 4));
end;

procedure TfrmMain.DrawRam(x, y: integer; nAddr: word; nRows, nColumns: integer);
var
  nRamX, nRamY: integer;
  row, col: integer;
  sOffset: string;
begin
  nRamX := x;
  nRamY := y;
  for row := 0 to nRows - 1 do
  begin
    sOffset := '$' + hex(nAddr, 4) + ':';
    for col := 0 to nColumns - 1 do
    begin
      sOffset := sOffset + ' ' + hex(nes.read(nAddr, true), 2);
      nAddr := nAddr + 1;
    end;
    Canvas.TextOut(nRamX, nRamY, sOffset);
    nRamY := nRamY + 20;
  end;
end;

procedure TfrmMain.FormCreate(Sender: TObject);
const
  code: array[0..27] of byte = ($A2, $0A, $8E, $00, $00, $A2, $03, $8E, $01, $00, $AC, $00, $00, $A9, $00, $18, $6D, $01, $00, $88, $D0, $FA, $8D, $02, $00, $EA, $EA, $EA);
var
  nOffset: word;
  i: integer;
begin
  //DDLH: put these here so that if this is copied over a Delphi form, that it's set up correctly
  Caption := 'olc6502 emulator test';
  ClientWidth := 1024;
  ClientHeight := 768;
  Canvas.Brush.Color := clNavy;
  Canvas.Font.Color := clWhite;
  Canvas.Font.Name := 'Consolas';
  Canvas.Font.Size := -16;

  nes := TBus.Create;

  // Load Program (assembled at https://www.masswerk.at/6502/assembler.html)
  {
    *=$8000
    LDX #10
    STX $0000
    LDX #3
    STX $0001
    LDY $0000
    LDA #0
    CLC
    loop
    ADC $0001
    DEY
    BNE loop
    STA $0002
    NOP
    NOP
    NOP
  }

  nOffset := $8000;
  for i := 0 to Length(code) - 1 do
    nes.write(nOffset + i, code[i]);

  // Set Reset Vector
  nes.write($FFFC, $00);
  nes.write($FFFD, $80);

  // Dont forget to set IRQ and NMI vectors if you want to play with those

  // Extract dissassembly
  nes.cpu.disassemble($0000, $FFFF);

  // Reset
  nes.cpu.reset();
end;

procedure TfrmMain.drawEverything;
begin
  if (nes = nil) then
    exit;

  Canvas.FillRect(Rect(0, 0, ClientWidth, ClientHeight));
  Canvas.Font.Color := clWhite;

  // Draw Ram Page 0x00
  DrawRam(2, 2, $0000, 16, 16);
  DrawRam(2, 2 + 180 * 2, $8000, 16, 16);
  DrawCpu(600, 4);
  DrawCode(600, 144, 26);

  Canvas.TextOut(10, 370 * 2, 'SPACE = Step Instruction    R = RESET    I = IRQ    N = NMI');
end;

procedure TfrmMain.FormKeyPress(Sender: TObject; var Key: char);
begin
  if (nes = nil) then
    exit;

  if Key = ' ' then
    repeat
      nes.cpu.clock();
    until nes.cpu.complete();

  if (Key = 'r') then
    nes.cpu.reset();

  if (Key = 'i') then
    nes.cpu.irq();

  if (Key = 'n') then
    nes.cpu.nmi();

  Repaint;
end;

procedure TfrmMain.FormPaint(Sender: TObject);
begin
  drawEverything;
end;

end.