unit bus;

{$IFDEF FPC}
  {$mode objfpc}{$H+}
{$ENDIF}

interface

uses
  Classes, SysUtils, olc6502;

type
  TBus = class(TObject)
  public
    constructor Create();
    destructor Destroy();

  public
    cpu: Tolc6502;
    ram: TMemoryStream;

  public
    procedure write(addr: word; Data: byte);
    function read(addr: word; bReadOnly: boolean = false): byte;
  end;

implementation

constructor TBus.Create();
var
  i: integer;
begin
  // Allocate RAM for the device
  ram := TMemoryStream.Create();
  ram.SetSize(64 * 1024);

  // Connect CPU to communication bus
  cpu := Tolc6502.Create;
  cpu.ConnectBus(self);

  // Clear RAM contents, just in case :P
  ram.Position := 0;
  for i := 0 to 64 * 1024 - 1 do
    ram.WriteByte(0); // Delphi only has Write, but that needs a temp variable, ick.
end;

destructor TBus.Destroy();
begin
  ram.Free;
end;

procedure TBus.write(addr: word; Data: byte);
begin
  if (addr >= $0000) and (addr <= $FFFF) then
  begin
    ram.Position := addr;
    ram.write(Data, 1);
  end;
end;

function TBus.read(addr: word; bReadOnly: boolean = false): byte;
begin
  result := 0;
  if (addr >= $0000) and (addr <= $FFFF) then
  begin
    ram.Position := addr;
    ram.read(result, 1);
  end;
end;

end.