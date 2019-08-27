(*
	olc6502 - An emulation of the 6502/2A03 processor
	"Thanks Dad for believing computers were gonna be a big deal..." - javidx9

	License (OLC-3)
	~~~~~~~~~~~~~~~

	Copyright 2018-2019 OneLoneCoder.com

	Redistribution and use in source and binary forms, with or without
	modification, are permitted provided that the following conditions
	are met:

	1. Redistributions or derivations of source code must retain the above
	copyright notice, this list of conditions and the following disclaimer.

	2. Redistributions or derivative works in binary form must reproduce
	the above copyright notice. This list of conditions and the following
	disclaimer must be reproduced in the documentation and/or other
	materials provided with the distribution.

	3. Neither the name of the copyright holder nor the names of its
	contributors may be used to endorse or promote products derived
	from this software without specific prior written permission.

	THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
	"AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
	LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
	A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
	HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
	SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
	LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
	DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
	THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
	(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
	OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

	Background
	~~~~~~~~~~
	I love this microprocessor. It was at the heart of two of my favourite
	machines, the BBC Micro, and the Nintendo Entertainment System, as well
	as countless others in that era. I learnt to program on the Model B, and
	I learnt to love games on the NES, so in many ways, this processor is
	why I am the way I am today.

	In February 2019, I decided to undertake a selfish personal project and
	build a NES emulator. Ive always wanted to, and as such I've avoided
	looking at source code for such things. This made making this a real
	personal challenge. I know its been done countless times, and very likely
	in far more clever and accurate ways than mine, but I'm proud of this.

	Datasheet: http://archive.6502.org/datasheets/rockwell_r650x_r651x.pdf

	Files: olc6502.h, olc6502.cpp

	Relevant Video: https://youtu.be/8XmxKPJDGU0

	Links
	~~~~~
	YouTube:	https://www.youtube.com/javidx9
				https://www.youtube.com/javidx9extra
	Discord:	https://discord.gg/WhwHUMV
	Twitter:	https://www.twitter.com/javidx9
	Twitch:		https://www.twitch.tv/javidx9
	GitHub:		https://www.github.com/onelonecoder
	Patreon:	https://www.patreon.com/javidx9
	Homepage:	https://www.onelonecoder.com

	Author
	~~~~~~
	David Barr, aka javidx9, ©OneLoneCoder 2019
*)

{
  Ported to Object Pascal by Dirk de la Hunt, aka noshbar
  Twitter: https://www.twitter.com/noshbar
}

unit olc6502;

{$IFDEF FPC}
  {$mode objfpc}{$H+}
{$ENDIF}

interface

uses
  Classes, SysUtils,
{$IFDEF FPC}
  FGL
{$ELSE}
  Generics.Collections
{$ENDIF};

// The status register stores 8 flags. Ive enumerated these here for ease
// of access. You can access the status register directly since its public.
// The bits have different interpretations depending upon the context and
// instruction being executed.
const FLAG_C = (1 SHL 0);	// Carry Bit
const FLAG_Z = (1 SHL 1);	// Zero
const FLAG_I = (1 SHL 2);	// Disable Interrupts
const FLAG_D = (1 SHL 3);	// Decimal Mode (unused in this implementation)
const FLAG_B = (1 SHL 4);	// Break
const FLAG_U = (1 SHL 5);	// Unused
const FLAG_V = (1 SHL 6);	// Overflow
const FLAG_N = (1 SHL 7);	// Negative

type
  // This structure and the following vector are used to compile and store
  // the opcode translation table. The 6502 can effectively have 256
  // different instructions. Each of these are stored in a table in numerical
  // order so they can be looked up easily, with no decoding required.
  // Each table entry holds:
  //	Pneumonic : A textual representation of the instruction (used for disassembly)
  //	Opcode Function: A function pointer to the implementation of the opcode
  //	Opcode Address Mode : A function pointer to the implementation of the
  //						  addressing mechanism used by the instruction
  //	Cycle Count : An integer that represents the base number of clock cycles the
  //				  CPU requires to perform the instruction
  TAddrMode = (_IMP,_IMM,_ZP0,_ZPX,_ZPY,_REL,_ABS,_ABX,_ABY,_IND,_IZX,_IZY);
  TInstructionCall = function() : byte of object;
  TINSTRUCTION = record
    name     : string;
    operate  : TInstructionCall;
    addrmode : TInstructionCall;
    cycles   : byte;
    addrenum : TAddrMode; //DDLH: I couldn't figure out a cross-toolchain way of comparing method pointers
  end;                    //so I doubled down on grossness and store the address mode separately
{$IFDEF FPC}
  TAddressSourceMap = specialize TFPGMap<word, integer>;
{$ENDIF}

type
  // The 6502 Emulation Class. This is it!
  Tolc6502 = class(TObject)
    public
      constructor Create();
      destructor Destroy(); override;

    public
      // CPU Core registers, exposed as public here for ease of access from external
      // examinors. This is all the 6502 has.
      a      : byte; // Accumulator Register
      x      : byte; // X Register
      y      : byte; // Y Register
      stkp   : byte; // Stack Pointer (points to location on bus)
      pc     : word; // Program Counter
      status : byte; // Status Register

{$IFDEF FPC}
      mapLines : TAddressSourceMap;
{$ELSE}
      mapLines : TDictionary<word, integer>;
{$ENDIF}
      sourceDump : TStringList; //DDLH: decompile the source to text, find the offset using the map
                                //I had to do this because I don't know Delphi Generics well enough,
                                //and couldn't find out how to "get the next" item in a TDictionary
                                //if all I had was the result of a retrieval

      // External event functions. In hardware these represent pins that are asserted
      // to produce a change in state.
      procedure reset();	// Reset Interrupt - Forces CPU into known state
      procedure irq();		// Interrupt Request - Executes an instruction at a specific location
      procedure nmi();		// Non-Maskable Interrupt Request - As above, but cannot be disabled
      procedure clock();	// Perform one clock cycle's worth of update

      // Indicates the current instruction has completed by returning true. This is
      // a utility function to enable "step-by-step" execution, without manually
      // clocking every cycle
      function complete() : boolean;

      // Link this CPU to a communications bus
      procedure ConnectBus(n : TObject);

      // Produces a map of strings, with keys equivalent to instruction start locations
      // in memory, for the specified address range
      procedure disassemble(nStart : word; nStop : word);

    private
	  // Convenience functions to access status register
	  function GetFlag(f : byte) : byte;
	  procedure SetFlag(f : byte; v : boolean);

    private
	  // Assisstive variables to facilitate emulation
	  fetched     : byte;    // Represents the working input value to the ALU
	  temp        : word;    // A convenience variable used everywhere
	  addr_abs    : word;    // All used memory addresses end up in here
	  addr_rel    : word;    // Represents absolute address following a branch
	  opcode      : byte;    // Is the instruction byte
	  cycles      : byte;    // Counts how many cycles the instruction has remaining
	  clock_count : integer; // A global accumulation of the number of clocks

    private
	  // Linkage to the communications bus
	  bus : TObject;
	  function read(address : word) : byte;
	  procedure write(address : word; d : byte);

	  // The read location of data can come from two sources, a memory address, or
	  // its immediately available as part of the instruction. This function decides
	  // depending on address mode of instruction byte
	  function fetch() : byte;

    private
	  // Addressing Modes =============================================
	  // The 6502 has a variety of addressing modes to access data in
	  // memory, some of which are direct and some are indirect (like
	  // pointers in C++). Each opcode contains information about which
	  // addressing mode should be employed to facilitate the
	  // instruction, in regards to where it reads/writes the data it
	  // uses. The address mode changes the number of bytes that
	  // makes up the full instruction, so we implement addressing
	  // before executing the instruction, to make sure the program
	  // counter is at the correct location, the instruction is
	  // primed with the addresses it needs, and the number of clock
	  // cycles the instruction requires is calculated. These functions
	  // may adjust the number of cycles required depending upon where
	  // and how the memory is accessed, so they return the required
	  // adjustment.

	  function _IMP() : byte;	function _IMM() : byte;
	  function _ZP0() : byte;	function _ZPX() : byte;
	  function _ZPY() : byte;	function _REL() : byte;
	  function _ABS() : byte;	function _ABX() : byte;
	  function _ABY() : byte;	function _IND() : byte;
	  function _IZX() : byte;	function _IZY() : byte;

    private
	  // Opcodes ======================================================
	  // There are 56 "legitimate" opcodes provided by the 6502 CPU. I
	  // have not modelled "unofficial" opcodes. As each opcode is
	  // defined by 1 byte, there are potentially 256 possible codes.
	  // Codes are not used in a "switch case" style on a processor,
	  // instead they are repsonisble for switching individual parts of
	  // CPU circuits on and off. The opcodes listed here are official,
	  // meaning that the functionality of the chip when provided with
	  // these codes is as the developers intended it to be. Unofficial
	  // codes will of course also influence the CPU circuitry in
	  // interesting ways, and can be exploited to gain additional
	  // functionality!
	  //
	  // These functions return 0 normally, but some are capable of
	  // requiring more clock cycles when executed under certain
	  // conditions combined with certain addressing modes. If that is
	  // the case, they return 1.
	  //
	  // I have included detailed explanations of each function _in
	  // the class implementation file. Note they are listed in
	  // alphabetical order here for ease of finding.

      //DDLH: had to add underscores, because "AND" is a reserved word in Pascal
	  function _ADC() : byte;	function _AND() : byte;	function _ASL() : byte;	function _BCC() : byte;
	  function _BCS() : byte;	function _BEQ() : byte;	function _BIT() : byte;	function _BMI() : byte;
	  function _BNE() : byte;	function _BPL() : byte;	function _BRK() : byte;	function _BVC() : byte;
	  function _BVS() : byte;	function _CLC() : byte;	function _CLD() : byte;	function _CLI() : byte;
	  function _CLV() : byte;	function _CMP() : byte;	function _CPX() : byte;	function _CPY() : byte;
	  function _DEC() : byte;	function _DEX() : byte;	function _DEY() : byte;	function _EOR() : byte;
	  function _INC() : byte;	function _INX() : byte;	function _INY() : byte;	function _JMP() : byte;
	  function _JSR() : byte;	function _LDA() : byte;	function _LDX() : byte;	function _LDY() : byte;
	  function _LSR() : byte;	function _NOP() : byte;	function _ORA() : byte;	function _PHA() : byte;
	  function _PHP() : byte;	function _PLA() : byte;	function _PLP() : byte;	function _ROL() : byte;
	  function _ROR() : byte;	function _RTI() : byte;	function _RTS() : byte;	function _SBC() : byte;
	  function _SEC() : byte;	function _SED() : byte;	function _SEI() : byte;	function _STA() : byte;
	  function _STX() : byte;	function _STY() : byte;	function _TAX() : byte;	function _TAY() : byte;
	  function _TSX() : byte;	function _TXA() : byte;	function _TXS() : byte;	function _TYA() : byte;

	  // I capture all "unofficial" opcodes with this function. It is
	  // functionally identical to a NOP
	  function _XXX() : byte;

    private
	  lookup : array of TINSTRUCTION;
      procedure _def(name : string; operate : TInstructionCall; addrmode : TInstructionCall; addrenum : TAddrMode; cycleCount : byte);

  end;

implementation

uses
  bus;

procedure Tolc6502.ConnectBus(n : TObject);
begin
  bus := n;
end;

procedure Tolc6502._def(name : string; operate : TInstructionCall; addrmode : TInstructionCall; addrenum : TAddrMode;  cycleCount : byte);
var
  itemCount : integer;
begin
  itemCount := Length(lookup);
  SetLength(lookup, itemCount + 1);
  lookup[itemCount].name     := name;
  lookup[itemCount].operate  := operate;
  lookup[itemCount].addrmode := addrmode;
  lookup[itemCount].cycles   := cycleCount;
  lookup[itemCount].addrenum := addrenum;
end;

// Constructor
constructor Tolc6502.Create();
begin
	// Assembles the translation table. It's big, it's ugly, but it yields a convenient way
	// to emulate the 6502. I'm certain there are some "code-golf" strategies to reduce this
	// but I've deliberately kept it verbose for study and alteration

	// It is 16x16 entries. This gives 256 instructions. It is arranged to that the bottom
	// 4 bits of the instruction choose the column, and the top 4 bits choose the row.

	// For convenience to get function pointers to members of this class, I'm using this
	// or else it will be much much larger :D

	// The table is one big initialiser list of initialiser lists...

{$IFDEF FPC}
  _def('BRK', @_BRK, @_IMM, TAddrMode._IMM, 7); _def('ORA', @_ORA, @_IZX, TAddrMode._IZX, 6); _def('???', @_XXX, @_IMP, TAddrMode._IMP, 2); _def('???', @_XXX, @_IMP, TAddrMode._IMP, 8); _def('???', @_NOP, @_IMP, TAddrMode._IMP, 3); _def('ORA', @_ORA, @_ZP0, TAddrMode._ZP0, 3); _def('ASL', @_ASL, @_ZP0, TAddrMode._ZP0, 5); _def('???', @_XXX, @_IMP, TAddrMode._IMP, 5); _def('PHP', @_PHP, @_IMP, TAddrMode._IMP, 3); _def('ORA', @_ORA, @_IMM, TAddrMode._IMM, 2); _def('ASL', @_ASL, @_IMP, TAddrMode._IMP, 2); _def('???', @_XXX, @_IMP, TAddrMode._IMP, 2); _def('???', @_NOP, @_IMP, TAddrMode._IMP, 4); _def('ORA', @_ORA, @_ABS, TAddrMode._ABS, 4); _def('ASL', @_ASL, @_ABS, TAddrMode._ABS, 6); _def('???', @_XXX, @_IMP, TAddrMode._IMP, 6);
  _def('BPL', @_BPL, @_REL, TAddrMode._REL, 2); _def('ORA', @_ORA, @_IZY, TAddrMode._IZY, 5); _def('???', @_XXX, @_IMP, TAddrMode._IMP, 2); _def('???', @_XXX, @_IMP, TAddrMode._IMP, 8); _def('???', @_NOP, @_IMP, TAddrMode._IMP, 4); _def('ORA', @_ORA, @_ZPX, TAddrMode._ZPX, 4); _def('ASL', @_ASL, @_ZPX, TAddrMode._ZPX, 6); _def('???', @_XXX, @_IMP, TAddrMode._IMP, 6); _def('CLC', @_CLC, @_IMP, TAddrMode._IMP, 2); _def('ORA', @_ORA, @_ABY, TAddrMode._ABY, 4); _def('???', @_NOP, @_IMP, TAddrMode._IMP, 2); _def('???', @_XXX, @_IMP, TAddrMode._IMP, 7); _def('???', @_NOP, @_IMP, TAddrMode._IMP, 4); _def('ORA', @_ORA, @_ABX, TAddrMode._ABX, 4); _def('ASL', @_ASL, @_ABX, TAddrMode._ABX, 7); _def('???', @_XXX, @_IMP, TAddrMode._IMP, 7);
  _def('JSR', @_JSR, @_ABS, TAddrMode._ABS, 6); _def('AND', @_AND, @_IZX, TAddrMode._IZX, 6); _def('???', @_XXX, @_IMP, TAddrMode._IMP, 2); _def('???', @_XXX, @_IMP, TAddrMode._IMP, 8); _def('BIT', @_BIT, @_ZP0, TAddrMode._ZP0, 3); _def('AND', @_AND, @_ZP0, TAddrMode._ZP0, 3); _def('ROL', @_ROL, @_ZP0, TAddrMode._ZP0, 5); _def('???', @_XXX, @_IMP, TAddrMode._IMP, 5); _def('PLP', @_PLP, @_IMP, TAddrMode._IMP, 4); _def('AND', @_AND, @_IMM, TAddrMode._IMM, 2); _def('ROL', @_ROL, @_IMP, TAddrMode._IMP, 2); _def('???', @_XXX, @_IMP, TAddrMode._IMP, 2); _def('BIT', @_BIT, @_ABS, TAddrMode._ABS, 4); _def('AND', @_AND, @_ABS, TAddrMode._ABS, 4); _def('ROL', @_ROL, @_ABS, TAddrMode._ABS, 6); _def('???', @_XXX, @_IMP, TAddrMode._IMP, 6);
  _def('BMI', @_BMI, @_REL, TAddrMode._REL, 2); _def('AND', @_AND, @_IZY, TAddrMode._IZY, 5); _def('???', @_XXX, @_IMP, TAddrMode._IMP, 2); _def('???', @_XXX, @_IMP, TAddrMode._IMP, 8); _def('???', @_NOP, @_IMP, TAddrMode._IMP, 4); _def('AND', @_AND, @_ZPX, TAddrMode._ZPX, 4); _def('ROL', @_ROL, @_ZPX, TAddrMode._ZPX, 6); _def('???', @_XXX, @_IMP, TAddrMode._IMP, 6); _def('SEC', @_SEC, @_IMP, TAddrMode._IMP, 2); _def('AND', @_AND, @_ABY, TAddrMode._ABY, 4); _def('???', @_NOP, @_IMP, TAddrMode._IMP, 2); _def('???', @_XXX, @_IMP, TAddrMode._IMP, 7); _def('???', @_NOP, @_IMP, TAddrMode._IMP, 4); _def('AND', @_AND, @_ABX, TAddrMode._ABX, 4); _def('ROL', @_ROL, @_ABX, TAddrMode._ABX, 7); _def('???', @_XXX, @_IMP, TAddrMode._IMP, 7);
  _def('RTI', @_RTI, @_IMP, TAddrMode._IMP, 6); _def('EOR', @_EOR, @_IZX, TAddrMode._IZX, 6); _def('???', @_XXX, @_IMP, TAddrMode._IMP, 2); _def('???', @_XXX, @_IMP, TAddrMode._IMP, 8); _def('???', @_NOP, @_IMP, TAddrMode._IMP, 3); _def('EOR', @_EOR, @_ZP0, TAddrMode._ZP0, 3); _def('LSR', @_LSR, @_ZP0, TAddrMode._ZP0, 5); _def('???', @_XXX, @_IMP, TAddrMode._IMP, 5); _def('PHA', @_PHA, @_IMP, TAddrMode._IMP, 3); _def('EOR', @_EOR, @_IMM, TAddrMode._IMM, 2); _def('LSR', @_LSR, @_IMP, TAddrMode._IMP, 2); _def('???', @_XXX, @_IMP, TAddrMode._IMP, 2); _def('JMP', @_JMP, @_ABS, TAddrMode._ABS, 3); _def('EOR', @_EOR, @_ABS, TAddrMode._ABS, 4); _def('LSR', @_LSR, @_ABS, TAddrMode._ABS, 6); _def('???', @_XXX, @_IMP, TAddrMode._IMP, 6);
  _def('BVC', @_BVC, @_REL, TAddrMode._REL, 2); _def('EOR', @_EOR, @_IZY, TAddrMode._IZY, 5); _def('???', @_XXX, @_IMP, TAddrMode._IMP, 2); _def('???', @_XXX, @_IMP, TAddrMode._IMP, 8); _def('???', @_NOP, @_IMP, TAddrMode._IMP, 4); _def('EOR', @_EOR, @_ZPX, TAddrMode._ZPX, 4); _def('LSR', @_LSR, @_ZPX, TAddrMode._ZPX, 6); _def('???', @_XXX, @_IMP, TAddrMode._IMP, 6); _def('CLI', @_CLI, @_IMP, TAddrMode._IMP, 2); _def('EOR', @_EOR, @_ABY, TAddrMode._ABY, 4); _def('???', @_NOP, @_IMP, TAddrMode._IMP, 2); _def('???', @_XXX, @_IMP, TAddrMode._IMP, 7); _def('???', @_NOP, @_IMP, TAddrMode._IMP, 4); _def('EOR', @_EOR, @_ABX, TAddrMode._ABX, 4); _def('LSR', @_LSR, @_ABX, TAddrMode._ABX, 7); _def('???', @_XXX, @_IMP, TAddrMode._IMP, 7);
  _def('RTS', @_RTS, @_IMP, TAddrMode._IMP, 6); _def('ADC', @_ADC, @_IZX, TAddrMode._IZX, 6); _def('???', @_XXX, @_IMP, TAddrMode._IMP, 2); _def('???', @_XXX, @_IMP, TAddrMode._IMP, 8); _def('???', @_NOP, @_IMP, TAddrMode._IMP, 3); _def('ADC', @_ADC, @_ZP0, TAddrMode._ZP0, 3); _def('ROR', @_ROR, @_ZP0, TAddrMode._ZP0, 5); _def('???', @_XXX, @_IMP, TAddrMode._IMP, 5); _def('PLA', @_PLA, @_IMP, TAddrMode._IMP, 4); _def('ADC', @_ADC, @_IMM, TAddrMode._IMM, 2); _def('ROR', @_ROR, @_IMP, TAddrMode._IMP, 2); _def('???', @_XXX, @_IMP, TAddrMode._IMP, 2); _def('JMP', @_JMP, @_IND, TAddrMode._IND, 5); _def('ADC', @_ADC, @_ABS, TAddrMode._ABS, 4); _def('ROR', @_ROR, @_ABS, TAddrMode._ABS, 6); _def('???', @_XXX, @_IMP, TAddrMode._IMP, 6);
  _def('BVS', @_BVS, @_REL, TAddrMode._REL, 2); _def('ADC', @_ADC, @_IZY, TAddrMode._IZY, 5); _def('???', @_XXX, @_IMP, TAddrMode._IMP, 2); _def('???', @_XXX, @_IMP, TAddrMode._IMP, 8); _def('???', @_NOP, @_IMP, TAddrMode._IMP, 4); _def('ADC', @_ADC, @_ZPX, TAddrMode._ZPX, 4); _def('ROR', @_ROR, @_ZPX, TAddrMode._ZPX, 6); _def('???', @_XXX, @_IMP, TAddrMode._IMP, 6); _def('SEI', @_SEI, @_IMP, TAddrMode._IMP, 2); _def('ADC', @_ADC, @_ABY, TAddrMode._ABY, 4); _def('???', @_NOP, @_IMP, TAddrMode._IMP, 2); _def('???', @_XXX, @_IMP, TAddrMode._IMP, 7); _def('???', @_NOP, @_IMP, TAddrMode._IMP, 4); _def('ADC', @_ADC, @_ABX, TAddrMode._ABX, 4); _def('ROR', @_ROR, @_ABX, TAddrMode._ABX, 7); _def('???', @_XXX, @_IMP, TAddrMode._IMP, 7);
  _def('???', @_NOP, @_IMP, TAddrMode._IMP, 2); _def('STA', @_STA, @_IZX, TAddrMode._IZX, 6); _def('???', @_NOP, @_IMP, TAddrMode._IMP, 2); _def('???', @_XXX, @_IMP, TAddrMode._IMP, 6); _def('STY', @_STY, @_ZP0, TAddrMode._ZP0, 3); _def('STA', @_STA, @_ZP0, TAddrMode._ZP0, 3); _def('STX', @_STX, @_ZP0, TAddrMode._ZP0, 3); _def('???', @_XXX, @_IMP, TAddrMode._IMP, 3); _def('DEY', @_DEY, @_IMP, TAddrMode._IMP, 2); _def('???', @_NOP, @_IMP, TAddrMode._IMP, 2); _def('TXA', @_TXA, @_IMP, TAddrMode._IMP, 2); _def('???', @_XXX, @_IMP, TAddrMode._IMP, 2); _def('STY', @_STY, @_ABS, TAddrMode._ABS, 4); _def('STA', @_STA, @_ABS, TAddrMode._ABS, 4); _def('STX', @_STX, @_ABS, TAddrMode._ABS, 4); _def('???', @_XXX, @_IMP, TAddrMode._IMP, 4);
  _def('BCC', @_BCC, @_REL, TAddrMode._REL, 2); _def('STA', @_STA, @_IZY, TAddrMode._IZY, 6); _def('???', @_XXX, @_IMP, TAddrMode._IMP, 2); _def('???', @_XXX, @_IMP, TAddrMode._IMP, 6); _def('STY', @_STY, @_ZPX, TAddrMode._ZPX, 4); _def('STA', @_STA, @_ZPX, TAddrMode._ZPX, 4); _def('STX', @_STX, @_ZPY, TAddrMode._ZPY, 4); _def('???', @_XXX, @_IMP, TAddrMode._IMP, 4); _def('TYA', @_TYA, @_IMP, TAddrMode._IMP, 2); _def('STA', @_STA, @_ABY, TAddrMode._ABY, 5); _def('TXS', @_TXS, @_IMP, TAddrMode._IMP, 2); _def('???', @_XXX, @_IMP, TAddrMode._IMP, 5); _def('???', @_NOP, @_IMP, TAddrMode._IMP, 5); _def('STA', @_STA, @_ABX, TAddrMode._ABX, 5); _def('???', @_XXX, @_IMP, TAddrMode._IMP, 5); _def('???', @_XXX, @_IMP, TAddrMode._IMP, 5);
  _def('LDY', @_LDY, @_IMM, TAddrMode._IMM, 2); _def('LDA', @_LDA, @_IZX, TAddrMode._IZX, 6); _def('LDX', @_LDX, @_IMM, TAddrMode._IMM, 2); _def('???', @_XXX, @_IMP, TAddrMode._IMP, 6); _def('LDY', @_LDY, @_ZP0, TAddrMode._ZP0, 3); _def('LDA', @_LDA, @_ZP0, TAddrMode._ZP0, 3); _def('LDX', @_LDX, @_ZP0, TAddrMode._ZP0, 3); _def('???', @_XXX, @_IMP, TAddrMode._IMP, 3); _def('TAY', @_TAY, @_IMP, TAddrMode._IMP, 2); _def('LDA', @_LDA, @_IMM, TAddrMode._IMM, 2); _def('TAX', @_TAX, @_IMP, TAddrMode._IMP, 2); _def('???', @_XXX, @_IMP, TAddrMode._IMP, 2); _def('LDY', @_LDY, @_ABS, TAddrMode._ABS, 4); _def('LDA', @_LDA, @_ABS, TAddrMode._ABS, 4); _def('LDX', @_LDX, @_ABS, TAddrMode._ABS, 4); _def('???', @_XXX, @_IMP, TAddrMode._IMP, 4);
  _def('BCS', @_BCS, @_REL, TAddrMode._REL, 2); _def('LDA', @_LDA, @_IZY, TAddrMode._IZY, 5); _def('???', @_XXX, @_IMP, TAddrMode._IMP, 2); _def('???', @_XXX, @_IMP, TAddrMode._IMP, 5); _def('LDY', @_LDY, @_ZPX, TAddrMode._ZPX, 4); _def('LDA', @_LDA, @_ZPX, TAddrMode._ZPX, 4); _def('LDX', @_LDX, @_ZPY, TAddrMode._ZPY, 4); _def('???', @_XXX, @_IMP, TAddrMode._IMP, 4); _def('CLV', @_CLV, @_IMP, TAddrMode._IMP, 2); _def('LDA', @_LDA, @_ABY, TAddrMode._ABY, 4); _def('TSX', @_TSX, @_IMP, TAddrMode._IMP, 2); _def('???', @_XXX, @_IMP, TAddrMode._IMP, 4); _def('LDY', @_LDY, @_ABX, TAddrMode._ABX, 4); _def('LDA', @_LDA, @_ABX, TAddrMode._ABX, 4); _def('LDX', @_LDX, @_ABY, TAddrMode._ABY, 4); _def('???', @_XXX, @_IMP, TAddrMode._IMP, 4);
  _def('CPY', @_CPY, @_IMM, TAddrMode._IMM, 2); _def('CMP', @_CMP, @_IZX, TAddrMode._IZX, 6); _def('???', @_NOP, @_IMP, TAddrMode._IMP, 2); _def('???', @_XXX, @_IMP, TAddrMode._IMP, 8); _def('CPY', @_CPY, @_ZP0, TAddrMode._ZP0, 3); _def('CMP', @_CMP, @_ZP0, TAddrMode._ZP0, 3); _def('DEC', @_DEC, @_ZP0, TAddrMode._ZP0, 5); _def('???', @_XXX, @_IMP, TAddrMode._IMP, 5); _def('INY', @_INY, @_IMP, TAddrMode._IMP, 2); _def('CMP', @_CMP, @_IMM, TAddrMode._IMM, 2); _def('DEX', @_DEX, @_IMP, TAddrMode._IMP, 2); _def('???', @_XXX, @_IMP, TAddrMode._IMP, 2); _def('CPY', @_CPY, @_ABS, TAddrMode._ABS, 4); _def('CMP', @_CMP, @_ABS, TAddrMode._ABS, 4); _def('DEC', @_DEC, @_ABS, TAddrMode._ABS, 6); _def('???', @_XXX, @_IMP, TAddrMode._IMP, 6);
  _def('BNE', @_BNE, @_REL, TAddrMode._REL, 2); _def('CMP', @_CMP, @_IZY, TAddrMode._IZY, 5); _def('???', @_XXX, @_IMP, TAddrMode._IMP, 2); _def('???', @_XXX, @_IMP, TAddrMode._IMP, 8); _def('???', @_NOP, @_IMP, TAddrMode._IMP, 4); _def('CMP', @_CMP, @_ZPX, TAddrMode._ZPX, 4); _def('DEC', @_DEC, @_ZPX, TAddrMode._ZPX, 6); _def('???', @_XXX, @_IMP, TAddrMode._IMP, 6); _def('CLD', @_CLD, @_IMP, TAddrMode._IMP, 2); _def('CMP', @_CMP, @_ABY, TAddrMode._ABY, 4); _def('NOP', @_NOP, @_IMP, TAddrMode._IMP, 2); _def('???', @_XXX, @_IMP, TAddrMode._IMP, 7); _def('???', @_NOP, @_IMP, TAddrMode._IMP, 4); _def('CMP', @_CMP, @_ABX, TAddrMode._ABX, 4); _def('DEC', @_DEC, @_ABX, TAddrMode._ABX, 7); _def('???', @_XXX, @_IMP, TAddrMode._IMP, 7);
  _def('CPX', @_CPX, @_IMM, TAddrMode._IMM, 2); _def('SBC', @_SBC, @_IZX, TAddrMode._IZX, 6); _def('???', @_NOP, @_IMP, TAddrMode._IMP, 2); _def('???', @_XXX, @_IMP, TAddrMode._IMP, 8); _def('CPX', @_CPX, @_ZP0, TAddrMode._ZP0, 3); _def('SBC', @_SBC, @_ZP0, TAddrMode._ZP0, 3); _def('INC', @_INC, @_ZP0, TAddrMode._ZP0, 5); _def('???', @_XXX, @_IMP, TAddrMode._IMP, 5); _def('INX', @_INX, @_IMP, TAddrMode._IMP, 2); _def('SBC', @_SBC, @_IMM, TAddrMode._IMM, 2); _def('NOP', @_NOP, @_IMP, TAddrMode._IMP, 2); _def('???', @_SBC, @_IMP, TAddrMode._IMP, 2); _def('CPX', @_CPX, @_ABS, TAddrMode._ABS, 4); _def('SBC', @_SBC, @_ABS, TAddrMode._ABS, 4); _def('INC', @_INC, @_ABS, TAddrMode._ABS, 6); _def('???', @_XXX, @_IMP, TAddrMode._IMP, 6);
  _def('BEQ', @_BEQ, @_REL, TAddrMode._REL, 2); _def('SBC', @_SBC, @_IZY, TAddrMode._IZY, 5); _def('???', @_XXX, @_IMP, TAddrMode._IMP, 2); _def('???', @_XXX, @_IMP, TAddrMode._IMP, 8); _def('???', @_NOP, @_IMP, TAddrMode._IMP, 4); _def('SBC', @_SBC, @_ZPX, TAddrMode._ZPX, 4); _def('INC', @_INC, @_ZPX, TAddrMode._ZPX, 6); _def('???', @_XXX, @_IMP, TAddrMode._IMP, 6); _def('SED', @_SED, @_IMP, TAddrMode._IMP, 2); _def('SBC', @_SBC, @_ABY, TAddrMode._ABY, 4); _def('NOP', @_NOP, @_IMP, TAddrMode._IMP, 2); _def('???', @_XXX, @_IMP, TAddrMode._IMP, 7); _def('???', @_NOP, @_IMP, TAddrMode._IMP, 4); _def('SBC', @_SBC, @_ABX, TAddrMode._ABX, 4); _def('INC', @_INC, @_ABX, TAddrMode._ABX, 7); _def('???', @_XXX, @_IMP, TAddrMode._IMP, 7);
{$ELSE}
  The same as above, but without the @ symbols
{$ENDIF}
end;

destructor Tolc6502.Destroy();
begin
	mapLines.Free;
  sourceDump.Free;
end;





///////////////////////////////////////////////////////////////////////////////
// BUS CONNECTIVITY

// Reads an 8-bit byte from the bus, located at the specified 16-bit address
function Tolc6502.read(address : word) : byte;
begin
	// In normal operation "read only" is set to false. This may seem odd. Some
	// devices on the bus may change state when they are read from, and this
	// is intentional under normal circumstances. However the disassembler will
	// want to read the data at an address without changing the state of the
	// devices on the bus
	result := TBus(bus).read(address, false);
end;

// Writes a byte to the bus at the specified address
procedure Tolc6502.write(address : word; d : byte);
begin
	TBus(bus).write(address, d);
end;





///////////////////////////////////////////////////////////////////////////////
// EXTERNAL INPUTS

// Forces the 6502 into a known state. This is hard-wired inside the CPU. The
// registers are set to $00, the status register is cleared except for unused
// bit which remains at 1. An absolute address is read from location $FFFC
// which contains a second address that the program counter is set to. This
// allows the programmer to jump to a known and programmable location in the
// memory to start executing from. Typically the programmer would set the value
// at location $FFFC at compile time.
procedure Tolc6502.reset();
var
  lo : word;
  hi : word;
begin
	// Get address to set program counter to
	addr_abs := $FFFC;
	lo := read(addr_abs + 0);
	hi := read(addr_abs + 1);

	// Set it
	pc := (hi SHL 8) OR lo;

	// Reset internal registers
	a := 0;
	x := 0;
	y := 0;
	stkp := $FD;
	status := $00 OR FLAG_U;

	// Clear internal helper variables
	addr_rel := $0000;
	addr_abs := $0000;
	fetched := $00;

	// Reset takes time
	cycles := 8;
end;


// Interrupt requests are a complex operation and only happen if the
// "disable interrupt" flag is 0. IRQs can happen at any time, but
// you dont want them to be destructive to the operation of the running
// program. Therefore the current instruction is allowed to finish
// (which I facilitate by doing the whole thing when cycles = 0) and
// then the current program counter is stored on the stack. Then the
// current status register is stored on the stack. When the routine
// that services the interrupt has finished, the status register
// and program counter can be restored to how they where before it
// occurred. This is impemented by the "RTI" instruction. Once the IRQ
// has happened, in a similar way to a reset, a programmable address
// is read form hard coded location $FFFE, which is subsequently
// set to the program counter.
procedure Tolc6502.irq();
var
  lo : word;
  hi : word;
begin
	// If interrupts are allowed
	if (GetFlag(FLAG_I) = 0) then
	begin
		// Push the program counter to the stack. It's 16-bits dont
		// forget so that takes two pushes
		write($0100 + stkp, (pc SHR 8) AND $00FF);
		Dec(stkp);
		write($0100 + stkp, pc AND $00FF);
		Dec(stkp);

		// Then Push the status register to the stack
		SetFlag(FLAG_B, false);
		SetFlag(FLAG_U, true);
		SetFlag(FLAG_I, true);
		write($0100 + stkp, status);
		Dec(stkp);

		// Read new program counter location from fixed address
		addr_abs := $FFFE;
		lo := read(addr_abs + 0);
		hi := read(addr_abs + 1);
		pc := (hi SHL 8) OR lo;

		// IRQs take time
		cycles := 7;
	end;
end;


// A Non-Maskable Interrupt cannot be ignored. It behaves in exactly the
// same way as a regular IRQ, but reads the new program counter address
// form location $FFFA.
procedure Tolc6502.nmi();
var
  lo : word;
  hi : word;
begin
	write($0100 + stkp, (pc SHR 8) AND $00FF);
	Dec(stkp);
	write($0100 + stkp, pc AND $00FF);
	Dec(stkp);

	SetFlag(FLAG_B, false);
	SetFlag(FLAG_U, true);
	SetFlag(FLAG_I, true);
	write($0100 + stkp, status);
	Dec(stkp);

	addr_abs := $FFFA;
	lo := read(addr_abs + 0);
	hi := read(addr_abs + 1);
	pc := (hi SHL 8) OR lo;

	cycles := 8;
end;

// Perform one clock cycles worth of emulation
procedure Tolc6502.clock();
var
  additional_cycle1 : byte;
  additional_cycle2 : byte;
begin
	// Each instruction requires a variable number of clock cycles to execute.
	// In my emulation, I only care about the final result and so I perform
	// the entire computation in one hit. In hardware, each clock cycle would
	// perform "microcode" style transformations of the CPUs state.
	//
	// To remain compliant with connected devices, it's important that the
	// emulation also takes "time" in order to execute instructions, so I
	// implement that delay by simply counting down the cycles required by
	// the instruction. When it reaches 0, the instruction is complete, and
	// the next one is ready to be executed.
	if (cycles = 0) then
	begin
		// Read next instruction byte. This 8-bit value is used to index
		// the translation table to get the relevant information about
		// how to implement the instruction
		opcode := read(pc);

{#ifdef LOGMODE
		uint16_t log_pc := pc;
#endif}

		// Always set the unused status flag bit to 1
		SetFlag(FLAG_U, true);

		// Increment program counter, we read the opcode byte
		Inc(pc);

		// Get Starting number of cycles
		cycles := lookup[opcode].cycles;

		// Perform fetch of intermmediate data using the
		// required addressing mode
		additional_cycle1 := lookup[opcode].addrmode();

		// Perform operation
		additional_cycle2 := lookup[opcode].operate();

		// The addressmode and opcode may have altered the number
		// of cycles this instruction requires before its completed
		Inc(cycles, (additional_cycle1 AND additional_cycle2));

		// Always set the unused status flag bit to 1
		SetFlag(FLAG_U, true);

{#ifdef LOGMODE
		// This logger dumps every cycle the entire processor state for analysis.
		// This can be used for debugging the emulation, but has little utility
		// during emulation. Its also very slow, so only use if you have to.
		if (logfile = nullptr)	logfile := fopen("olc6502.txt", "wt");
		if (logfile != nullptr)
		begin
			fprintf(logfile, "%10d:%02d PC:%04X %s A:%02X X:%02X Y:%02X %s%s%s%s%s%s%s%s STKP:%02X\n",
				clock_count, 0, log_pc, "XXX", a, x, y,
				GetFlag(FLAG_N) ? "N" : ".",	GetFlag(FLAG_V) ? "V" : ".",	GetFlag(FLAG_U) ? "U" : ".",
				GetFlag(FLAG_B) ? "B" : ".",	GetFlag(FLAG_D) ? "D" : ".",	GetFlag(FLAG_I) ? "I" : ".",
				GetFlag(FLAG_Z) ? "Z" : ".",	GetFlag(FLAG_C) ? "C" : ".",	stkp);
		end;
#endif}
	end;

	// Increment global clock count - This is actually unused unless logging is enabled
	// but I've kept it in because its a handy watch variable for debugging
	Inc(clock_count);

	// Decrement the number of cycles remaining for this instruction
	Dec(cycles);
end;





///////////////////////////////////////////////////////////////////////////////
// FLAG FUNCTIONS

// Returns the value of a specific bit of the status register
function Tolc6502.GetFlag(f : byte) : byte;
begin
  result := 0;
  if ((status AND f) > 0) then
    result := 1;
end;

// Sets or clears a specific bit of the status register
procedure Tolc6502.SetFlag(f : byte; v : boolean);
begin
	if (v) then
		status := status OR f
	else
		status := status AND (NOT f);
end;





///////////////////////////////////////////////////////////////////////////////
// ADDRESSING MODES

// The 6502 can address between $0000 - $FFFF. The high byte is often referred
// to as the "page", and the low byte is the offset into that page. This implies
// there are 256 pages, each containing 256 bytes.
//
// Several addressing modes have the potential to require an additional clock
// cycle if they cross a page boundary. This is combined with several instructions
// that enable this additional clock cycle. So each addressing function returns
// a flag saying it has potential, as does each instruction. If both instruction
// and address function return 1, then an additional clock cycle is required.


// Address Mode: Implied
// There is no additional data required for this instruction. The instruction
// does something very simple like like sets a status bit. However, we will
// target the accumulator, for instructions like PHA
function Tolc6502._IMP() : byte;
begin
	fetched := a;
	result := 0;
end;


// Address Mode: Immediate
// The instruction expects the next byte to be used as a value, so we'll prep
// the read address to point to the next byte
function Tolc6502._IMM() : byte;
begin
	addr_abs := pc;
  Inc(pc);
	result := 0;
end;



// Address Mode: Zero Page
// To save program bytes, zero page addressing allows you to absolutely address
// a location in first $FF bytes of address range. Clearly this only requires
// one byte instead of the usual two.
function Tolc6502._ZP0() : byte;
begin
	addr_abs := read(pc);
	Inc(pc);
	addr_abs := addr_abs AND $00FF;
	result := 0;
end;



// Address Mode: Zero Page with X Offset
// Fundamentally the same as Zero Page addressing, but the contents of the X Register
// is added to the supplied single byte address. This is useful for iterating through
// ranges within the first page.
function Tolc6502._ZPX() : byte;
begin
	addr_abs := (read(pc) + x);
	Inc(pc);
	addr_abs := addr_abs AND $00FF;
  result := 0;
end;


// Address Mode: Zero Page with Y Offset
// Same as above but uses Y Register for offset
function Tolc6502._ZPY() : byte;
begin
	addr_abs := (read(pc) + y);
	Inc(pc);
	addr_abs := addr_abs AND $00FF;
	result := 0;
end;


// Address Mode: Relative
// This address mode is exclusive to branch instructions. The address
// must reside within -128 to +127 of the branch instruction, i.e.
// you cant directly branch to any address in the addressable range.
function Tolc6502._REL() : byte;
begin
	addr_rel := read(pc);
	Inc(pc);
	if (addr_rel AND $80) <> 0 then
		addr_rel := addr_rel OR $FF00;
	result := 0;
end;


// Address Mode: Absolute
// A full 16-bit address is loaded and used
function Tolc6502._ABS() : byte;
var
  lo : word;
  hi : word;
begin
	lo := read(pc);
	Inc(pc);
	hi := read(pc);
	Inc(pc);

	addr_abs := (hi SHL 8) OR lo;

	result := 0;
end;


// Address Mode: Absolute with X Offset
// Fundamentally the same as absolute addressing, but the contents of the X Register
// is added to the supplied two byte address. If the resulting address changes
// the page, an additional clock cycle is required
function Tolc6502._ABX() : byte;
var
  lo : word;
  hi : word;
begin
	lo := read(pc);
	Inc(pc);
	hi := read(pc);
	Inc(pc);

	addr_abs := (hi SHL 8) OR lo;
	addr_abs := addr_abs + x;

	if ((addr_abs AND $FF00) <> (hi SHL 8)) then
		result := 1
	else
		result := 0;
end;


// Address Mode: Absolute with Y Offset
// Fundamentally the same as absolute addressing, but the contents of the Y Register
// is added to the supplied two byte address. If the resulting address changes
// the page, an additional clock cycle is required
function Tolc6502._ABY() : byte;
var
  lo : word;
  hi : word;
begin
	lo := read(pc);
	Inc(pc);
	hi := read(pc);
	Inc(pc);

	addr_abs := (hi SHL 8) OR lo;
	addr_abs := addr_abs + y;

	if ((addr_abs AND $FF00) <> (hi SHL 8)) then
		result := 1
	else
		result := 0;
end;

// Note: The next 3 address modes use indirection (aka Pointers!)

// Address Mode: Indirect
// The supplied 16-bit address is read to get the actual 16-bit address. This is
// instruction is unusual in that it has a bug in the hardware! To emulate its
// function accurately, we also need to emulate this bug. If the low byte of the
// supplied address is $FF, then to read the high byte of the actual address
// we need to cross a page boundary. This doesnt actually work on the chip as
// designed, instead it wraps back around in the same page, yielding an
// invalid actual address
function Tolc6502._IND() : byte;
var
  ptr_lo : word;
  ptr_hi : word;
  ptr    : word;
begin
	ptr_lo := read(pc);
	Inc(pc);
	ptr_hi := read(pc);
	Inc(pc);

	ptr := (ptr_hi SHL 8) OR ptr_lo;

	if (ptr_lo = $00FF) then // Simulate page boundary hardware bug
	begin
		addr_abs := (read(ptr AND $FF00) SHL 8) OR read(ptr + 0);
	end
	else // Behave normally
	begin
		addr_abs := (read(ptr + 1) SHL 8) OR read(ptr + 0);
	end;

	result := 0;
end;


// Address Mode: Indirect X
// The supplied 8-bit address is offset by X Register to index
// a location in page $00. The actual 16-bit address is read
// from this location
function Tolc6502._IZX() : byte;
var
  t  : word;
  lo : word;
  hi : word;
begin
	t := read(pc);
	Inc(pc);

	lo := read(word(t + word(x)) AND $00FF);
	hi := read(word(t + word(x) + 1) AND $00FF);

	addr_abs := (hi SHL 8) OR lo;

	result := 0;
end;


// Address Mode: Indirect Y
// The supplied 8-bit address indexes a location in page $00. From
// here the actual 16-bit address is read, and the contents of
// Y Register is added to it to offset it. If the offset causes a
// change in page then an additional clock cycle is required.
function Tolc6502._IZY() : byte;
var
  t  : word;
  lo : word;
  hi : word;
begin
	t := read(pc);
	Inc(pc);

	lo := read(t AND $00FF);
	hi := read((t + 1) AND $00FF);

	addr_abs := (hi SHL 8) OR lo;
	addr_abs := addr_abs + y;

	if ((addr_abs AND $FF00) <> (hi SHL 8)) then
		result := 1
	else
		result := 0;
end;



// This function sources the data used by the instruction into
// a convenient numeric variable. Some instructions dont have to
// fetch data as the source is implied by the instruction. For example
// "INX" increments the X register. There is no additional data
// required. For all other addressing modes, the data resides at
// the location held within addr_abs, so it is read from there.
// Immediate adress mode exploits this slightly, as that has
// set addr_abs := pc + 1, so it fetches the data from the
// next byte for example "LDA $FF" just loads the accumulator with
// 256, i.e. no far reaching memory fetch is required. "fetched"
// is a variable global to the CPU, and is set by calling this
// function. It also returns it for convenience.
function Tolc6502.fetch() : byte;
begin
	if (lookup[opcode].addrenum <> TAddrMode._IMP) then
		fetched := read(addr_abs);
	result := fetched;
end;





///////////////////////////////////////////////////////////////////////////////
// INSTRUCTION IMPLEMENTATIONS

// Note: Ive started with the two most complicated instructions to emulate, which
// ironically is addition and subtraction! Ive tried to include a detailed
// explanation as to why they are so complex, yet so fundamental. Im also NOT
// going to do this through the explanation of 1 and 2's complement.

// Instruction: Add with Carry In
// Function:    A := A + M + C
// Flags Out:   C, V, N, Z
//
// Explanation:
// The purpose of this function is to add a value to the accumulator and a carry bit. If
// the result is > 255 there is an overflow setting the carry bit. Ths allows you to
// chain together ADC instructions to add numbers larger than 8-bits. This in itself is
// simple, however the 6502 supports the concepts of Negativity/Positivity and Signed Overflow.
//
// 10000100 := 128 + 4 := 132 in normal circumstances, we know this as unsigned and it allows
// us to represent numbers between 0 and 255 (given 8 bits). The 6502 can also interpret
// this word as something else if we assume those 8 bits represent the range -128 to +127,
// i.e. it has become signed.
//
// Since 132 > 127, it effectively wraps around, through -128, to -124. This wraparound is
// called overflow, and this is a useful to know as it indicates that the calculation has
// gone outside the permissable range, and therefore no longer makes numeric sense.
//
// Note the implementation of ADD is the same in binary, this is just about how the numbers
// are represented, so the word 10000100 can be both -124 and 132 depending upon the
// context the programming is using it in. We can prove this!
//
//  10000100 :=  132  or  -124
// +00010001 := + 17      + 17
//  ====    ==       ==     See, both are valid additions, but our interpretation of
//  10010101 :=  149  or  -107     the context changes the value, not the hardware!
//
// In principle under the -128 to 127 range:
// 10000000 := -128, 11111111 := -1, 00000000 := 0, 00000000 := +1, 01111111 := +127
// therefore negative numbers have the most significant set, positive numbers do not
//
// To assist us, the 6502 can set the overflow flag, if the result of the addition has
// wrapped around. V <- ~(A^M) AND A^(A+M+C) :D lol, let's work out why!
//
// Let's suppose we have A := 30, M := 10 and C := 0
//          A := 30 := 00011110
//          M := 10 := 00001010+
//     RESULT := 40 := 00101000
//
// Here we have not gone out of range. The resulting significant bit has not changed.
// So let's make a truth table to understand when overflow has occurred. Here I take
// the MSB of each component, where R is RESULT.
//
// A  M  R OR V OR A^R OR A^M |~(A^M) OR
// 0  0  0 OR 0 OR  0  OR  0  OR   1   |
// 0  0  1 OR 1 OR  1  OR  0  OR   1   |
// 0  1  0 OR 0 OR  0  OR  1  OR   0   |
// 0  1  1 OR 0 OR  1  OR  1  OR   0   OR  so V := ~(A^M) AND (A^R)
// 1  0  0 OR 0 OR  1  OR  1  OR   0   |
// 1  0  1 OR 0 OR  0  OR  1  OR   0   |
// 1  1  0 OR 1 OR  1  OR  0  OR   1   |
// 1  1  1 OR 0 OR  0  OR  0  OR   1   |
//
// We can see how the above equation calculates V, based on A, M and R. V was chosen
// based on the following hypothesis:
//       Positive Number + Positive Number := Negative Result -> Overflow
//       Negative Number + Negative Number := Positive Result -> Overflow
//       Positive Number + Negative Number := Either Result -> Cannot Overflow
//       Positive Number + Positive Number := Positive Result -> OK! No Overflow
//       Negative Number + Negative Number := Negative Result -> OK! NO Overflow

function Tolc6502._ADC() : byte;
begin
	// Grab the data that we are adding to the accumulator
	fetch();

	// Add is performed in 16-bit domain for emulation to capture any
	// carry bit, which will exist in bit 8 of the 16-bit word
	temp := word(a) + word(fetched) + word(GetFlag(FLAG_C));

	// The carry flag out exists in the high byte bit 0
	SetFlag(FLAG_C, temp > 255);

	// The Zero flag is set if the result is 0
	SetFlag(FLAG_Z, (temp AND $00FF) = 0);

	// The signed Overflow flag is set based on all that up there! :D
	SetFlag(FLAG_V, ((NOT(word(a) XOR word(fetched)) AND (word(a) XOR word(temp))) AND $0080) <> 0);

	// The negative flag is set to the most significant bit of the result
	SetFlag(FLAG_N, (temp AND $80) <> 0);

	// Load the result into the accumulator (it's 8-bit dont forget!)
	a := temp AND $00FF;

	// This instruction has the potential to require an additional clock cycle
	result := 1;
end;


// Instruction: Subtraction with Borrow In
// Function:    A := A - M - (1 - C)
// Flags Out:   C, V, N, Z
//
// Explanation:
// Given the explanation for ADC above, we can reorganise our data
// to use the same computation for addition, for subtraction by multiplying
// the data by -1, i.e. make it negative
//
// A := A - M - (1 - C)  ->  A := A + -1 * (M - (1 - C))  ->  A := A + (-M + 1 + C)
//
// To make a signed positive number negative, we can invert the bits and add 1
// (OK, I lied, a little bit of 1 and 2s complement :P)
//
//  5 := 00000101
// -5 := 11111010 + 00000001 := 11111011 (or 251 in our 0 to 255 range)
//
// The range is actually unimportant, because if I take the value 15, and add 251
// to it, given we wrap around at 256, the result is 10, so it has effectively
// subtracted 5, which was the original intention. (15 + 251) % 256 := 10
//
// Note that the equation above used (1-C), but this got converted to + 1 + C.
// This means we already have the +1, so all we need to do is invert the bits
// of M, the data(!) therfore we can simply add, exactly the same way we did
// before.

function Tolc6502._SBC() : byte;
var
  value : word;
begin
	fetch();

	// Operating in 16-bit domain to capture carry out

	// We can invert the bottom 8 bits with bitwise xor
	value := word(fetched) XOR $00FF;

	// Notice this is exactly the same as addition from here!
	temp := word(a) + value + word(GetFlag(FLAG_C));
	SetFlag(FLAG_C, (temp AND $FF00) <> 0);
	SetFlag(FLAG_Z, ((temp AND $00FF) = 0));
	SetFlag(FLAG_V, ((temp XOR word(a)) AND (temp XOR value) AND $0080) <> 0);
	SetFlag(FLAG_N, (temp AND $0080) <> 0);
	a := temp AND $00FF;
	result := 1;
end;

// OK! Complicated operations are done! the following are much simpler
// and conventional. The typical order of events is:
// 1) Fetch the data you are working with
// 2) Perform calculation
// 3) Store the result in desired place
// 4) Set Flags of the status register
// 5) Return if instruction has potential to require additional
//    clock cycle


// Instruction: Bitwise Logic _AND
// Function:    A := A AND M
// Flags Out:   N, Z
function Tolc6502._AND() : byte;
begin
	fetch();
	a := a AND fetched;
	SetFlag(FLAG_Z, a = $00);
	SetFlag(FLAG_N, (a AND $80) <> 0);
	result := 1;
end;


// Instruction: Arithmetic Shift Left
// Function:    A := C <- (A SHL 1) <- 0
// Flags Out:   N, Z, C
function Tolc6502._ASL() : byte;
begin
	fetch();
	temp := word(fetched) SHL 1;
	SetFlag(FLAG_C, (temp AND $FF00) > 0);
	SetFlag(FLAG_Z, (temp AND $00FF) = $00);
	SetFlag(FLAG_N, (temp AND $80) <> 0);
	if (lookup[opcode].addrenum = TAddrMode._IMP) then
		a := temp AND $00FF
	else
		write(addr_abs, temp AND $00FF);
	result := 0;
end;


// Instruction: Branch if Carry Clear
// Function:    if(C = 0) pc := address
function Tolc6502._BCC() : byte;
begin
	if (GetFlag(FLAG_C) = 0) then
	begin
		Inc(cycles);
		addr_abs := pc + addr_rel;

		if((addr_abs AND $FF00) <> (pc AND $FF00)) then
			Inc(cycles);

		pc := addr_abs;
	end;
	result := 0;
end;


// Instruction: Branch if Carry Set
// Function:    if(C = 1) pc := address
function Tolc6502._BCS() : byte;
begin
	if (GetFlag(FLAG_C) = 1) then
	begin
		Inc(cycles);
		addr_abs := pc + addr_rel;

		if ((addr_abs AND $FF00) <> (pc AND $FF00)) then
			Inc(cycles);

		pc := addr_abs;
	end;
	result := 0;
end;


// Instruction: Branch if Equal
// Function:    if(Z = 1) pc := address
function Tolc6502._BEQ() : byte;
begin
	if (GetFlag(FLAG_Z) = 1) then
	begin
		Inc(cycles);
		addr_abs := pc + addr_rel;

		if ((addr_abs AND $FF00) <> (pc AND $FF00)) then
			Inc(cycles);

		pc := addr_abs;
	end;
	result := 0;
end;

function Tolc6502._BIT() : byte;
begin
	fetch();
	temp := a AND fetched;
	SetFlag(FLAG_Z, (temp AND $00FF) = $00);
	SetFlag(FLAG_N, (fetched AND (1 SHL 7)) <> 0);
	SetFlag(FLAG_V, (fetched AND (1 SHL 6)) <> 0);
	result := 0;
end;


// Instruction: Branch if Negative
// Function:    if(N = 1) pc := address
function Tolc6502._BMI() : byte;
begin
	if (GetFlag(FLAG_N) = 1) then
	begin
		Inc(cycles);
		addr_abs := pc + addr_rel;

		if ((addr_abs AND $FF00) <> (pc AND $FF00)) then
			Inc(cycles);

		pc := addr_abs;
	end;
	result := 0;
end;


// Instruction: Branch if Not Equal
// Function:    if(Z = 0) pc := address
function Tolc6502._BNE() : byte;
begin
	if (GetFlag(FLAG_Z) = 0) then
	begin
		Inc(cycles);
		addr_abs := pc + addr_rel;

		if ((addr_abs AND $FF00) <> (pc AND $FF00)) then
			Inc(cycles);

		pc := addr_abs;
	end;
	result := 0;
end;


// Instruction: Branch if Positive
// Function:    if(N = 0) pc := address
function Tolc6502._BPL() : byte;
begin
	if (GetFlag(FLAG_N) = 0) then
	begin
		Inc(cycles);
		addr_abs := pc + addr_rel;

		if ((addr_abs AND $FF00) <> (pc AND $FF00)) then
			Inc(cycles);

		pc := addr_abs;
	end;
	result := 0;
end;

// Instruction: Break
// Function:    Program Sourced Interrupt
function Tolc6502._BRK() : byte;
begin
	Inc(pc);

	SetFlag(FLAG_I, true);
	write($0100 + stkp, (pc SHR 8) AND $00FF);
	Dec(stkp);
	write($0100 + stkp, pc AND $00FF);
	Dec(stkp);

	SetFlag(FLAG_B, true);
	write($0100 + stkp, status);
	Dec(stkp);
	SetFlag(FLAG_B, false);

	pc := word(read($FFFE)) OR (word(read($FFFF)) SHL 8);
	result := 0;
end;


// Instruction: Branch if Overflow Clear
// Function:    if(V = 0) pc := address
function Tolc6502._BVC() : byte;
begin
	if (GetFlag(FLAG_V) = 0) then
	begin
		Inc(cycles);
		addr_abs := pc + addr_rel;

		if ((addr_abs AND $FF00) <> (pc AND $FF00)) then
			Inc(cycles);

		pc := addr_abs;
	end;
	result := 0;
end;


// Instruction: Branch if Overflow Set
// Function:    if(V = 1) pc := address
function Tolc6502._BVS() : byte;
begin
	if (GetFlag(FLAG_V) = 1) then
	begin
		Inc(cycles);
		addr_abs := pc + addr_rel;

		if ((addr_abs AND $FF00) <> (pc AND $FF00)) then
			Inc(cycles);

		pc := addr_abs;
	end;
	result := 0;
end;


// Instruction: Clear Carry Flag
// Function:    C := 0
function Tolc6502._CLC() : byte;
begin
	SetFlag(FLAG_C, false);
	result := 0;
end;


// Instruction: Clear Decimal Flag
// Function:    D := 0
function Tolc6502._CLD() : byte;
begin
	SetFlag(FLAG_D, false);
	result := 0;
end;


// Instruction: Disable Interrupts / Clear Interrupt Flag
// Function:    I := 0
function Tolc6502._CLI() : byte;
begin
	SetFlag(FLAG_I, false);
	result := 0;
end;


// Instruction: Clear Overflow Flag
// Function:    V := 0
function Tolc6502._CLV() : byte;
begin
	SetFlag(FLAG_V, false);
	result := 0;
end;

// Instruction: Compare Accumulator
// Function:    C <- A >= M      Z <- (A - M) = 0
// Flags Out:   N, C, Z
function Tolc6502._CMP() : byte;
begin
	fetch();
	temp := word(a) - word(fetched);
	SetFlag(FLAG_C, a >= fetched);
	SetFlag(FLAG_Z, (temp AND $00FF) = $0000);
	SetFlag(FLAG_N, (temp AND $0080) <> 0);
	result := 1;
end;


// Instruction: Compare X Register
// Function:    C <- X >= M      Z <- (X - M) = 0
// Flags Out:   N, C, Z
function Tolc6502._CPX() : byte;
begin
	fetch();
	temp := word(x) - word(fetched);
	SetFlag(FLAG_C, x >= fetched);
	SetFlag(FLAG_Z, (temp AND $00FF) = $0000);
	SetFlag(FLAG_N, (temp AND $0080) <> 0);
	result := 0;
end;


// Instruction: Compare Y Register
// Function:    C <- Y >= M      Z <- (Y - M) = 0
// Flags Out:   N, C, Z
function Tolc6502._CPY() : byte;
begin
	fetch();
	temp := word(y) - word(fetched);
	SetFlag(FLAG_C, y >= fetched);
	SetFlag(FLAG_Z, (temp AND $00FF) = $0000);
	SetFlag(FLAG_N, (temp AND $0080) <> 0);
	result := 0;
end;


// Instruction: Decrement Value at Memory Location
// Function:    M := M - 1
// Flags Out:   N, Z
function Tolc6502._DEC() : byte;
begin
	fetch();
	temp := fetched - 1;
	write(addr_abs, temp AND $00FF);
	SetFlag(FLAG_Z, (temp AND $00FF) = $0000);
	SetFlag(FLAG_N, (temp AND $0080) <> 0);
	result := 0;
end;


// Instruction: Decrement X Register
// Function:    X := X - 1
// Flags Out:   N, Z
function Tolc6502._DEX() : byte;
begin
	Dec(x);
	SetFlag(FLAG_Z, x = $00);
	SetFlag(FLAG_N, (x AND $80) <> 0);
	result := 0;
end;


// Instruction: Decrement Y Register
// Function:    Y := Y - 1
// Flags Out:   N, Z
function Tolc6502._DEY() : byte;
begin
	Dec(y);
	SetFlag(FLAG_Z, y = $00);
	SetFlag(FLAG_N, (y AND $80) <> 0);
	result := 0;
end;


// Instruction: Bitwise Logic XOR
// Function:    A := A xor M
// Flags Out:   N, Z
function Tolc6502._EOR() : byte;
begin
	fetch();
	a := a XOR fetched;
	SetFlag(FLAG_Z, a = $00);
	SetFlag(FLAG_N, (a AND $80) <> 0);
	result := 1;
end;


// Instruction: Increment Value at Memory Location
// Function:    M := M + 1
// Flags Out:   N, Z
function Tolc6502._INC() : byte;
begin
	fetch();
	temp := fetched + 1;
	write(addr_abs, temp AND $00FF);
	SetFlag(FLAG_Z, (temp AND $00FF) = $0000);
	SetFlag(FLAG_N, (temp AND $0080) <> 0);
	result := 0;
end;


// Instruction: Increment X Register
// Function:    X := X + 1
// Flags Out:   N, Z
function Tolc6502._INX() : byte;
begin
	Inc(x);
	SetFlag(FLAG_Z, x = $00);
	SetFlag(FLAG_N, (x AND $80) <> 0);
	result := 0;
end;


// Instruction: Increment Y Register
// Function:    Y := Y + 1
// Flags Out:   N, Z
function Tolc6502._INY() : byte;
begin
	Inc(y);
	SetFlag(FLAG_Z, y = $00);
	SetFlag(FLAG_N, (y AND $80) <> 0);
	result := 0;
end;


// Instruction: Jump To Location
// Function:    pc := address
function Tolc6502._JMP() : byte;
begin
	pc := addr_abs;
	result := 0;
end;


// Instruction: Jump To Sub-Routine
// Function:    Push current pc to stack, pc := address
function Tolc6502._JSR() : byte;
begin
	Dec(pc);

	write($0100 + stkp, (pc SHR 8) AND $00FF);
	Dec(stkp);
	write($0100 + stkp, pc AND $00FF);
	Dec(stkp);

	pc := addr_abs;
	result := 0;
end;


// Instruction: Load The Accumulator
// Function:    A := M
// Flags Out:   N, Z
function Tolc6502._LDA() : byte;
begin
	fetch();
	a := fetched;
	SetFlag(FLAG_Z, a = $00);
	SetFlag(FLAG_N, (a AND $80) <> 0);
	result := 1;
end;


// Instruction: Load The X Register
// Function:    X := M
// Flags Out:   N, Z
function Tolc6502._LDX() : byte;
begin
	fetch();
	x := fetched;
	SetFlag(FLAG_Z, x = $00);
	SetFlag(FLAG_N, (x AND $80) <> 0);
	result := 1;
end;


// Instruction: Load The Y Register
// Function:    Y := M
// Flags Out:   N, Z
function Tolc6502._LDY() : byte;
begin
	fetch();
	y := fetched;
	SetFlag(FLAG_Z, y = $00);
	SetFlag(FLAG_N, (y AND $80) <> 0);
	result := 1;
end;

function Tolc6502._LSR() : byte;
begin
	fetch();
	SetFlag(FLAG_C, (fetched AND $0001) <> 0);
	temp := fetched SHR 1;
	SetFlag(FLAG_Z, (temp AND $00FF) = $0000);
	SetFlag(FLAG_N, (temp AND $0080) <> 0);
	if (lookup[opcode].addrenum = TAddrMode._IMP) then
		a := temp AND $00FF
	else
		write(addr_abs, temp AND $00FF);
	result := 0;
end;

function Tolc6502._NOP() : byte;
begin
	// Sadly not all NOPs are equal, Ive added a few here
	// based on https://wiki.nesdev.com/w/index.php/CPU_unofficial_opcodes
	// and will add more based on game compatibility, and ultimately
	// I'd like to cover all illegal opcodes too
  case opcode of
	  $1C,
	  $3C,
	  $5C,
	  $7C,
	  $DC,
	  $FC:
		result := 1;
  else
    result := 0;
	end;
end;


// Instruction: Bitwise Logic OR
// Function:    A := A OR M
// Flags Out:   N, Z
function Tolc6502._ORA() : byte;
begin
	fetch();
	a := a OR fetched;
	SetFlag(FLAG_Z, a = $00);
	SetFlag(FLAG_N, (a AND $80) <> 0);
	result := 1;
end;


// Instruction: Push Accumulator to Stack
// Function:    A -> stack
function Tolc6502._PHA() : byte;
begin
	write($0100 + stkp, a);
	Dec(stkp);
	result := 0;
end;


// Instruction: Push Status Register to Stack
// Function:    status -> stack
// Note:        Break flag is set to 1 before push
function Tolc6502._PHP() : byte;
begin
	write($0100 + stkp, status OR FLAG_B OR FLAG_U);
	SetFlag(FLAG_B, false);
	SetFlag(FLAG_U, false);
	Dec(stkp);
	result := 0;
end;


// Instruction: Pop Accumulator off Stack
// Function:    A <- stack
// Flags Out:   N, Z
function Tolc6502._PLA() : byte;
begin
	Inc(stkp);
	a := read($0100 + stkp);
	SetFlag(FLAG_Z, a = $00);
	SetFlag(FLAG_N, (a AND $80) <> 0);
	result := 0;
end;


// Instruction: Pop Status Register off Stack
// Function:    Status <- stack
function Tolc6502._PLP() : byte;
begin
	Inc(stkp);
	status := read($0100 + stkp);
	SetFlag(FLAG_U, true);
	result := 0;
end;

function Tolc6502._ROL() : byte;
begin
	fetch();
	temp := word(fetched SHL 1) OR GetFlag(FLAG_C);
	SetFlag(FLAG_C, (temp AND $FF00) <> 0);
	SetFlag(FLAG_Z, (temp AND $00FF) = $0000);
	SetFlag(FLAG_N, (temp AND $0080) <> 0);
	if (lookup[opcode].addrenum = TAddrMode._IMP) then
		a := temp AND $00FF
	else
		write(addr_abs, temp AND $00FF);
	result := 0;
end;

function Tolc6502._ROR() : byte;
begin
	fetch();
	temp := word(GetFlag(FLAG_C) SHL 7) OR (fetched SHR 1);
	SetFlag(FLAG_C, (fetched AND $01) <> 0);
	SetFlag(FLAG_Z, (temp AND $00FF) = $00);
	SetFlag(FLAG_N, (temp AND $0080) <> 0);
	if (lookup[opcode].addrenum = TAddrMode._IMP) then
		a := temp AND $00FF
	else
		write(addr_abs, temp AND $00FF);
	result := 0;
end;

function Tolc6502._RTI() : byte;
begin
	Inc(stkp);
	status := read($0100 + stkp);
	status := status AND (NOT FLAG_B);
	status := status AND (NOT FLAG_U);

	Inc(stkp);
	pc := word(read($0100 + stkp));
	Inc(stkp);
	pc := pc OR (word(read($0100 + stkp)) SHL 8);
	result := 0;
end;

function Tolc6502._RTS() : byte;
begin
	Inc(stkp);
	pc := word(read($0100 + stkp));
	Inc(stkp);
	pc := pc OR (word(read($0100 + stkp)) SHL 8);

	Inc(pc);
	result := 0;
end;




// Instruction: Set Carry Flag
// Function:    C := 1
function Tolc6502._SEC() : byte;
begin
	SetFlag(FLAG_C, true);
	result := 0;
end;


// Instruction: Set Decimal Flag
// Function:    D := 1
function Tolc6502._SED() : byte;
begin
	SetFlag(FLAG_D, true);
	result := 0;
end;


// Instruction: Set Interrupt Flag / Enable Interrupts
// Function:    I := 1
function Tolc6502._SEI() : byte;
begin
	SetFlag(FLAG_I, true);
	result := 0;
end;


// Instruction: Store Accumulator at Address
// Function:    M := A
function Tolc6502._STA() : byte;
begin
	write(addr_abs, a);
	result := 0;
end;


// Instruction: Store X Register at Address
// Function:    M := X
function Tolc6502._STX() : byte;
begin
	write(addr_abs, x);
	result := 0;
end;


// Instruction: Store Y Register at Address
// Function:    M := Y
function Tolc6502._STY() : byte;
begin
	write(addr_abs, y);
	result := 0;
end;


// Instruction: Transfer Accumulator to X Register
// Function:    X := A
// Flags Out:   N, Z
function Tolc6502._TAX() : byte;
begin
	x := a;
	SetFlag(FLAG_Z, x = $00);
	SetFlag(FLAG_N, (x AND $80) <> 0);
	result := 0;
end;


// Instruction: Transfer Accumulator to Y Register
// Function:    Y := A
// Flags Out:   N, Z
function Tolc6502._TAY() : byte;
begin
	y := a;
	SetFlag(FLAG_Z, y = $00);
	SetFlag(FLAG_N, (y AND $80) <> 0);
	result := 0;
end;


// Instruction: Transfer Stack Pointer to X Register
// Function:    X := stack pointer
// Flags Out:   N, Z
function Tolc6502._TSX() : byte;
begin
	x := stkp;
	SetFlag(FLAG_Z, x = $00);
	SetFlag(FLAG_N, (x AND $80) <> 0);
	result := 0;
end;


// Instruction: Transfer X Register to Accumulator
// Function:    A := X
// Flags Out:   N, Z
function Tolc6502._TXA() : byte;
begin
	a := x;
	SetFlag(FLAG_Z, a = $00);
	SetFlag(FLAG_N, (a AND $80) <> 0);
	result := 0;
end;


// Instruction: Transfer X Register to Stack Pointer
// Function:    stack pointer := X
function Tolc6502._TXS() : byte;
begin
	stkp := x;
	result := 0;
end;


// Instruction: Transfer Y Register to Accumulator
// Function:    A := Y
// Flags Out:   N, Z
function Tolc6502._TYA() : byte;
begin
	a := y;
	SetFlag(FLAG_Z, a = $00);
	SetFlag(FLAG_N, (a AND $80) <> 0);
	result := 0;
end;


// This function captures illegal opcodes
function Tolc6502._XXX() : byte;
begin
	result := 0;
end;





///////////////////////////////////////////////////////////////////////////////
// HELPER FUNCTIONS

function Tolc6502.complete() : boolean;
begin
	result := cycles = 0;
end;

// This is the disassembly function. Its workings are not required for emulation.
// It is merely a convenience function to turn the binary instruction code into
// human readable form. Its included as part of the emulator because it can take
// advantage of many of the CPUs internal operations to do this.
procedure Tolc6502.disassemble(nStart : word; nStop : word);
var
  sInst     : string;
  addr      : integer;
  value     : byte;
  lo        : byte;
  hi        : byte;
  line_addr : word;
  op        : byte;

  function hex(n : integer; d : byte) : string;
  var
    i : integer;
    s : string;
  begin
    s := StringOfChar('0', d);
  	for i := d - 1 downto 0 do
    begin
  		s[i+1] := '0123456789ABCDEF'[(n AND $F)+1];
      n := n SHR 4;
    end;
  	result := s;
  end;

begin
	addr      := nStart;
	value     := 0;
  lo        := 0;
  hi        := 0;
	line_addr := 0;

{$IFDEF FPC}
  mapLines := TAddressSourceMap.Create;
{$ELSE}
  mapLines := TDictionary<word, integer>.Create();
{$ENDIF}
  sourceDump := TStringList.Create;
  sourceDump.Capacity := 65535;

	// Starting at the specified address we read an instruction
	// byte, which in turn yields information from the lookup table
	// as to how many additional bytes we need to read and what the
	// addressing mode is. I need this info to assemble human readable
	// syntax, which is different depending upon the addressing mode

	// As the instruction is decoded, a std::string is assembled
	// with the readable output
	while (addr <= integer(nStop)) do
	begin
		line_addr := addr;

		// Prefix line with instruction address
		sInst := '$' + hex(addr, 4) + ': ';

		// Read instruction, and get its readable name
		op := TBus(bus).read(addr, true); Inc(addr);
		sInst := sInst + lookup[op].name + ' ';

		// Get oprands from desired locations, and form the
		// instruction based upon its addressing mode. These
		// routines mimmick the actual fetch routine of the
		// 6502 in order to get accurate data as part of the
		// instruction

		if (lookup[op].addrenum = TAddrMode._IMP) then
		begin
			sInst := sInst + ' {IMP}';
    end
		else if (lookup[op].addrenum = TAddrMode._IMM) then
		begin
			value := TBus(bus).read(addr, true); Inc(addr);
			sInst := sInst + '#$' + hex(value, 2) + ' {IMM}';
		end
		else if (lookup[op].addrenum = TAddrMode._ZP0) then
		begin
			lo := TBus(bus).read(addr, true); Inc(addr);
			hi := $00;
			sInst := sInst + '$' + hex(lo, 2) + ' {ZP0}';
		end
		else if (lookup[op].addrenum = TAddrMode._ZPX) then
		begin
			lo := TBus(bus).read(addr, true); Inc(addr);
			hi := $00;
			sInst := sInst + '$' + hex(lo, 2) + ', X {ZPX}';
		end
		else if (lookup[op].addrenum = TAddrMode._ZPY) then
		begin
			lo := TBus(bus).read(addr, true); Inc(addr);
			hi := $00;
			sInst := sInst + '$' + hex(lo, 2) + ', Y {ZPY}';
		end
		else if (lookup[op].addrenum = TAddrMode._IZX) then
		begin
			lo := TBus(bus).read(addr, true); Inc(addr);
			hi := $00;
			sInst := sInst + '($' + hex(lo, 2) + ', X) {IZX}';
		end
		else if (lookup[op].addrenum = TAddrMode._IZY) then
		begin
			lo := TBus(bus).read(addr, true); Inc(addr);
			hi := $00;
			sInst := sInst + '($' + hex(lo, 2) + '), Y {IZY}';
		end
		else if (lookup[op].addrenum = TAddrMode._ABS) then
		begin
			lo := TBus(bus).read(addr, true); Inc(addr);
			hi := TBus(bus).read(addr, true); Inc(addr);
			sInst := sInst + '$' + hex(word(hi SHL 8) OR lo, 4) + ' {ABS}';
		end
		else if (lookup[op].addrenum = TAddrMode._ABX) then
		begin
			lo := TBus(bus).read(addr, true); Inc(addr);
			hi := TBus(bus).read(addr, true); Inc(addr);
			sInst := sInst + '$' + hex(word(hi SHL 8) OR lo, 4) + ', X {ABX}';
		end
		else if (lookup[op].addrenum = TAddrMode._ABY) then
		begin
			lo := TBus(bus).read(addr, true); Inc(addr);
			hi := TBus(bus).read(addr, true); Inc(addr);
			sInst := sInst + '$' + hex(word(hi SHL 8) OR lo, 4) + ', Y {ABY}';
		end
		else if (lookup[op].addrenum = TAddrMode._IND) then
		begin
			lo := TBus(bus).read(addr, true); Inc(addr);
			hi := TBus(bus).read(addr, true); Inc(addr);
			sInst := sInst + '($' + hex(word(hi SHL 8) OR lo, 4) + ') {IND}';
		end
		else if (lookup[op].addrenum = TAddrMode._REL) then
		begin
			value := TBus(bus).read(addr, true); Inc(addr);
			sInst := sInst + '$' + hex(value, 2) + ' [$' + hex(addr + value, 4) + '] {REL}';
		end;

    //DDLH: Add the new source line to a list, but store the index of that new line
    // in a map, tied to the address in memory it came from.
    // Makes it easier to:
    // 1. get the code for a memory location
    // 2. easily get the code around that location without having to know how many cycles
    //    before or after in memory you'd have to be, you can simply go to the next/previous
    //    source line.
    mapLines.Add(line_addr, sourceDump.Add(sInst));
  end;
end;

end.


