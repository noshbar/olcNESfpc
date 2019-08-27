# olcNES - Object Pascal version
NES Emulator, and Tutorial Video Code - ported to Object Pascal.
[![IMAGE ALT TEXT HERE](https://img.youtube.com/vi/8XmxKPJDGU0/0.jpg)](https://www.youtube.com/watch?v=8XmxKPJDGU0)

This is a rather quick port of the [olc 6502 emulator](https://github.com/OneLoneCoder/olcNES) from C++ to Object Pascal, or more specifically, FreePascal in Lazarus form.
While I made an effort to cater for what I remember from Delphi, minor work will have to be done to get it to work.

I tried to keep everything as similar to the original C++ code, but had to make a few changes here and there to get things to work, such as:

* adding underscores to the operation functions, due to e.g., `AND` being a reserved word.
* I could not for the life of me figure out how to compare the addresses of method functions, so added a gross enum to state what method was being used
* having never used Generics before, I didn't know how to duplicate the C++ `std::map` procedure of 1. find item in map 2. enumerate from that item towards the end of the map.
So instead I store a map of memory_line:source_line, and store the disassembled source into a stringlist, which I reference with source_line, and can move freely before and after it.

As with the source of this conversation, it's not optimised in the slightest, more a straight forward example of how everything 6502 works.

# License (OLC-3)
Copyright 2018, 2019 OneLoneCoder.com

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