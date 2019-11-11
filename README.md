# importATDF
Generate Pascal style units from AVR ATDF definition files.

Two options available in main form:
## Open file
This will open a single file and convert it into a Pascal unit according to the options as configured in generatepascalunit.pas.
The generated code will be displayed in the text memo area.

## Parse folder
This will iterate through _all_ files in the selected folder and try to parse each file as an ATDF compatible XML file. 
The output units will be saved in a sub folder _pp_. Extra information is collected from the _gcc_ folder (this tool assume the
default folder layout of the atpack archive) to determine the Gnu tools compatible subarchitecture type. Also a cpuinfo.pas file is created
with memory layout and subarchitecture information required in the compiler's avr/cpuinfo.pas unit, and also a _CPU_UNITS_ line in case
one needs to update the make files in RTL if a new subarchitecture is added.
