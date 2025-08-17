unit subarchinfo;

interface

type
  TSubarch = (
    avrtiny,
    avr1,
    avr2,
    avr25,
    avr3,
    avr31,
    avr35,
    avr4,
    avr5,
    avr51,
    avr6,
    avrxmega2,
    avrxmega3,
    avrxmega4,
    avrxmega5,
    avrxmega6,
    avrxmega7,
    avrunknown);

const
   subarchNames: array[TSubarch] of string = (
     'avrtiny',
     'avr1',
     'avr2',
     'avr25',
     'avr3',
     'avr31',
     'avr35',
     'avr4',
     'avr5',
     'avr51',
     'avr6',
     'avrxmega2',
     'avrxmega3',
     'avrxmega4',
     'avrxmega5',
     'avrxmega6',
     'avrxmega7',
     'avrunknown');

function stringToSubarch(s: string): TSubarch;

implementation

uses
  sysutils;

function stringToSubarch(s: string): TSubarch;
begin
  for Result := low(TSubarch) to high(TSubarch) do
    if CompareText(s, subarchNames[Result]) = 0 then
      Break;
end;

end.

