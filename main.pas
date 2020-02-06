unit main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  Menus;

type

  { TForm1 }

  TForm1 = class(TForm)
    MainMenu1: TMainMenu;
    Memo1: TMemo;
    ParseMenuItem: TMenuItem;
    OpenDialog1: TOpenDialog;
    OpenMenuItem: TMenuItem;
    procedure FormShow(Sender: TObject);
    procedure ParseMenuItemClick(Sender: TObject);
    procedure OpenMenuItemClick(Sender: TObject);
  private
  public

  end;

var
  Form1: TForm1;
  // Format of an entry:
  // NAME=flashbase,flashsize,srambase,sramsize,eeprombase,eepromsize
  ControllerInfo: TStringList;

procedure ParseFile(FileName: string; SL: TStrings);

implementation

uses
  XMLRead, dom, parsingATDF, generatepascalunit;

{$R *.lfm}

type
  TMemoryMap = record
    flashbase, flashsize,
    srambase, sramsize,
    eeprombase, eepromsize: integer;
  end;

{ TForm1 }

procedure TForm1.OpenMenuItemClick(Sender: TObject);
begin
  if OpenDialog1.Execute then
  begin
    Self.Caption := ExtractFileName(OpenDialog1.FileName);
    ParseFile(OpenDialog1.FileName, Memo1.Lines);
  end;
end;

procedure TForm1.FormShow(Sender: TObject);
begin
  if (Application.ParamCount > 0) and FileExists(Application.Params[0]) then
  begin
    OpenDialog1.FileName :=  Application.Params[1];
    Self.Caption := ExtractFileName(OpenDialog1.FileName);
    ParseFile(Application.Params[1], Memo1.Lines);
  end;
end;

procedure TForm1.ParseMenuItemClick(Sender: TObject);
var
  info: TSearchRec;
  sourcepath, destpath, s, mculist: string;
  SL: TStringList;
begin
  if OpenDialog1.Execute then
  begin
    SL := TStringList.Create;
    ControllerInfo := TStringList.Create;
    mculist := 'CPU_UNITS=';

    sourcepath := IncludeTrailingPathDelimiter(ExtractFileDir(OpenDialog1.FileName));
    SetCurrentDir(sourcepath);
    destpath := sourcepath + 'pp/';
    if not DirectoryExists(destpath) then
      CreateDir(destpath);

    Memo1.Clear;
    if FindFirst(sourcepath + '*', faNormal, info) = 0 then
    repeat
      // Save empty file so that empty file indicates error during processing
      s := ExtractFileNameWithoutExt(info.Name);
      Memo1.Lines.Add(s);
      Self.Caption := s;
      Application.ProcessMessages;
      s := LowerCase(s);
      mculist := mculist + s + ' ';
      SL.Clear;
      SL.SaveToFile(destpath + s + '.pp');
      ParseFile(sourcepath + info.Name, SL);
      SL.SaveToFile(destpath + s + '.pp');
    until FindNext(info) <> 0;
    FindClose(info);
    SL.Free;
    ControllerInfo.Add(mculist);
    ControllerInfo.SaveToFile(destpath + 'cpuinfo.pas');
    FreeAndNil(ControllerInfo);
  end;
end;

function loadDeviceType(deviceName: string): string;
var
  sr: TSearchRec;
  s: string;
begin
  result := '';
  s := '../gcc/dev/';
  if not DirectoryExists(s) then // possibly work from nested subdirectory, so move up hierarchy
    s := '../'+s;
  s := s + lowercase(deviceName);
  s := IncludeTrailingPathDelimiter(ExpandFileName(s)) + '*';

  if FindFirst(s, faAnyFile or faDirectory, sr) = 0 then
  repeat
    s := ExtractFileName(sr.Name);
    if pos('avr', s) > 0 then
    begin
      result := s;
      Break;
    end;
  until FindNext(sr) <> 0;
  FindClose(sr);
end;

function unpackAddressSpaces(constref dev: TDevice): string;
var
  i, j: integer;
  memmap: TMemoryMap;
begin
  FillByte(memmap, SizeOf(memmap), 0);
  for i := 0 to High(dev.AddressSpaces) do
  begin
    if CompareText(dev.AddressSpaces[i].id, 'prog') = 0 then
    begin
      for j := 0 to High(dev.AddressSpaces[i].memorySegments) do
      begin
        if (CompareText(dev.AddressSpaces[i].memorySegments[j].aname, 'FLASH') = 0) or
           (CompareText(dev.AddressSpaces[i].memorySegments[j].aname, 'PROGMEM') = 0) then
        begin
          memmap.flashbase := dev.AddressSpaces[i].memorySegments[j].start;
          memmap.flashsize := dev.AddressSpaces[i].memorySegments[j].size;
          Break;
        end;
      end;
    end
    else if CompareText(dev.AddressSpaces[i].id, 'data') = 0 then
    begin
      for j := 0 to High(dev.AddressSpaces[i].memorySegments) do
      begin
        if (dev.AddressSpaces[i].memorySegments[j].aname = 'IRAM') or
           (dev.AddressSpaces[i].memorySegments[j].aname = 'INTERNAL_SRAM') then
        begin
          memmap.srambase := dev.AddressSpaces[i].memorySegments[j].start;
          memmap.sramsize := dev.AddressSpaces[i].memorySegments[j].size;
          Break;
        end
        else if CompareText(dev.AddressSpaces[i].memorySegments[j].aname, 'EEPROM') = 0 then
        begin
          memmap.eeprombase := dev.AddressSpaces[i].memorySegments[j].start;
          memmap.eepromsize := dev.AddressSpaces[i].memorySegments[j].size;
        end;
      end;
    end
    else if CompareText(dev.AddressSpaces[i].id, 'eeprom') = 0 then
    begin
      memmap.eeprombase := 0;
      memmap.eepromsize := dev.AddressSpaces[i].size;
    end;
  end;

  Result := format(',(controllertypestr:''%s'';controllerunitstr:''%s'';cputype:%s;fputype:fpu_soft;flashbase:%d;flashsize:%d;srambase:%d;sramsize:%d;eeprombase:%d;eepromsize:%d)',
            [UpperCase(dev.deviceName), UpperCase(dev.deviceName),
             'cpu_' + loadDeviceType(dev.deviceName),
             memmap.flashbase, memmap.flashsize,
             memmap.srambase, memmap.sramsize,
             memmap.eeprombase, memmap.eepromsize]);
end;

procedure ParseFile(FileName: string; SL: TStrings);
var
  Doc: TXMLDocument;
  device: TDevice;
  s: string;
begin
  ReadXMLFile(Doc, FileName);
  device := parseDevice(Doc.DocumentElement);
  FreeAndNil(Doc);

  if device.architechture = 'AVR8X' then
    generateUnitXFromATDFInfo(device, SL)
  else
    generateUnitFromATDFInfo(device, SL);

  if Assigned(ControllerInfo) then
  begin
    s := unpackAddressSpaces(device);
    ControllerInfo.Add(s);
  end;
end;

end.
