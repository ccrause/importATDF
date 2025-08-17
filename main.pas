unit main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LazFileUtils, Forms, Controls, Graphics, Dialogs, StdCtrls,
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
  XMLRead, dom, parsingATDF, generatepascalunit,
  subarchinfo;

{$R *.lfm}

type
  TMemoryMap = record
    flashbase, flashsize,
    srambase, sramsize,
    eeprombase, eepromsize,
    bootbase, bootsize: integer;
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
    ControllerInfo.Sorted := true;
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

function loadSubarch(deviceName: string): TSubArch;
var
  sr: TSearchRec;
  s: string;
begin
  result := avrunknown;
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
      //result := s;
      Break;
    end;
  until FindNext(sr) <> 0;
  FindClose(sr);

  Result := stringToSubarch(s);
end;

function unpackAddressSpaces(constref dev: TDevice): TMemoryMap;
var
  i, j: integer;
begin
  FillByte(Result, SizeOf(Result), 0);
  for i := 0 to High(dev.AddressSpaces) do
  begin
    if CompareText(dev.AddressSpaces[i].id, 'prog') = 0 then
    begin
      for j := 0 to High(dev.AddressSpaces[i].memorySegments) do
      begin
        if (CompareText(dev.AddressSpaces[i].memorySegments[j].aname, 'FLASH') = 0) or
           (CompareText(dev.AddressSpaces[i].memorySegments[j].aname, 'PROGMEM') = 0) or
           // XMega devices, this will exclude the bootsection from the flash size
           (CompareText(dev.AddressSpaces[i].memorySegments[j].aname, 'APP_SECTION') = 0) then
        begin
          Result.flashbase := dev.AddressSpaces[i].memorySegments[j].start;
          Result.flashsize := dev.AddressSpaces[i].memorySegments[j].size;
        end

        // typ FLASH + BOOT_SECTION_1 .. BOOT_SECTION_4
        else if (CompareText(dev.AddressSpaces[i].memorySegments[j].aname, 'BOOT_SECTION_4') = 0) or
                // XMega devices
                (CompareText(dev.AddressSpaces[i].memorySegments[j].aname, 'BOOT_SECTION') = 0) or
           // or simply FLASH + BOOT_SECTION_1
           (length(dev.AddressSpaces[i].memorySegments) < 5) and
            ((CompareText(dev.AddressSpaces[i].memorySegments[j].aname, 'BOOT_SECTION_1') = 0)) then
        begin
          Result.bootbase := dev.AddressSpaces[i].memorySegments[j].start;
          Result.bootsize := dev.AddressSpaces[i].memorySegments[j].size;
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
          Result.srambase := dev.AddressSpaces[i].memorySegments[j].start;
          Result.sramsize := dev.AddressSpaces[i].memorySegments[j].size;
          Break;
        end
        else if CompareText(dev.AddressSpaces[i].memorySegments[j].aname, 'EEPROM') = 0 then
        begin
          Result.eeprombase := dev.AddressSpaces[i].memorySegments[j].start;
          Result.eepromsize := dev.AddressSpaces[i].memorySegments[j].size;
        end;
      end;
    end
    else if CompareText(dev.AddressSpaces[i].id, 'eeprom') = 0 then
    begin
      Result.eeprombase := 0;
      Result.eepromsize := dev.AddressSpaces[i].size;
    end;
  end;
end;

function generateCPUInfo(constref dev: TDevice; const memmap: TMemoryMap;
  subarch: TSubarch): string;
var
  flagged: boolean = false;
begin
  // Check that subarch and memory size is aligned
  if ((subarch = avr35) and (memmap.flashsize <= 8192)) or
     ((subarch = avr5) and (memmap.flashsize <= 8192)) or
     ((subarch = avr51) and (memmap.flashsize < 131072)) or
     ((subarch = avr6) and (memmap.flashsize < 262144)) then
    flagged := true;

  if memmap.bootbase > 0 then
    Result := format(',(controllertypestr:''%s'';controllerunitstr:''%s'';cputype:%s;'+
            'fputype:fpu_soft;flashbase:%d;flashsize:%d;srambase:%d;sramsize:%d;'+
            'eeprombase:%d;eepromsize:%d;bootbase:%d:bootsize:%d)',
            [UpperCase(dev.deviceName), UpperCase(dev.deviceName),
             'cpu_' + subarchNames[subarch],
             memmap.flashbase, memmap.flashsize,
             memmap.srambase, memmap.sramsize,
             memmap.eeprombase, memmap.eepromsize,
             memmap.bootbase, memmap.bootsize])
  else
    Result := format(',(controllertypestr:''%s'';controllerunitstr:''%s'';cputype:%s;'+
            'fputype:fpu_soft;flashbase:%d;flashsize:%d;srambase:%d;sramsize:%d;'+
            'eeprombase:%d;eepromsize:%d)',
            [UpperCase(dev.deviceName), UpperCase(dev.deviceName),
            'cpu_' + subarchNames[subarch],
             memmap.flashbase, memmap.flashsize,
             memmap.srambase, memmap.sramsize,
             memmap.eeprombase, memmap.eepromsize]);

  if flagged then
    Result := '>> ' + Result;
end;

procedure ParseFile(FileName: string; SL: TStrings);
var
  Doc: TXMLDocument;
  device: TDevice;
  memmap: TMemoryMap;
  s: string;
  subarch: TSubarch;
begin
  ReadXMLFile(Doc, FileName);
  device := parseDevice(Doc.DocumentElement);
  FreeAndNil(Doc);

  if (device.architechture = 'AVR8X') or
     (device.architechture = 'AVR8_XMEGA') then
    generateUnitXFromATDFInfo(device, SL)
  else
    generateUnitFromATDFInfo(device, SL);

  if Assigned(ControllerInfo) then
  begin
    memmap := unpackAddressSpaces(device);
    subarch := loadSubarch(device.deviceName);
    s := generateCPUInfo(device, memmap, subarch);
    ControllerInfo.Add(s);
  end;
end;

end.
