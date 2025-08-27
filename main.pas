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

procedure ParseFile(FileName: string; SL: TStrings);

implementation

uses
  XMLRead, dom, parsingATDF, generatepascalunit,
  subarchinfo, NaturalSortUnit;

{$R *.lfm}

type
  TMemoryMap = record
    flashbase, flashsize,
    srambase, sramsize,
    externalRAMbase, externalRAMsize,
    eeprombase, eepromsize,
    bootbase, bootsize: integer;
  end;

  TSubarchCheck = (scOK, scSubarchModified, scSubarchInconsistent);

var
  // Format of an entry:
  // NAME=flashbase,flashsize,srambase,sramsize,eeprombase,eepromsize
  ControllerInfo: TStringList;
  // Store list of controllers grouped per subarch
  subarchList: array[TSubarch] of TStringList;

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

function generateCPUInfo(const deviceName: string; const memmap: TMemoryMap;
  var subarch: TSubarch): string;
var
  subarchCheck: TSubarchCheck;
  flagged: boolean;
begin
  subarchCheck := scSubarchModified;
  // Fix subarch for known controllers
  case deviceName of
    'AT90USB82': subarch := avr25;          // 8 kB flash
    'ATMEGA8U2': subarch := avr25;          // 8 kB flash
    'ATXMEGA128A4U': subarch := avrxmega6;  // Does not support external RAM
    else
      subarchCheck := scOK;
  end;

  // Check that subarch and memory sizes are aligned
  case subarch of
    avr25, avr4: flagged := memmap.flashsize > 8192;
    avr35, avr5: flagged := (memmap.flashsize <= 8192) or (memmap.flashsize > 65536);
    avr51: flagged := memmap.flashsize <> 131072;
    avr6 : flagged := memmap.flashsize <= 131072;
    avrxmega2: flagged := (memmap.flashsize < 8192) or (memmap.flashsize > 65536);
    avrxmega3: flagged := memmap.flashsize + memmap.sramsize > 65536;
    avrxmega4: flagged := (memmap.flashsize < 65536) or (memmap.flashsize > 131072) or (memmap.sramsize > 65536);
    avrxmega5: flagged := (memmap.flashsize < 65536) or (memmap.flashsize > 131072) or (memmap.sramsize + memmap.externalRAMsize <= 65536);
    avrxmega6: flagged := (memmap.flashsize < 131072) or (memmap.sramsize > 65536);
    avrxmega7: flagged := (memmap.flashsize < 131072) or (memmap.sramsize + memmap.externalRAMsize <= 65536);
    else
      flagged := false;
  end;

  if flagged then
    subarchCheck := scSubarchInconsistent;

  if memmap.bootbase > 0 then
    Result := format(',(controllertypestr:''%s'';controllerunitstr:''%s'';cputype:%s;'+
            'fputype:fpu_soft;flashbase:%d;flashsize:%d;srambase:%d;sramsize:%d;'+
            'eeprombase:%d;eepromsize:%d;bootbase:%d;bootsize:%d)',
            [UpperCase(deviceName), UpperCase(deviceName),
             'cpu_' + subarchNames[subarch],
             memmap.flashbase, memmap.flashsize,
             memmap.srambase, memmap.sramsize,
             memmap.eeprombase, memmap.eepromsize,
             memmap.bootbase, memmap.bootsize])
  else
    Result := format(',(controllertypestr:''%s'';controllerunitstr:''%s'';cputype:%s;'+
            'fputype:fpu_soft;flashbase:%d;flashsize:%d;srambase:%d;sramsize:%d;'+
            'eeprombase:%d;eepromsize:%d)',
            [UpperCase(deviceName), UpperCase(deviceName),
            'cpu_' + subarchNames[subarch],
             memmap.flashbase, memmap.flashsize,
             memmap.srambase, memmap.sramsize,
             memmap.eeprombase, memmap.eepromsize]);

  if subarchCheck = scSubarchModified then
    Result := Result + '// Subarch modified to better match memory map';
  if subarchCheck = scSubarchInconsistent then
    Result := Result + '// Subarch appears inconsistent with memory map'#13#10;

  subarchList[subarch].Add(LowerCase(deviceName));
end;

procedure addAttiny28Info;
var
  memmap: TMemoryMap;
  subarch: TSubarch;
  s: string;
begin
  FillByte(memmap, SizeOf(memmap), 0);
  memmap.flashsize := 2048;
  subarch := avr1;
  s := generateCPUInfo('ATtiny28', memmap, subarch);
  ControllerInfo.Add(s);
end;

procedure TForm1.ParseMenuItemClick(Sender: TObject);
const
  attiny28Name = 'ATtiny28';
var
  info: TSearchRec;
  sourcepath, destpath, s, line: string;
  SL, mculist: TStringList;
  subarch: TSubarch;
  i: integer;
  attiny28Added: boolean;
begin
  if OpenDialog1.Execute then
  begin
    SL := TStringList.Create;
    mculist := TStringList.Create;
    mculist.Sorted := false;
    for subarch in TSubarch do
    begin
      subarchList[subarch] := TStringList.Create;
      subarchList[subarch].Sorted := false;
      subarchList[subarch].Duplicates := dupError;
    end;

    if not Assigned(ControllerInfo) then
    begin
      ControllerInfo := TStringList.Create;
      ControllerInfo.Sorted := false;
    end
    else
      ControllerInfo.Clear;

    ControllerInfo.Sorted := false;
    sourcepath := IncludeTrailingPathDelimiter(ExtractFileDir(OpenDialog1.FileName));
    SetCurrentDir(sourcepath);
    destpath := sourcepath + 'pp/';
    if not DirectoryExists(destpath) then
      CreateDir(destpath);

    if FindFirst(sourcepath + '*', faNormal, info) = 0 then
    repeat
      mculist.Add(ExtractFileNameOnly(info.Name));
    until FindNext(info) <> 0;
    FindClose(info);

    // Check if attiny28 is included, else add it
    if mculist.IndexOf(attiny28Name) < 0 then
    begin
      mcuList.Add(attiny28Name);
      attiny28Added := true;
    end
    else
      attiny28Added := false;

    NaturalSort(mculist, stNatural);

    // Add controller to tcontrollertype list
    for i := 0 to mculist.Count-1 do
      ControllerInfo.Add('ct_' + LowerCase(mculist[i]) + ',');

    Memo1.Clear;
    for i := 0 to mculist.Count-1 do
    begin
      // Save empty file so that empty file indicates error during processing
      Memo1.Lines.Add(mculist[i]);
      Self.Caption := Format('%s - %d of %d', [mculist[i], i+1, mculist.Count]);
      Application.ProcessMessages;
      s := LowerCase(mculist[i]);
      if attiny28Added and (CompareText(mculist[i], attiny28Name) = 0) then
        addAttiny28Info
      else
      begin
        SL.Clear;
        SL.SaveToFile(destpath + s + '.pp');
        ParseFile(sourcepath + mculist[i]+'.atdf', SL);
        SL.SaveToFile(destpath + s + '.pp');
      end;
    end;

    // Save subarch lists to MakeFile
    SL.Clear;
    for subarch in TSubarch do
    begin
      if subarchList[subarch].Count > 0 then
      begin
        s := 'ifeq ($(SUBARCH),'+subarchNames[subarch]+')'+LineEnding;
        line := 'CPU_UNITS=';
        for i := 0 to subarchList[subarch].Count-1 do
        begin
          if length(line) < 80 then
            line := line + subarchList[subarch][i] + ' '
          else
          begin
            s := s + line + '\'+LineEnding;
            line := '          ' + subarchList[subarch][i] + ' ';
          end;
        end;

        if length(line) > 10 then
          s := s + TrimRight(line);
        s := s + LineEnding + 'CPU_UNITS_DEFINED=1'+LineEnding+'endif';
        SL.Add(s);
      end;
      subarchList[subarch].Free;
    end;
    if SL.Count > 0 then
      SL.SaveToFile(destpath + 'MakeFile.txt');
    SL.Free;

    ControllerInfo.SaveToFile(destpath + 'cpuinfo.pas');
    FreeAndNil(ControllerInfo);
  end;
end;

function loadSubarch(deviceName: string): TSubArch;
var
  sr: TSearchRec;
  searchPath, s, stmp, lowerDeviceName: string;
  SL: TStringList;
  i, j: integer;
begin
  { Typical folder layout:
    gcc/dev/attiny44/avr25/ - folder with startup file and low level library
    gcc/dev/attiny44/device-specs - folder with device specification file

    Either read the subarch folder name to get subarch name,
    or search for -mmcu= in the spec file (specs-attiny44).

    avr1 controllers do not have the subarch folder, hence the specs file
    should be read if the subarch folder is not found. }

  Result := avrunknown;
  lowerDeviceName := LowerCase(deviceName);
  searchPath := '../gcc/dev/';
  if not DirectoryExists(searchPath) then // possibly work from nested subdirectory, so move up hierarchy
    searchPath := '../' + searchPath;
  searchPath := searchPath + lowerDeviceName;
  searchPath := IncludeTrailingPathDelimiter(ExpandFileName(searchPath));

  if FindFirst(searchPath+'*', faAnyFile or faDirectory, sr) = 0 then
  repeat
    s := ExtractFileName(sr.Name);
    if pos('avr', s) > 0 then
    begin
      Result := stringToSubarch(s);
      Break;
    end;
  until FindNext(sr) <> 0;
  FindClose(sr);

  if Result = avrunknown then
  begin
    searchPath := searchPath + 'device-specs/specs-' + lowerDeviceName;
    if FileExists(searchPath) then
    begin
      SL := TStringList.Create;
      SL.LoadFromFile(searchPath);
      stmp := SL.Text;
      SL.Free;
      i := pos('-mmcu=avr', stmp);
      if i > 0 then
      begin
        s := '';
        j := i + 9;
        while (stmp[j] in ['0'..'9', 'a'..'z']) do
        begin
          s := s + stmp[j];
          inc(j);
        end;

        case s of
          'tiny'  : Result := avrtiny;
          '1'     : Result := avr1;
          '2'     : Result := avr2;
          '25'    : Result := avr25;
          '3'     : Result := avr3;
          '31'    : Result := avr31;
          '35'    : Result := avr35;
          '4'     : Result := avr4;
          '5'     : Result := avr5;
          '51'    : Result := avr51;
          '6'     : Result := avr6;
          'xmega2': Result := avrxmega2;
          'xmega3': Result := avrxmega3;
          'xmega4': Result := avrxmega4;
          'xmega5': Result := avrxmega5;
          'xmega6': Result := avrxmega6;
          'xmega7': Result := avrxmega7;
        end;  // case s of
      end;  // if i > 0
    end;  // if FileExists(searchPath)
  end;  // if Result = avrunknown
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
        end
        else if CompareText(dev.AddressSpaces[i].memorySegments[j].aname, 'EEPROM') = 0 then
        begin
          Result.eeprombase := dev.AddressSpaces[i].memorySegments[j].start;
          Result.eepromsize := dev.AddressSpaces[i].memorySegments[j].size;
        end
        else if (CompareText(dev.AddressSpaces[i].memorySegments[j].aname, 'EXTERNAL_SRAM') = 0) or
                (CompareText(dev.AddressSpaces[i].memorySegments[j].aname, 'XRAM') = 0) then
        begin
          Result.externalRAMbase := dev.AddressSpaces[i].memorySegments[j].start;
          Result.externalRAMsize := dev.AddressSpaces[i].memorySegments[j].size;
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
    s := generateCPUInfo(device.deviceName, memmap, subarch);
    ControllerInfo.Add(s);
  end;
end;

end.
