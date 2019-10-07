unit generatepascalunit;

interface

uses
  parsingATDF, classes;

procedure generateUnitFromATDFInfo(constref device: TDevice; var SL: TStrings);
procedure generateUnitXFromATDFInfo(constref device: TDevice; var SL: TStrings);

implementation

uses
  sysutils, math;

const
  // 's' clashes with attiny40 PSR
  setPrefix = 's';

  // 'm' clashes at90pwm316 MUBBR
  constMaskPrefix = 'c';

type
  // avr8 style declarations are per register
  TReg = record
    aname,
    caption: string;
    address,
    size,
    mask: integer;
    bitFields: TBitFields;  // add bit fields in sorted order
  end;
  TSortedRegs = array of TReg;

  // avr8X style declarations are per register group (record)
  TRegGroup = record
    aname,
    atype: string;
    offset: integer;
  end;
  TSortedRegGroups = array of TRegGroup;

var
  IDList: TStringList;  // list used to check uniqueness of identifier names

// Check if ID is unique, append _ to make unique
// then add to IDList
function AddUniqueID(s: string): string;
var
  i: integer;
begin
  if s <> '' then
  begin
    i := IDList.IndexOf(s);
    while not (i = -1) do
    begin
      s := s + '_';
      i := IDList.IndexOf(s);
    end;
    IDList.Add(s);
  end;
  Result := s;
end;

function findPeriphModule(constref device: TDevice; nameInModule, nameInRegGroup: string): PPeriphRegGroup;
var
  i, j, k: integer;
begin
  Result := nil;

  i := 0;
  while (i < length(device.PeriphModules)) and (device.PeriphModules[i].aname <> nameInModule) do
    inc(i);

  if i = length(device.PeriphModules) then exit;

  j := 0;
  while (j < length(device.PeriphModules[i].periphInstances)) and (device.PeriphModules[i].periphInstances[j].aname <> nameInRegGroup) do
    inc(j);

  if j = length(device.PeriphModules[i].periphInstances) then exit;

  k := 0;
  while (k < length(device.PeriphModules[i].periphInstances[j].RegGroup)) and
        (device.PeriphModules[i].periphInstances[j].RegGroup[k].nameInModule <> nameInRegGroup) do
    inc(k);

  if k = length(device.PeriphModules[i].periphInstances[j].RegGroup) then exit;

  Result := @(device.PeriphModules[i].periphInstances[j].RegGroup[k]);
end;

procedure insertBitfield(constref source: TBitField; var dest: TBitFields);
var
  i, j, len: integer;
begin
  len := Length(dest)+1;
  SetLength(dest, len);

  // Locate spot after next smallest entry
  i := 0;
  while (i < len-1) and (source.mask >= dest[i].mask) do
    inc(i);

  // Potentially just after previous duplicate
  if (i > 0) and (source.aname = dest[i-1].aname) then
  begin
    SetLength(dest, len-1);  // remove added field
    exit;
  end;

  j := len-1;
  while (j > i) do
  begin
    dest[j] := dest[j-1];
    dec(j);
  end;

  // now i should point to new available slot
  dest[i] := source;
end;

function sortedRegisters(constref device: TDevice): TSortedRegs;
var
  i, j, k, l, reg_offset, len: integer;
  reg: ^TReg;
  prg: PPeriphRegGroup;
  portChar: char;
  bitFieldList: TStringList;
  s: string;
begin
  bitFieldList := TStringList.Create;

  i := 0;
  while (i < length(device.AddressSpaces)) and (device.AddressSpaces[i].aname <> 'data') do
    inc(i);

  j := 0;
  while (j < Length(device.AddressSpaces[i].memorySegments)) and
        (device.AddressSpaces[i].memorySegments[j].type_ <> 'io') do
    inc(j);

  SetLength(Result, device.AddressSpaces[i].memorySegments[j].start + device.AddressSpaces[i].memorySegments[j].size);

  for i := 0 to High(device.Modules) do
  begin
    for j := 0 to High(device.Modules[i].registerGroups) do
    begin
      prg := findPeriphModule(device,
                              device.Modules[i].aname,
                              device.Modules[i].registerGroups[j].aname);
      // ATtiny80 has TC3 under modules but not peripherals...
      if prg = nil then
        continue;

      if prg^.addressSpace = 'data' then
        for k := 0 to High(device.Modules[i].registerGroups[j].registers) do
        begin
          reg := @device.Modules[i].registerGroups[j].registers[k];
          reg_offset := device.Modules[i].registerGroups[j].registers[k].offset;

          Result[reg_offset].address := reg_offset + prg^.offset;  // TODO: What about module offset?
          Result[reg_offset].aname := reg^.aname;
          Result[reg_offset].caption := reg^.caption;
          Result[reg_offset].size := reg^.size;
          Result[reg_offset].mask := reg^.mask;

          if length(device.Modules[i].registerGroups[j].registers[k].bitFields) > 0 then
          begin
            for l := 0 to length(device.Modules[i].registerGroups[j].registers[k].bitFields)-1 do
            begin
              s := device.Modules[i].registerGroups[j].registers[k].bitFields[l].aname;
              if bitFieldList.IndexOf(s) = -1 then  // hack to prevent duplicate identifier names from at90usb646
              begin
                bitFieldList.Add(s);
                insertBitfield(device.Modules[i].registerGroups[j].registers[k].bitFields[l], Result[reg_offset].bitFields);
              end;
            end;
          end
          else if pos('PORT', reg^.aname) = 1 then  // check for PORTA, PORTD etc.
          begin
            len := Length(Result[reg_offset].aname);
            portChar := Result[reg_offset].aname[len];
            for l := 0 to 7 do
              if ((1 shl l) and device.Modules[i].registerGroups[j].registers[k].mask) > 0 then
              begin
                len := length(Result[reg_offset].bitFields);
                SetLength(Result[reg_offset].bitFields, len+1);
                Result[reg_offset].bitFields[len].aname := 'P' + portChar + IntToStr(l);
                Result[reg_offset].bitFields[len].mask := (1 shl l);
              end;
          end;
        end;
    end;
  end;

  FreeAndNil(bitFieldList);
end;

procedure processBitField(constref bitField: TBitField; constref List: TStrings; const indentSpaces: byte; const maskOnly: boolean = false);
var
  i, bitsInMask: integer;
  idx, bmp, padding, s1, s2: string;
begin
  idx := 'bp';
  bmp := 'bm';
  SetLength(padding, indentSpaces);
  FillChar(padding[1], indentSpaces, ' ');

  // check if single bit value
  if bitField.mask in [1, 2, 4, 8, 16, 32, 64, 128] then
  begin
    if not maskOnly then
    begin
      s1 := format('%s = $%.2x;',
                  [bitField.aname + idx, round(log2(bitField.mask))]);
    end;

    if bitField.caption = '' then
      s2 := format('%s = $%.2x;',
                    [bitField.aname + bmp, bitField.mask])
    else
      s2 := format('%s = $%.2x;  // %s',
                    [bitField.aname + bmp, bitField.mask, bitField.caption]);

    if maskOnly then
      List.Add(padding + s2)
    else
      List.Add(padding + s1 + '  ' + s2);
  end
  else if not maskOnly then// expand mask into several bit definitions
  begin
    bitsInMask := -1;     // identify lowest bit label
    for i := 0 to 7 do
      if ((1 shl i) and bitField.mask) > 0 then
      begin
        inc(bitsInMask);  // zero based
        if bitField.caption = '' then
          List.Add(format(padding + '%s = $%.2x;',
            [bitField.aname + IntToStr(bitsInMask + bitField.lsb) + idx, i]))
        else
          List.Add(format(padding + '%s = $%.2x;  // %s',
            [bitField.aname + IntToStr(bitsInMask + bitField.lsb) + idx, i, bitField.caption]));
      end;
  end;
end;

function indexOfBitFieldInValuesGroup(const bitFieldName: string; constref valuegroups: TValueGroups): integer;
var
  i: integer;
begin
  Result := -1;
  i := 0;
  if bitFieldName <> '' then
  begin
    while (i < length(valuegroups)) and ((CompareText(bitFieldName, valuegroups[i].aname) <> 0)) do
      inc(i);

    if i < length(valuegroups) then
      Result := i;
  end;
end;

procedure processValueGroup(constref ValueGroup: TValueGroup; const bitFieldOffset: integer; constref List: TStrings);
var
  i: integer;
  s: string;
begin
 // Compose value name as [bitfield name]_[value name]
  i := pos('_', ValueGroup.aname);
  if i > 0 then
    s := copy(ValueGroup.aname, i+1, 255) + '_'
  else
    s := '';

  for i := 0 to high(ValueGroup.values) do
  begin
    // Ensure const name doesn't start with a numerical character
    if (s = '') and (valueGroup.values[i].aname[1] in ['0'..'9']) then
      s := '_';
    List.Add(format('    %s = $%.2x;',
      [s + valueGroup.values[i].aname, valueGroup.values[i].value shl bitFieldOffset]));
  end;
end;

procedure processBitFieldAsMask(constref bitField: TBitField; constref valuegroups: TValueGroups; constref List: TStrings; const indentSpaces: byte; const maskOnly: boolean = false);
var
  i, bitsInMask, mask, lsb: integer;
  idx, bmp, padding, s1, s2: string;
begin
  // GCC convention
  idx := 'bp';
  bmp := 'bm';
  SetLength(padding, indentSpaces);
  FillChar(padding[1], indentSpaces, ' ');

  // List of values are supplied
  if bitField.values <> '' then
  begin
    i := indexOfBitFieldInValuesGroup(bitField.values, valuegroups);
    if i > -1 then
    begin
      // Find LSB position of value mask
      lsb := 0;
      mask := bitField.mask;
      while (mask > 0) and ((mask and 1) = 0) do
      begin
       inc(lsb);
       mask := mask shr 1;
      end;
      // mask is now the largest possible valid value
      // Check last value in value-group against mask
      if valuegroups[i].values[high(valuegroups[i].values)].value > mask then
        List.Add('// **** Inconsistency between mask and values for ' +bitField.aname);

      List.Add(padding + '// ' + bitField.values); // perhaps strip module prefix?
      // Add generic bit mask for whole value group
      // Some registers have multiple modes - this seems to be encoded in the values-group name
      // as MODULE_MODE_BITFIELDNAME
      // Strip MODULE_ then MODE_BITFIELDNAME ensures a unique name
      // Also work for standard case which is labeled MODULE_BITFIELDNAME
      //List.Add(padding + bitField.aname+'mask = $' + IntToHex(bitField.mask, 2) + ';');
      s1 := Copy(bitField.values, pos('_', bitField.values)+1, 255);
      List.Add(padding + s1+'mask = $' + IntToHex(bitField.mask, 2) + ';');
      processValueGroup(valuegroups[i], lsb, List);
    end
    else
      List.Add('Unexpectedly no matching value-group for "' + bitField.values + '"');
  end
  else
  begin
   // Single bits may share a name with a register - not compatible with Pascal
   // Need to add something to ensure uniqueness
    List.Add(padding + '// ' + bitField.caption);
    // check if single bit value
    if bitField.mask in [1, 2, 4, 8, 16, 32, 64, 128] then
    begin
      s1 := format('%s = $%.2x;',
                   [bitField.aname + bmp, bitField.mask]);

      if not maskOnly then
      begin
        s2 := format('%s = $%.2x;',
                    [bitField.aname + idx, round(log2(bitField.mask))]);
      end;

      if maskOnly then
        List.Add(padding + s1)
      else
        List.Add(padding + s1 + '  ' + s2);
    end
    else  // expand mask into several bit definitions
    begin
      bitsInMask := -1;     // identify lowest bit label
      for i := 0 to 7 do
        if ((1 shl i) and bitField.mask) > 0 then
        begin
          inc(bitsInMask);  // zero based
          s1 := format('%s = $%.2x;',
                [bitField.aname + IntToStr(bitsInMask + bitField.lsb) + bmp, 1 shl i]);

          if not maskOnly then
          begin
            s2 := format(padding + '%s = $%.2x;',
                  [bitField.aname + IntToStr(bitsInMask + bitField.lsb) + idx, i]);
            List.Add(padding + s1 + '  ' + s2);
          end
          else
            List.Add(padding + s1);
        end;
    end;
  end;
end;

// Plain style var declaration per register
// var
//  PORTB: byte absolute $25;  // Port B Data Register
// with bit field declarations
// const
//  PB0 = 0;
procedure generateDeclarationsOpt1(constref device: TDevice; var List: TStrings);
var
  i, j, k, bitsInMask: integer;
  sortedRegs: TSortedRegs;
  r: TReg;
  b: TBitField;
  previousVar: boolean;
  comment: string;
begin
  sortedRegs := sortedRegisters(device);

  previousVar := false;
  for i := 0 to length(sortedRegs)-1 do
  begin
    r := sortedRegs[i];
    if r.aname <> '' then
    begin
      if not previousVar then
      begin
        List.Add('var');
        previousVar := true;
      end;

      if r.caption <> '' then
        comment := '  // ' + r.caption
      else
        comment := '';

      if r.size = 1 then
        List.Add(format('  %s: byte absolute $%.2x;%s', [r.aname, r.address, comment]))
      else if r.size = 2 then
      begin
        List.Add(format('  %s: word absolute $%.2x;%s', [r.aname, r.address, comment]));
        List.Add(format('  %sL: byte absolute $%.2x;', [r.aname, r.address]));
        List.Add(format('  %sH: byte absolute $%.2x;', [r.aname, r.address+1]));
      end
      else if r.size = 4 then
      begin
        List.Add(format('  %s: dword absolute $%.2x;%s', [r.aname, r.address, comment]));
        List.Add(format('  %sL: word absolute $%.2x;', [r.aname, r.address]));
        List.Add(format('  %sH: word absolute $%.2x;', [r.aname, r.address+2]));
      end
      else
      raise Exception.Create('Unexpected register size:: $' + IntToHex(r.size, 2));

      // Bit fields
      if length(r.bitFields) > 0 then
      begin
        List.Add('const');
        previousVar := false;
        for j := 0 to high(r.bitFields) do
          processBitField(r.bitFields[j], List, 2);
      end;
    end;
  end;
end;

// Conventional RTL style declarations
// var
//  PORTA: byte absolute $22;  // Port A Data Register
procedure generateStandardDeclarationFromRegisterInfo(constref reg: TReg; var  typeStr, varStr, constStr: string);
var
  j, k, bitsInMask, bitIndex, bitmask, bitnumber: integer;
  b: TBitField;
  comment, s: string;
  typeList, varList, constList: TStringList;
begin
  typeList := TStringList.Create;
  varList := TStringList.Create;
  constList := TStringList.Create;

  if reg.aname <> '' then
  begin
    if reg.caption <> '' then
      comment := '  // ' + reg.caption
    else
      comment := '';

    if reg.size = 1 then
      varList.Add(format('  %s: byte absolute $%.2x;%s', [reg.aname, reg.address, comment]))
    else if reg.size = 2 then
    begin
      varList.Add(format('  %s: word absolute $%.2x;%s', [reg.aname, reg.address, comment]));
      varList.Add(format('  %sL: byte absolute $%.2x;', [reg.aname, reg.address]));
      varList.Add(format('  %sH: byte absolute $%.2x;', [reg.aname, reg.address+1]));
    end
    else if reg.size = 4 then
    begin
      varList.Add(format('  %s: dword absolute $%.2x;%s', [reg.aname, reg.address, comment]));
      varList.Add(format('  %sL: word absolute $%.2x;', [reg.aname, reg.address]));
      varList.Add(format('  %sH: word absolute $%.2x;', [reg.aname, reg.address+2]));
    end
    else
    raise Exception.Create('Unexpected register size:: $' + IntToHex(reg.size, 2));

    // Bit fields
    bitmask := 0;
    if length(reg.bitFields) > 0 then
    begin
      for j := 0 to length(reg.bitFields)-1 do
      begin
        b := reg.bitFields[j];
        if b.caption = '' then
          comment := ''
        else
          comment := '  // ' + b.caption;

        // if values has been assigned then there is a corresponding value-group to expand
        if reg.bitFields[j].values = '' then
        begin
          if (bitmask and reg.bitFields[j].mask) = 0 then
          begin
            // const name and mask name handled together
            s := AddUniqueID(b.aname);
            constList.Add(format('  %s = %d;  ' + constMaskPrefix + '%s = %d;%s',
                          [s, round(log2(b.mask)), s, b.mask, comment]));
            bitmask := bitmask or reg.bitFields[j].mask;
          end;
        end
        else // expand mask into several bit definitions
        begin
          bitsInMask := -1;     // identify lowest bit label
          for k := 0 to 7 do
            if ((1 shl k) and reg.bitFields[j].mask) > 0 then
            begin
              inc(bitsInMask);  // zero based
              bitnumber := bitsInMask + reg.bitFields[j].lsb;
              if (bitmask and (1 shl k)) = 0 then
              begin
                // const name and mask name handled together
                s := reg.bitFields[j].aname + IntToStr(bitnumber);
                s := AddUniqueID(s);
                constList.Add(format('  %s = %d;  ' + constMaskPrefix + '%s = %d;%s',
                  [s, k, s, 1 shl k, comment]));
                bitmask := bitmask or (1 shl k);
              end;
            end;
        end;
      end;
    end;
  end;

  if typeList.Count > 0 then
    if typeStr = '' then
      typeStr := TrimRight(typeList.Text)
    else
      typeStr := typeStr + typeList.LineBreak + TrimRight(typeList.Text);

  if varList.Count > 0 then
    if varStr = '' then
      varStr := TrimRight(varList.Text)
    else
      varStr := varStr + varList.LineBreak + TrimRight(varList.Text);

  if constList.Count > 0 then
    if constStr = '' then
      constStr := TrimRight(constList.Text)
    else
      constStr := constStr + constList.LineBreak + TrimRight(constList.Text);

  FreeAndNil(constList);
  FreeAndNil(varList);
  FreeAndNil(typeList);
end;

// Record type declaration of bit fields for ports
// type
//  TBitField = 0..1;
//  TPORTArec = bitpacked record
//    PA0, PA1, PA2, PA3,
//    PA4, PA5, PA6, PA7: TBitField;
//  end;
//var
//  PORTArec: TPORTArec absolute $22;
procedure generateRecordDeclarationFromRegisterInfo(constref reg: TReg; var typeStr, varStr, constStr: string);
var
  j, k, bitsInMask, dummycount: integer;
  s, recdef: string;
  typeList, varList, constList, bitList: TStringList;
  b: TBitField;
begin
  typeList  := TStringList.Create;
  varList  := TStringList.Create;
  constList  := TStringList.Create;
  bitList := TStringList.Create;

  if (reg.aname <> '') and (Length(reg.bitFields) > 0) and (reg.size = 1) then // no bitfields, use generic definitions
  begin
    // Sort Bits from 0 to 7
    bitlist.Clear;
    for j := 0 to length(reg.bitFields)-1 do
    begin
      b := reg.bitFields[j];

      if b.mask in [1, 2, 4, 8, 16, 32, 64, 128] then
      begin
        s := IntToStr(round(log2(b.mask))) + '=' + b.aname;
        bitlist.Add(s);
      end
      else // expand mask into several bit definitions
      begin
        bitsInMask := -1;     // identify lowest bit label
        for k := 0 to 7 do
          if ((1 shl k) and b.mask) > 0 then
          begin
            inc(bitsInMask);  // zero based
            s := IntToStr(k) + '=' + b.aname + IntToStr(bitsInMask + b.lsb);
            bitlist.Add(s);
          end;
      end;
    end;

    recdef := format('  T%srec = bitpacked record', [reg.aname]);
    dummycount := -1;
    for j := 0 to 7 do
    begin
      k := bitlist.IndexOfName(IntToStr(j));
      if k > -1 then
        recdef := recdef + bitList.LineBreak + '    ' + bitlist.ValueFromIndex[k] + ','
      else // skip missing bit,
      begin
        inc(dummycount);
        recdef := recdef + bitList.LineBreak + '    ReservedBit' + IntToStr(j) + ',';
      end;
    end;

    if recdef <> '' then  // remove last ,
    begin
      delete(recdef, length(recdef), 1);
      recdef := recdef + ': TBitField;' + bitList.LineBreak + '  end;';
      typeList.Add(recdef);
    end;

    varList.Add(format('  %srec: T%srec absolute $%.2x;', [reg.aname, reg.aname, reg.address]));
  end
  else if (reg.aname <> '') then
  begin
    // generic record declaration?
  end;

  if typeList.Count > 0 then
    if typeStr = '' then
      typeStr := TrimRight(typeList.Text)
    else
      typeStr := typeStr + typeList.LineBreak + TrimRight(typeList.Text);

  if varList.Count > 0 then
    if varStr = '' then
      varStr := TrimRight(varList.Text)
    else
      varStr := varStr + varList.LineBreak + TrimRight(varList.Text);

  if constList.Count > 0 then
    if constStr = '' then
      constStr := TrimRight(constList.Text)
    else
      constStr := constStr + constList.LineBreak + TrimRight(constList.Text);

  FreeAndNil(bitList);
  FreeAndNil(constList);
  FreeAndNil(varList);
  FreeAndNil(typeList);
end;

// Set style declaration of ports
// type
//  TPORTAset = bitpacked set of (sPA0, sPA1, sPA2, sPA3, sPA4, sPA5, sPA6, sPA7);
//var
//  PORTAset: TPORTAset absolute $22;
procedure generateSetDeclarationFromRegisterInfo(constref reg: TReg; var typeStr, varStr, constStr: string);
var
  j, k, bitsInMask: integer;
  s, sID, setdef: string;
  skippedbit: boolean;
  typeList, varList, constList, bitList: TStringList;
  b: TBitField;
begin
  typeList  := TStringList.Create;
  varList  := TStringList.Create;
  constList  := TStringList.Create;
  bitList := TStringList.Create;

  if (reg.aname <> '') and (Length(reg.bitFields) > 0) and (reg.size = 1) then // no bitfields, use generic definitions
  begin
    // Sort Bits from 0 to 7
    bitlist.Clear;
    for j := 0 to length(reg.bitFields)-1 do
    begin
      b := reg.bitFields[j];

      if b.mask in [1, 2, 4, 8, 16, 32, 64, 128] then
      begin
        s := IntToStr(round(log2(b.mask))) + '=' + b.aname;
        bitlist.Add(s);
      end
      else // expand mask into several bit definitions
      begin
        bitsInMask := -1;     // identify lowest bit label
        for k := 0 to 7 do
          if ((1 shl k) and b.mask) > 0 then
          begin
            inc(bitsInMask);  // zero based
            s := IntToStr(k) + '=' + b.aname + IntToStr(bitsInMask + b.lsb);
            bitlist.Add(s);
          end;
      end;
    end;

    skippedbit := false;
    setdef := format('  T%sset = bitpacked set of (', [reg.aname]);
    for j := 0 to 7 do
    begin
      s := '';
      sID := '';
      k := bitlist.IndexOfName(IntToStr(j));
      if k > -1 then
      begin
        //setdef := setdef + 'e';  // e prefix = set element prefix
        //s := 'e';
        if skippedbit then
        begin
          skippedbit := false;
          //setdef := setdef + bitlist.ValueFromIndex[k] + '=' +IntToStr(j) + ', '
          s := setPrefix + bitlist.ValueFromIndex[k];
          sID := '=' + IntToStr(j);
        end
        else
          //setdef := setdef + bitlist.ValueFromIndex[k] + ', ';
          s := setPrefix + bitlist.ValueFromIndex[k];

        s := AddUniqueID(s);
        setdef := setdef + s + sID + ', ';
      end
      else // skip missing bit,
      begin
        skippedbit := true;
      end;
    end;

    if setdef <> '' then
    begin
      delete(setdef, length(setdef)-1, 2);
      setdef := setdef + ');';
      typeList.Add(setdef);
    end;

    varList.Add(format('  %sset: T%sset absolute $%.2x;', [reg.aname, reg.aname, reg.address]));
  end;

  if typeList.Count > 0 then
    if typeStr = '' then
      typeStr := TrimRight(typeList.Text)
    else
      typeStr := typeStr + typeList.LineBreak + TrimRight(typeList.Text);

  if varList.Count > 0 then
    if varStr = '' then
      varStr := TrimRight(varList.Text)
    else
      varStr := varStr + varList.LineBreak + TrimRight(varList.Text);

  if constList.Count > 0 then
    if constStr = '' then
      constStr := TrimRight(constList.Text)
    else
      constStr := constStr + constList.LineBreak + TrimRight(constList.Text);

  FreeAndNil(bitList);
  FreeAndNil(constList);
  FreeAndNil(varList);
  FreeAndNil(typeList);
end;

// Combine standard, set and record definitions for each register.
procedure generateDeclarationsOpt2(constref device: TDevice; var List: TStrings);
var
  i, typedefcount: integer;
  sortedRegs: TSortedRegs;
  r: TReg;
  previousVar, previousType: boolean;
  s, sType, sVar, sConst: string;
begin
  IDList := TStringList.Create;
  IDList.CaseSensitive := false;
  typedefcount := 0;
  List.Add('{$bitpacking on}{$packset 1}{$packenum 1}');
  List.Add('type');
  List.Add('  TBitField = 0..1;');

  sortedRegs := sortedRegisters(device);

  previousVar := false;
  previousType := false;
  for i := 0 to length(sortedRegs)-1 do
  begin
    r := sortedRegs[i];
    if r.aname <> '' then
    begin
      sType := '';
      sVar := '';
      sConst := '';
      // First standard declarations so that conventional identifiers are used afap for the plain const names
      generateStandardDeclarationFromRegisterInfo(r, sType, sVar, sConst);
      generateSetDeclarationFromRegisterInfo(r, sType, sVar, sConst);
      generateRecordDeclarationFromRegisterInfo(r, sType, sVar, sConst);

      if (sType <> '') then
      begin
        if not previousType then
        begin
          List.Add('');
          List.Add('type');
          previousVar := false;
          previousType := true;
          Inc(typedefcount);
        end;

        List.Add(sType);
      end;

      if (sVar <> '') then
      begin
        if not previousVar then
        begin
          if not previousType then
            List.Add('');
          List.Add('var');
          previousVar := true;
          previousType := false;
        end;

        List.Add(sVar);
      end;

      if (sConst <> '') then
      begin
        List.Add('const');  // always add const identifier
        previousVar := false;
        previousType := false;
        List.Add(sConst);
      end;
    end
    else
      s := '';

    if s <> '' then
      List.Add(s);
  end;
  List.Add('  // typedefs = ' + IntToStr(typedefcount));
  FreeAndNil(IDList);
end;

// Combine register declarations with rest of unit information such as ISR declarations and including startup code
procedure generateUnitFromATDFInfo(constref device: TDevice; var SL: TStrings);
var
  jmpInstr: string;  // jmp/rjmp depending on target capability
  startInc: string;  // start.inc/start_noram.inc depending on target RAM
  i: integer;
  pgmsize, sramsize: integer;
  cl, vl: TStringList;
begin
  cl := TStringList.Create;
  vl := TStringList.Create;
  try
    // device capability checks required later on
    pgmsize := 0;
    sramsize := 0;
    i := 0;
    while i < length(device.AddressSpaces) do
    begin
      if device.AddressSpaces[i].aname = 'prog' then
        pgmsize := device.AddressSpaces[i].size;
      if device.AddressSpaces[i].aname = 'data' then
        sramsize := device.AddressSpaces[i].size;

      inc(i);
    end;

    if pgmsize > 8192 then jmpInstr := 'jmp' else jmpInstr := 'rjmp';
    if sramsize > 0 then startInc := 'start' else startInc := 'start_noram';

    SL.Clear;
    SL.Add('unit ' + device.deviceName + ';');
    SL.Add(#13#10'{$goto on}'#13#10'interface'#13#10);

    generateDeclarationsOpt1(device, SL);
    //generateDeclarationsOpt2(device, SL);

    if pgmsize > 8192 then
      SL.Add(#13#10'implementation'#13#10#13#10'{$i avrcommon.inc}'#13#10)
    else
      SL.Add(#13#10'implementation'#13#10'{$define RELBRANCHES}'#13#10'{$i avrcommon.inc}'#13#10);

    for i := 0 to High(device.Interrupts) do
    begin
      if device.Interrupts[i].index <> 0 then
        SL.Add(format('procedure %s_ISR; external name ''%s_ISR''; // Interrupt %d %s',
            [device.Interrupts[i].name, device.Interrupts[i].name,
             device.Interrupts[i].index, device.Interrupts[i].caption]));
    end;

    SL.Add(#13#10'procedure _FPC_start; assembler; nostackframe;'#13#10'label'#13#10'  _start;'#13#10 +
                    'asm'#13#10'  .init'#13#10'  .globl _start'#13#10#13#10'  '+jmpInstr+' _start');

    for i := 0 to High(device.Interrupts) do
      if device.Interrupts[i].index <> 0 then
        SL.Add('  '+jmpInstr+' ' + device.Interrupts[i].name + '_ISR');

    SL.Add(#13#10'  {$i '+startInc+'.inc}'#13#10);
    for i := 0 to High(device.Interrupts) do
      if device.Interrupts[i].index <> 0 then
        SL.Add('  .weak ' + device.Interrupts[i].name + '_ISR');

    SL.Add('');
    for i := 0 to High(device.Interrupts) do
      if device.Interrupts[i].index <> 0 then
        SL.Add('  .set ' + device.Interrupts[i].name + '_ISR' + ', Default_IRQ_handler');

    SL.Add('end;');
    SL.Add('');
    SL.Add('end.');
  finally
    FreeAndNil(cl);
    FreeAndNil(vl);
  end;
end;

// Generate variable decarations for register records at absolute offsets
// var
//  VPORTA: TVPORT absolute $0000;
procedure sortedRegGroupsX(constref device: TDevice; SL: TStrings);
var
  i, j, k: integer;
  pms: TPeriphModules;
  rg: TPeriphRegGroup;
  typ, ref: string;
  sortedList: TStringList;
begin
  pms := device.PeriphModules;
  if length(pms) = 0 then
  begin
    Exception.Create('Error - no peripheral modules registered for device');
    exit;
  end;

  sortedList := TStringList.Create;
  for i := 0 to high(pms) do
  begin
    typ := pms[i].aname;
    for j := 0 to high(pms[i].periphInstances) do
    begin
      for k := 0 to high(pms[i].periphInstances[j].RegGroup) do
      begin
        rg := pms[i].periphInstances[j].RegGroup[k];
        // AVR8 controllers have separate memory spaces, only generate
        // declarations for data space
        if (device.architechture = 'AVR8X') or (rg.addressSpace = 'data') then
        begin
          ref := format('  %s: T%s absolute $%.4X;',[rg.aname, typ, rg.offset]);
          sortedList.Add(IntToHex(rg.offset, 4) + '=' + ref);
        end;
      end;
    end;
  end;

  sortedList.Sort;
  SL.Add('var');
  for i := 0 to sortedList.Count-1 do
    SL.Add(sortedList.ValueFromIndex[i]);

  sortedList.Free;
end;

// Escape reserved Pascal words to prevent compilation errors
function EscapeReservedWord(s: string): string;
const
  reservedwords: array[0..1] of string = ('IN', 'OUT');
var
  i: integer;
begin
  result := s;
  i := 0;

  while (i < length(reservedwords)) and (s <> reservedwords[i]) do
    inc(i);

  if (i < length(reservedwords)) then
    result := result + '_';
end;


procedure convertToRegisterGroupFormat(device: TDevice);
var
  i, j, k, regGroupOffset, tmpOffset: integer;
  tmpRegGroup: TRegisterGroup;
  prg: PPeriphRegGroup;
begin
  for i := 0 to high(device.Modules) do
  begin
    for j := 0 to high(device.Modules[i].registerGroups) do
    begin
      tmpRegGroup := device.Modules[i].registerGroups[j];
      regGroupOffset := MaxInt;
      for k := 0 to high(tmpRegGroup.registers) do
      begin
        tmpOffset := tmpRegGroup.registers[k].offset;
        if tmpOffset > 0 then
          regGroupOffset := min(regGroupOffset, tmpOffset);
      end;

      // Set register offset relative to register group offset
      for k := 0 to high(tmpRegGroup.registers) do
      begin
        tmpOffset := tmpRegGroup.registers[k].offset;
        if tmpOffset > 0 then
          tmpRegGroup.registers[k].offset := tmpOffset - regGroupOffset
        else
        begin
          writeln('!');
        end;
      end;

      // Update peripheral module offset to be compatible with avr8X format
      prg := findPeriphModule(device,
                              device.Modules[i].aname,
                              device.Modules[i].registerGroups[j].aname);

      prg^.offset := regGroupOffset;
    end;
  end;
end;

function findBitFieldOffsetByValues(constref m: TModule; values: string): integer;
var
  i, j, k: integer;
  tmp: uint16;
  found: boolean;
begin
  result := -1;
  i := 0;
  found := false;
  while (i <= high(m.registerGroups)) and not found do
  begin
    j := 0;
    while (j <= high(m.registerGroups[i].registers)) and not found do
    begin
      k := 0;
      while (k <= high(m.registerGroups[i].registers[j].bitFields)) and not found do
      begin
        found := CompareText(m.registerGroups[i].registers[j].bitFields[k].values, values) = 0;
        if found then
        begin
          tmp := m.registerGroups[i].registers[j].bitFields[k].mask;
          if tmp = 0 then  // older avr's use lsb to indicate start of multi-bit fields
            tmp := m.registerGroups[i].registers[j].bitFields[k].lsb
          else
          begin
            result := 0;
            while (tmp > 0) and ((tmp and 1) = 0) do
            begin
              tmp := tmp shr 1;
              inc(Result);
            end;
          end;
        end;
        inc(k);
      end;
      inc(j);
    end;
    inc(i);
  end;
end;

// Generate record style decalarations for register groups
// type
// TRSTCTRL = record //Reset controller
//   RSTFR: byte;  //Reset Flags
//   SWRR: byte;  //Software Reset
// end;
procedure generateDeclarationsOptX(constref device: TDevice; var List: TStrings);
var
  i, j, k, bitsInMask, tmp2, tmp, tmp3: integer;
  sortedRegs: TSortedRegs;
  r: TRegister;
  b: TBitField;
  previousType, foundBottomReg, isUniqueBitField: boolean;
  s, comment, type_: string;
  pm: TPeriphModule;
  m: TModule;
  reverseList, bitFieldList: TStringList;
  RegGroupArray: array of string;
begin
  if not Assigned(device.Modules) then
    exit;
  previousType := false;

  // Experimental, doesn't really work satisfactory
  if device.architechture = 'AVR8' then
    convertToRegisterGroupFormat(device);

  reverseList := TStringList.Create;
  bitFieldList := TStringList.Create;
  for i := 0 to High(device.Modules) do
  begin
    m := device.Modules[i];
    if not Assigned(m.registerGroups) then
      Continue;
    type_ := ': T'+m.aname+';';
    for j := 0 to High(m.registerGroups) do
    begin
      if not previousType then
      begin
        List.Add('type');
        previousType := true;
      end;

      if m.registerGroups[j].class_ = '' then
      begin
        // Declare normal record as object so that const declarations can be encapsulated inside object
        List.Add('  T' + m.registerGroups[j].aname + ' = object //'+m.registerGroups[j].caption);
        SetLength(RegGroupArray, m.registerGroups[j].size);
        for k := 0 to high(RegGroupArray) do
          RegGroupArray[k] := '';

        bitFieldList.Add('  const');
        for k := 0 to high(m.registerGroups[j].registers) do
        begin
          r := m.registerGroups[j].registers[k];
          // Check if entry exceeds stated size (e.g. USERROW module which states size=32 but entries run to 63)
          if (r.offset + r.size) > length(RegGroupArray) then
            SetLength(RegGroupArray, r.offset + r.size);

          if r.caption = '' then
            comment := ''
          else
            comment := '  //' + r.caption;

          s := EscapeReservedWord(r.aname);
          case r.size of
            1: RegGroupArray[r.offset] := '    '+s+': byte;'+comment;
            2:
            begin
              RegGroupArray[r.offset] := '    '+s+': word;'+comment;
              RegGroupArray[r.offset+1] := '_';
            end;
            4:
            begin
              RegGroupArray[r.offset] := '    '+s+': dword;'+comment;
              RegGroupArray[r.offset+1] := '_';
              RegGroupArray[r.offset+2] := '_';
              RegGroupArray[r.offset+3] := '_';
            end;
            else
              List.Add('################ Error unexpected register byte size...');
          end;

          if (length(r.bitFields) > 0) then
          begin
            // Loop over bit fields
            // check if bit field name clashes with any other bit name from any previous register
            for tmp := 0 to high(r.bitFields) do
            begin
              isUniqueBitField := true;

              // loop over previous registers
              for tmp2 := k-1 downto 0 do
              begin
                if length(m.registerGroups[j].registers[tmp2].bitFields) > 0 then
                begin
                  for tmp3 := 0 to high(m.registerGroups[j].registers[tmp2].bitFields) do
                  begin
                    isUniqueBitField := r.bitFields[tmp].aname <> m.registerGroups[j].registers[tmp2].bitFields[tmp3].aname;
                    if not isUniqueBitField then
                      break;
                  end;
                  if not isUniqueBitField then
                    break;
                end;
              end;
              if isUniqueBitField then
                processBitFieldAsMask(r.bitFields[tmp], m.valueGroups, bitFieldList, 4, true);
            end;
          end;
        end;

        // Scan from bottom up, only add registers after first real register from bottom
        foundBottomReg := false;
        for k := high(RegGroupArray) downto 0 do
        begin
          if (RegGroupArray[k] = '') then
          begin
            if foundBottomReg then
             reverseList.Add('    Reserved'+IntToStr(k)+': byte;')
          end
          else if RegGroupArray[k] <> '_' then
          begin
            reverseList.Add(RegGroupArray[k]);
            foundBottomReg := true;
          end;
        end;

        // Flip order of registers
        for k := reverseList.Count-1 downto 0 do
          List.Add(reverseList[k]);

        reverseList.Clear;
        if bitFieldList.Count > 1 then // list always starts with 'const' as first item
          List.AddStrings(bitFieldList);

        List.Add('  end;');
      end  // if
      else if m.registerGroups[j].class_ = 'union' then
      begin
        // Need to declare variant parts as record, so no const declarations included
        List.Add('  T' + m.registerGroups[j].aname + ' = record //'+m.registerGroups[j].caption);
        List.Add('    case byte of');
        for k := 0 to high(m.registerGroups[j].registers) do
          List.Add(format('      %d: (%s: T%s);', [k, m.registerGroups[j].registers[k].aname, m.registerGroups[j].registers[k].caption]));
        List.Add('  end;');
      end
      else
        List.Add('Error unexpected class type: '+m.registerGroups[j].class_);

      List.Add('');
      bitFieldList.Clear;
    end;
  end;

  reverseList.Free;
  bitFieldList.Free;

  // Add generic pin definitions
  List.Add('');
  List.Add('const');
  for i := 0 to 7 do
    List.Add(format(' Pin%didx = %d;  Pin%dbm = %d;', [i, i, i, 1 shl i]));
  List.Add('');

  sortedRegGroupsX(device, List);
end;

// Combine register declarations with rest of unit information such as ISR declarations and including startup code
// Similar to generateUnitFromATDFInfo, except for calling a different register declaration function
// Could actually use a single function and just specify which declaration style to use...
procedure generateUnitXFromATDFInfo(constref device: TDevice; var SL: TStrings);
var
  jmpInstr: string;  // jmp/rjmp depending on target capability
  startInc: string;  // start.inc/start_noram.inc depending on target RAM
  i, prevID: integer;
  pgmsize, sramsize: integer;
  cl, vl: TStringList;
begin
  cl := TStringList.Create;
  vl := TStringList.Create;
  try
    // device capability checks required later on
    pgmsize := 0;
    sramsize := 0;
    i := 0;
    while i < length(device.AddressSpaces) do
    begin
      if device.AddressSpaces[i].aname = 'prog' then
        pgmsize := device.AddressSpaces[i].size;
      if device.AddressSpaces[i].aname = 'data' then
        sramsize := device.AddressSpaces[i].size;

      inc(i);
    end;

    if pgmsize > 8192 then jmpInstr := 'jmp' else jmpInstr := 'rjmp';
    if sramsize > 0 then startInc := 'start' else startInc := 'start_noram';

    SL.Clear;
    SL.Add('unit ' + device.deviceName + ';');
    SL.Add(#13#10'{$goto on}'#13#10'interface'#13#10);

    //generateDeclarationsOpt1(device, SL);
    //generateDeclarationsOpt2(device, SL);
    generateDeclarationsOptX(device, SL);
    if pgmsize > 8192 then
      SL.Add(#13#10'implementation'#13#10#13#10'{$i avrcommon.inc}'#13#10)
    else
      SL.Add(#13#10'implementation'#13#10'{$define RELBRANCHES}'#13#10'{$i avrcommon.inc}'#13#10);

    prevID := 0;
    for i := 0 to High(device.Interrupts) do
    begin
      if device.Interrupts[i].index <> prevID then
        SL.Add(format('procedure %s_ISR; external name ''%s_ISR''; // Interrupt %d %s',
            [device.Interrupts[i].name, device.Interrupts[i].name,
             device.Interrupts[i].index, device.Interrupts[i].caption]))
      else
      SL.Add(format('//procedure %s_ISR; external name ''%s_ISR''; // Interrupt %d %s',
          [device.Interrupts[i].name, device.Interrupts[i].name,
           device.Interrupts[i].index, device.Interrupts[i].caption]));

      prevID := device.Interrupts[i].index;
    end;

    SL.Add(#13#10'procedure _FPC_start; assembler; nostackframe;'#13#10'label'#13#10'  _start;'#13#10 +
                    'asm'#13#10'  .init'#13#10'  .globl _start'#13#10#13#10'  '+jmpInstr+' _start');

    prevID := 0;
    for i := 0 to High(device.Interrupts) do
    begin
      if device.Interrupts[i].index <> prevID then
        SL.Add('  '+jmpInstr+' ' + device.Interrupts[i].name + '_ISR')
      else
        SL.Add('//  '+jmpInstr+' ' + device.Interrupts[i].name + '_ISR');

      prevID := device.Interrupts[i].index;
    end;

    SL.Add(#13#10'  {$i '+startInc+'.inc}'#13#10);
    prevID := 0;
    for i := 0 to High(device.Interrupts) do
    begin
      if device.Interrupts[i].index <> prevID then
        SL.Add('  .weak ' + device.Interrupts[i].name + '_ISR')
      else
        SL.Add('//  .weak ' + device.Interrupts[i].name + '_ISR');

      prevID := device.Interrupts[i].index;
    end;

    SL.Add('');
    prevID := 0;
    for i := 0 to High(device.Interrupts) do
    begin
      if device.Interrupts[i].index <> prevID then
        SL.Add('  .set ' + device.Interrupts[i].name + '_ISR' + ', Default_IRQ_handler')
      else
        SL.Add('//  .set ' + device.Interrupts[i].name + '_ISR' + ', Default_IRQ_handler');

      prevID := device.Interrupts[i].index;
    end;

    SL.Add('end;');
    SL.Add('');
    SL.Add('end.');
  finally
    FreeAndNil(cl);
    FreeAndNil(vl);
  end;
end;

end.

