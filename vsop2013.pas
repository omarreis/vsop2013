unit vsop2013;       //---Delphi implementation of theory VSOP2013 ----------------\
 //-----------------//                                                              \
// This is Delphi port from original Fortran code by FRANCOU & SIMON                 \
// VSOP original files:                                                               \
//   ftp://ftp.imcce.fr/pub/ephem/planets/vsop2013/ephemerides/                        \
//   VSOP2013.m2000, VSOP2013.m1000, VSOP2013.p1000,VSOP2013.p2000,VSOP2013.p4000       \
//   Files are large 400 MB ASCII containing chebyshev polynomial 1st kind coeficients.  \
// There are 6 coefs/line ( 163 x )                                                      /
// The file header constains a table loc[,] of indexes into the coefs[] array           /
// Each planet has a number of coefs.                                                  /
//                                                                                    /
//  programmed by oMAR                                                               /
//  Source: github.com/omarreis/vsop2013                                            /
//---------------------------------------------------------------------------------+
//                                                                                  \
// History:                                                                          \-----------------\
//   jun 2020: Loose port v0.9 from Fortran by oMAR                                                     \
//     this code loads coefs from ASCII file and builds memory tables (instead of original BIN files)    \
//     note: Fortran language is '1 based' (array indexes all start with 1 (at least this code is) )      \
//   jul 2020: For reasons of max bundle size and load speed on mobile, I adopted a custom binary format  /
//             ( same solution as VSOP2013 authors, however w/ a different approach )                    /
//             This cut the database size from 400 MB to 130 MB :)                                      /
//-----------------------------------------------------------------------------------------------------/

interface

uses
  System.Math,           //NaN
  System.Classes,        //anonymous thread
  System.Math.Vectors,
  System.SysUtils,
  doubleVector3D;  // TVector3D_D - 3D vectors with Double components

const
  TXT_FILES_ROOT = './ephemerides/';
  NUM_COEFS=978;   // fixed ?
  NUM_FILES=6;     // one file for each 1500y period
  NUM_PLANETS=9;
  //
  // NUM_PERIODS=17122;

var
   // original file names. Each of these files is 400 MB !
   TXT_FILES : Array[1..NUM_FILES] of string = (
             'VSOP2013.m4000',
             'VSOP2013.m2000',
             'VSOP2013.m1000',
             'VSOP2013.p1000',
             'VSOP2013.p2000',
             'VSOP2013.p4000');

  PLANET_NAMES :Array[1..NUM_PLANETS] of string =
     //   1         2       3        4         5         6         7          8        9
     ('Mercury', 'Venus', 'Earth', 'Mars', 'Jupiter', 'Saturn', 'Uranus', 'Neptune', 'Pluto');

const
  jd2000=2451545.0;
  TestFileName='VSOP2013.p2000.bin';

type
   TDateRange=record  // epoch range covered by a given file
     ti,tf:Double;
   end;

var
   DATE_RANGES: Array[1..NUM_FILES] of TDateRange = (
        (ti:77294.5  ; tf: 625198.5 ),   // -4500 to -3000       VSOP2013.m4000
        (ti:625198.5 ; tf: 1173102.5),   // -3000 to -1500       VSOP2013.m2000
        (ti:1173102.5; tf: 1721006.5),   // -1500 to 0           VSOP2013.m1000
        (ti:1721006.5; tf: 2268910.5),   // 0 to +1500           VSOP2013.p1000
        (ti:2268910.5; tf: 2816814.5),   // +1500 to +3000       VSOP2013.p2000
        (ti:2816814.5; tf: 3364718.5) ); // +3000 to +4500       VSOP2013.p4000

type
  // binary file record format, for faster access and smaller files (than  text)
  // ( Android has a 150MB bundle size limit. VSOP2013.p2000 is 400MB!! )
  // binary file is 7840*17123 = 134,244,320 bytes. much better than original
  T_VSOP2013_binaryRec=packed record    // rec len = 7840 bytes
    case boolean of                     //  <----------------- don't mess with this. file records
       true:  ( t1,t2,delta:Double;     //header ( 1st record )
                nintv,ncoef:integer;
                loc:Array[1..3,1..9] of integer;
              );
       false: ( dj1,dj2:Double;         // each rec corresponds to a 32 period ( there are 17122 of these )
                coef:Array[1..NUM_COEFS] of Double;    // 978 coefs / period
              );
    end;

  // VSOP2013File loads a bin file and parses it into a Chebyshev Polynomial
  // indexing and evaluation machine.

  T_VSOP2013_LoadProgress=procedure( Sender:TObject; Percent:integer) of Object;

  T_VSOP2013_Period=class  // a 32.0 day VSOP2013 period
  private
  public
    dj1,dj2:Double;                       // bounds
    coef:Array[1..NUM_COEFS] of Double; // coefs
    Constructor Create;
    Destructor  Destroy; override;
    procedure   Reset;
  end;

  T_VSOP2013_File=class
  private
    fOnLoadProgress:T_VSOP2013_LoadProgress;
    fOnLoadTerminate: TNotifyEvent;
    procedure DoNotifyTerminate;
  public
    //header
    idf:integer;       // file identification.  Should read 2013
    t1,t2:Double;     // file time range in JD
    delta:Double;    // =32.0  number of days per period
    nintv:integer;  // =number of 32d periods ( formerly NUM_PERIODS )
    ncoef:integer; // =0978 number of Chebyshev coeficients / period

    loc:Array[1..3,1..9] of integer;     // indexes into coeficient arrays

    Periods:Array of T_VSOP2013_Period;  // dynamic allocated Periods array

    fLoaded:boolean;
    fFilename:String;

    Constructor Create;

    Destructor  Destroy; override;
    Procedure   Reset;
    // custom binary file format ahead..
    function    WriteBinaryFile(const aFilename:String):boolean;   // vsop2013.p2000.bin
    function    ReadBinaryFile(const aFilename:String):boolean;

    function    Read_ASCII_File(const aFilename:String):boolean;    //read vsop2013 ASCII file to memory
    Procedure   Threaded_Read_ASCII_File(const aFilename: String);

    Procedure   Threaded_ReadBinaryFile(const aFilename: String);

    function    calculate_coordinates(ip: integer; const jde:Double; var Position, Speed: TVector3D_D): boolean;
    function    getMetadata:String;
    property    OnLoadProgress:T_VSOP2013_LoadProgress read fOnLoadProgress  write fOnLoadProgress;
    property    OnLoadTerminate:TNotifyEvent           read fOnLoadTerminate write fOnLoadTerminate; //threaded loat terminate

  end;

var
  VSOP_File:T_VSOP2013_File=nil;     // global vsop2013 file. When created, stores coeffs in memory for fast calculations

implementation            // ------------------


{ T_VSOP2013_Period }

constructor T_VSOP2013_Period.Create;
begin
  inherited Create;
  Reset;
end;

destructor T_VSOP2013_Period.Destroy;
begin
  inherited;
end;

Procedure  T_VSOP2013_Period.Reset;  // zero vars
var i:integer;
begin
  dj1:=0;
  dj2:=0;
  for i :=1 to NUM_COEFS do coef[i]:=0;
end;

{ T_VSOP2013_File }

constructor T_VSOP2013_File.Create;
begin
  inherited Create;
  fLoaded:= false;

  Reset;
  fOnLoadProgress  := nil;
  fOnLoadTerminate := nil;
  fFilename:='';
end;

Procedure  T_VSOP2013_File.Reset;  // zero vars
var i,j:integer;
begin
  idf   := 0;
  t1    := 0;
  t2    := 0;
  delta := 0;
  nintv := 0;  // num of 32d periods. =size of Periods[]
  ncoef := 0;  // always 978 ??

  for i:=1 to 3 do for j:= 1 to 6 do loc[i,j]:=0;

  SetLength(Periods,0);  //clear previous, if any
end;

destructor T_VSOP2013_File.Destroy;
var i:integer;
begin
  for i:=1 to nintv do Periods[i].Free;
  SetLength(Periods,0);  //clear previous, if any

  inherited;
end;

Procedure T_VSOP2013_File.DoNotifyTerminate;
begin
  if Assigned(fOnLoadTerminate) then
      fOnLoadTerminate( Self );
end;

Procedure T_VSOP2013_File.Threaded_Read_ASCII_File(const aFilename:String);
var aThread:TThread;
begin
  aThread := TThread.CreateAnonymousThread(
    procedure
    begin
      try
        Read_ASCII_File(aFilename); // this takes some 15 secs to load
      except
        // ignore error ??
      end;
      if Assigned(fOnLoadTerminate) then
        TThread.Synchronize( aThread, DoNotifyTerminate );   // notify vsop2013 file loaded and parsed
    end
    );
  aThread.Start;
end;


Procedure   T_VSOP2013_File.Threaded_ReadBinaryFile(const aFilename: String);
var aThread:TThread;
begin
  aThread := TThread.CreateAnonymousThread(
    procedure
    begin
      try
        ReadBinaryFile(aFilename); // this takes some 15 secs to load
      except
        // ignore error ??
      end;
      if Assigned(fOnLoadTerminate) then
        TThread.Synchronize( aThread, DoNotifyTerminate );   // notify vsop2013 file loaded and parsed
    end
    );
  aThread.Start;
end;

// once loaded from original ASCII text, save smaller and faster binary format
function  T_VSOP2013_File.WriteBinaryFile(const aFilename:String):boolean;
var aRec:T_VSOP2013_binaryRec; i,j:integer; F:File of T_VSOP2013_binaryRec;
    aPeriod:T_VSOP2013_Period;
begin
  Result := false;
  if not fLoaded then exit;
  assignFile(F, aFilename );
  rewrite(F);

  //write header rec
  aRec.t1     := t1;
  aRec.t2     := t2;
  aRec.delta  := delta;
  aRec.nintv  := nintv;
  aRec.ncoef  := ncoef;
  for i:=1 to 3 do for j:=1 to 9 do aRec.loc[i,j] := loc[i,j];
  write(F,aRec);

  //write coeficient records
  for i:=1 to nintv do        // 17k
    begin
       aPeriod := Periods[i];
       aRec.dj1  := aPeriod.dj1;
       aRec.dj2  := aPeriod.dj2;
       for j := 1 to NUM_COEFS do aRec.coef[j] := aPeriod.coef[j];    // 978 coefs
       write(F,aRec);
    end;
  CloseFile(F);
  Result := true;
end;

// !! ReadBinaryFile() binary format is not the same as original VSOP2013 binaries.
// Use only .bin binaries generated by this object
function  T_VSOP2013_File.ReadBinaryFile(const aFilename:String):boolean;  // 'vsop2013.p2000.bin'
var aRec:T_VSOP2013_binaryRec; i,j:integer;
    F:File of T_VSOP2013_binaryRec;
    aPeriod:T_VSOP2013_Period;
begin
  Result := false;
  fFilename   := aFilename;       //
  assignFile(F, aFilename );
  System.Reset(F);
  read(F,aRec);    //read header rec
  t1     := aRec.t1;
  t2     := aRec.t2;
  delta  := aRec.delta;
  nintv  := aRec.nintv;
  ncoef  := aRec.ncoef;
  for i:=1 to 3 do for j:=1 to 9 do loc[i,j] := aRec.loc[i,j];
  //read coef records
  SetLength(Periods, nintv+1);          // alloc Periods[]
  for i:=1 to nintv do  // 17k of them
    begin
       aPeriod := T_VSOP2013_Period.Create;
       read(F, aRec);
       aPeriod.dj1 := aRec.dj1;
       aPeriod.dj2 := aRec.dj2;
       for j := 1 to NUM_COEFS do aPeriod.coef[j] := aRec.coef[j];
       Periods[i] := aPeriod;
    end;
  CloseFile(F);
  fLoaded := true;
  Result := true;
end;

function T_VSOP2013_File.Read_ASCII_File(const aFilename:String):boolean;  //read vsop2013 ASCII file to memory
var
  InFile:Textfile;
  S,ss:String;
  i,j,k,l,ix,prevPerc,aPerc:integer;
  aPeriod:T_VSOP2013_Period;
  sMant,sExp:String;
  aMant:Extended;
  aExp:integer;

begin
  If FileExists(aFileName) then
    begin
      fLoaded:= false;
      fFilename := aFilename;

      AssignFile(InFile,aFileName);
      try
        System.Reset(InFile);
      except
        raise Exception.Create('Error opening VSOP2013 file');
      end;

      prevPerc:=0;

      try
          // read vsop2013 file header
          //  2013
          //  2268910.5
          //  2816814.5
          //  32.0
          //  017122
          //  0978
          Readln(InFile,S);   idf   := StrToIntDef(S,0);
          Readln(InFile,S);   t1    := StrToFloatDef(S,0);
          Readln(InFile,S);   t2    := StrToFloatDef(S,0);
          Readln(InFile,S);   delta := StrToFloatDef(S,0);
          Readln(InFile,S);   nintv := StrToIntDef(S,0);   //number of intervals in this file
          Readln(InFile,S);   ncoef := StrToIntDef(S,0);

          if (idf<>2013)    or  //sanity test in header
             (nintv<>17122) or  //CHECK: are all these parameters fixed ??
             (ncoef<>978) then
              Raise Exception.Create('some error in file');

          for i:=1 to 3 do  //read loc[,] table
            begin
              Readln(InFile,S); //load line
              //        123456789.123456789.123456789.123456789.123456
              // tipo: '  0001 0337 0469 0637 0715 0781 0841 0895 0937'
              for j:=1 to NUM_PLANETS do
                begin
                  ss := Copy(S,2+5*(j-1), 5);   // there is one extra ' ' in the begining
                  loc[i,j] := StrToInt( ss );   //
                end;
            end;

          SetLength(Periods, nintv+1);          // alloc Periods[]

          // Periods[0] not used

          for i:=1 to nintv do  //  nintv = num of 32d periods ( =17122  1 based )
            begin
              aPeriod := T_VSOP2013_Period.Create;
              Readln(InFile,S);    // read period's jd1,jd2
              //       123456789.123456789.123
              // tipo '  2268910.5  2268942.5'
              ss := Copy(S,2,10);
              aPeriod.dj1 := StrToFloat(ss);
              ss := Copy(S,13,10);
              aPeriod.dj2 := StrToFloat(ss);

              for k := 1 to 163 do  // 163 lines of coefs
                begin
                  Readln(InFile,S);  //read coef line ( 6 coefs[] per line )
                  //       123456789.123456789:123456789T
                  // tipo '   -0.2536115476181505 +00  0.7009448388105931 -01  0.3144971753049469 -02 -0.1722448640420403 -03  0.8042649431438953 -06 -0.5011012033888969 -07'
                  for L := 0 to 5 do
                    begin
                      sMant := Copy(S, 3+L*24,20);   //mantissa
                      sExp  := Copy(S,23+L*24,4);   //exp
                      aMant := StrToFloat(sMant);
                      aExp  := StrToInt(sExp);
                      ix    := (k-1)*6+L+1;
                      aPeriod.coef[ ix ] := Power10( aMant,aExp );
                    end;
                end;

              Periods[i] := aPeriod;  //save loaded Period in dynamic array

              if Assigned(fOnLoadProgress) and (nintv<>0) then //load progress
                begin
                  aPerc := Trunc(i/nintv*100);
                  if (aPerc<>prevPerc) then  //report once per %
                    begin
                      fOnLoadProgress(Self,aPerc);
                      prevPerc := aPerc;
                    end;
                end;
            end;
          CloseFile(InFile);
          fLoaded := true;
      except
        CloseFile(InFile);  //close anyway
        raise Exception.Create('Error loading file');
      end;

    end
    else raise Exception.Create('File not found');
end;

const
  crlf=#13#10;

function  T_VSOP2013_File.getMetadata:String;  //returns file header information
var aFN:String; L:integer;
begin
  if fLoaded then
    begin
      aFN := fFilename;
      L:= aFN.Length;
      if (L>20) then aFN := '...'+ Copy(aFN,L-20,MAXINT);  //long filename...keep last part

      Result := 'file:'+aFN+crlf+
                'per:'  + Format('%5.1f',[t1])+' - '+Format('%5.1f',[t2])+crlf+
                'delta:'+ Format('%4.1f',[delta])+' nint:'+IntToStr(nintv);
                 // ncoef always 0978 ?
    end
    else begin
      Result := 'no data';
    end;
end;

//         Attempt to calculate the position and velocity for the specified planet.
//         :param ip: index of planet to perform calculation for
//         :param jde: the julian date
//         :return: an array of the X,Y,Z heliocentric position of the planet and  X',Y',Z' heliocentric velocity ( units au and au/d )
function T_VSOP2013_File.calculate_coordinates( ip:integer; const jde:Double; {out:} var Position,Speed:TVector3D_D):boolean;

var i,j,ifile,iper,iad,ncf,nsi,ik,iloc,jt,jp:integer;
    r1,rng:TDateRange;
    aPeriod:T_VSOP2013_Period;
    delta2,dj0:Double;
    x:Extended;
    tn:Array[1..20] of Extended; //Chebyshev factors
    r :Array[1..6] of Extended;  //planet pos and speed  vectors

begin
  result := false;

  // sanity check params
  if (ip < 1) or (ip > NUM_PLANETS) then
       raise Exception.Create('Invalid planet index');

  if (jde<t1) or (jde>t2) then  // file bounds
       raise Exception.Create('time out of range of the loaded file');

  // rng.ti:=0; rng.tf:=0;
  // ifile :=-1;
  //
  // for i:=1 to NUM_FILES do  //find the file
  //   begin
  //     r1 := DATE_RANGES[i];
  //     if (jde>=r1.ti) and (jde<r1.tf) then
  //       begin
  //         rng  := r1;
  //         ifile:=i;
  //         break;
  //       end;
  //   end;

  // if rng.ti=0 then
  //       raise Exception.Create('Invalid jde');

  iper := Trunc( (jde - t1) / delta)+1;  // calc 32d period index ( 1 based )

  if (iper>0) and (iper<=nintv) then
      begin
        aPeriod := Periods[iper];  //get period containing jde

        iad := loc[1,ip];   // initial term of the planet (index into coef[])
        ncf := loc[2,ip];   // number of Chebyshev coefs
        nsi := loc[3,ip];   // some delta divider ?

        delta2 := delta/nsi; // default delta is 32.0, but it can be divided by nsi ??

        ik := Trunc( (jde-aPeriod.dj1)/delta2 );
        if (ik=nsi) then ik := ik-1;
        iloc := iad + 6*ncf*ik;      // iloc = last term??
        dj0  := aPeriod.dj1 + ik*delta2;

        x    := 2.0*(jde-dj0)/delta2 - 1.0;  // Chebyshev time x in interval -1..+1
        // build Chebyshev terms
        tn[1] := 1.0;
        tn[2] := x;
        for i := 3 to ncf do tn[i] := 2.0*x*tn[i-1]-tn[i-2];
        // calculate planet pos & speed
        for i:=1 to 6 do        // 6 vars ( pos and speed)
          begin
            r[i]:=0;            // zero sum
            for j:=1 to ncf do  // loop ncf terms
              begin
                jp   := ncf-j+1;               //index into tn[], work backwards ??
                jt   := iloc+ncf*(i-1)+jp-1;   //index into coef[]
                if (jt<1) or (jt>nintv) then raise Exception.Create('invalid coef index');     //sanity check index
                r[i] := r[i] + tn[jp]*aPeriod.coef[jt];
              end;
          end;
        // copy vectors
        Position := Vector3D_D( r[1], r[2], r[3] );
        Speed    := Vector3D_D( r[4], r[5], r[6] );
        result   := true;
      end
      else raise Exception.Create('period outside bounds');
end;

end.
