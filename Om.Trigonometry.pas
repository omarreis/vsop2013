Unit Om.Trigonometry;  // angle calculations and formating
//--------------------//
// programmed by oMAR
//      repository: github.com/omarreis/VSOP2013
// -------------------------------------------------------------------------

interface

uses
  System.SysUtils,System.Math;

const
  Bolinha='°';  // degree symbol


procedure AngleTo0_360(var A:Double);              // put angle A in 0..360° range
function  getAngleTo0_360(const A:Double):Double;  //same as above, but different..

// trigonometric functions using degree params
Function Sing(const G:Double):Double;  { Sin() using degrees}
Function Cosg(const G:Double):Double;  { Cos() using degrees}
Function Tang(const G:Double):Double;  { Tan() using degrees }
Function ATan2(y,x:Double):Double;     {Arco tangente com quadrante correto em graus}
Function ATang(T:Double):Double;       {ATan returning degrees -90 to 90}
Function ASing(const S:Double):Double; {arc sin returning degrees}
Function ACosg(C:Double):Double;

// Format angles to string (or hours). Angles in degrees
function floatToLatitudeStr(const a:Double):String;     // a in degrees -->  '23.26°N'  ( not minutes )  S=negative
function floatToLongitudeStr(const a:Double):String;   // '46.26°W'                                      E=negative
Function floatToHHMMSS(R:Double):String;              // Time Double --> HHHh MM' SS"
Function floatToGMSD(R:Double):String;               // degrees Double --> GGG°MM'SS.DD"
Function floatToGMSD_Lat(R:Double):String;          // Same as floatToGMSD() but w/ negative signal sulfixed with 'S' ( positive='N' )

// format Lat or Lon to string, using sulfix 'NS' or 'WE'
Function R2GMD(R:Double;var GM:Double; Sufx:String):String; {Double --> 'GGGøMM.D' X' }

// angle/hour normalization ( set to range )
Procedure AjustaHora(var H:Double);                 // retuns H in range 0..24
Procedure AjustaAngulo(var R:Double);              // returns angle R in range 0..360
Procedure PoeEmRange(var R:Double; RMax:Double);  // Coloca R in range 0..RMax

implementation //------------------------------------------------------------------------------

Procedure PoeEmRange(var R:Double;RMax:Double);    // Coloca R no range entre 0 e RMax
var D:Double;
begin
  if (R>RMax) then D:=-RMax
  else if (R<0.0) then D:=RMax
  else exit; {Esta no range, sai}
  While (R<0.0) or (R>RMax) do R:=R+D;
end;

Procedure AjustaHora(var H:Double); {Coloca H no range entre 0 e 24}
begin  PoeEmRange(H,24.0);  end;

Procedure AjustaAngulo(var R:Double); {Coloca R no range entre 0 e 360.0}
begin  PoeEmRange(R,360); end;


Function Sing(const G:Double):Double;  { Sin() using degrees}
begin Sing := Sin(G*Pi/180); end;

Function ASing(const S:Double):Double; {arc sin em Graus}
begin ASing := ArcSin(S)*180/Pi; end;

Function ATang(T:Double):Double; {ATan em Graus}
begin ATang:=ArcTan(T)*180/Pi; end;

Function ACosg(C:Double):Double; {arc cos em Graus}
begin ACosg:=ArcCos(C)*180/Pi; end;

Function Cosg(const G:Double):Double;  { Cos() using degrees}
begin Cosg := Cos(G*Pi/180); end;

Function Tang(const G:Double):Double;        { Tan() using degrees }
var CG:Double;
begin
  CG:=Cosg(G);
  {if CG=0.0 then CG:=1E-20;}     {= Numero bem pequeno}
  Tang:=Sing(G)/CG;
end;

Function ATan2(y,x:Double):Double; {Arco tangente com quadrante correto em graus}
var t:Double;    {Tan=Sin/cos=y/x}
begin
  t:=ArcTan(y/x)*180/Pi; {Calcula tangente no range -90..+90 }
  if x>0 then
    begin
      if y<0 then t:=360.0+t; {4o quadrante (t<0)}
      {else t:=t  1o quadrante, nao muda}
    end
    else t:=t+180.0;   {3o e 4o quadrante}
  Atan2:=t;
end;

function floatToLatitudeStr(const a:Double):String;
var ang:Double; Sulfix:String;
begin
  if      (a>0) then Sulfix:='N'
  else if (a<0) then Sulfix:='S'
  else Sulfix:='';   // 0 --> '00.00'
  ang := Abs(a);
  Result := Trim( Format('%5.1f°',[ang]) )+Sulfix;
end;

function floatToLongitudeStr(const a:Double):String;
var ang:Double; Sulfix:String;
begin
  if      (a>0) then Sulfix:='E'
  else if (a<0) then Sulfix:='W'
  else Sulfix:='';   // 0 --> '00.00'
  ang := Abs(a);
  Result := Trim( Format('%6.1f°',[ang]) )+Sulfix;
end;

function R2HMS(R:Double;var HMS:Double):String;    {Double --> 'HHHh MM' SS"}
var h,m,s:Double; sh,sm,ss:String; Sx:Char; Sn:Integer;
begin
  if R<0 then begin Sx:='O'; Sn:=-1; r:=-r;  end //nov/05 Sx= '0' quando negativo ?
    else begin Sx:=' '; Sn:=1; end;
  {R=HH.DDDD}
  h := Trunc(R);  {HH}
  R := 60*(R-h);  {MM.DD}
  m := Trunc(R);  {MM}
  R := 60*(R-m);  {SSDD}
  s := Round(R);
  if (S>=60) then begin S:=s-60; m:=m+1; end;
  if (m>=60) then begin m:=m-60; h:=h+1; end;
  Str(h:2:0, sh);
  Str(m:2:0, sm);
  Str(s:2:0, ss);
  if sm[1]=' ' then sm[1]:='0';
  if ss[1]=' ' then ss[1]:='0';
  R2HMS:=sh+':'+sm+':'+ss+' '+Sx;
  HMS:=Sn*(h+m/100+s/10000);      {H retorna HH.MMSS}
end;

// R (0..1) --> HH:MM:SS (0..24)
Function floatToHHMMSS(R:Double):String; {Double --> 'HHHh MM' SS"} //nova fev06
var Dummy:Double; L:integer;
begin
  Result := R2HMS(R,Dummy);
  L := Length(Result);
  if (Result[L]='O') then //O no final de R2HMS() significa negativo
    begin
      Delete(Result,L,1);
      Result:='(-)'+Result;
    end;
end;

Function floatToGMSD(R:Double):String;   // degrees Double --> 'GGGøMM'SS.DD"} mai08:Om:
var g,m,s:Double; sg,sm,ss:String; Sx:Char; Sn:Integer;
begin
  if (R<0) then begin Sx:='-'; r:=-r;  end
    else begin  Sx:=' '; end;
  {R=GG.DDDD}
  g := Trunc(R);  {GG}
  R := 60*(R-g);  {MM.DD}
  m := Trunc(R);  {MM}
  R := 60*(R-m);  {SSDD}
  s := R;
  if (s>=60) then begin s:=s-60; m:=m+1; end;
  if (m>=60) then begin m:=m-60; g:=g+1; end;
  Str(g:2:0,sg);
  Str(m:2:0,sm);
  if (sm[1]=' ') then sm[1]:='0';
  ss := Format('%5.2f',[s]);
  if (ss[1]=' ') then ss[1]:='0';
  Result := Trim( Sx+sg+'°'+sm+''''+ss+'"');
end;

// floatToGMSD_Lat() Same as floatToGMSD, but negative signal sulfixed with 'S' and positive='N'
// Used to display Latitudes and Declinations
Function floatToGMSD_Lat(R:Double):String;
var sulfix:String;
begin
  if (R<0) then begin sulfix:='S'; R:=-R; end
    else sulfix:='N';
  Result := floatToGMSD(R)+sulfix;
end;

Function R2GMD(R:Double; var GM:Double; Sufx:String):String;  {Double --> 'GGGø MM.D' X'}
var h,m:Double; sh,sm:String; Sn:Integer;
  Sx:Char;
begin
  if R<0 then begin Sx:=Sufx[2]; {p.e. N} R:=-R; Sn:=-1; end
      else begin Sx:=Sufx[1];    {p.e. S} Sn:=1; end;

  h:= Trunc(R);      {h=HHH}
  m:= 60*(R-h);      {m=MM.D}
  if m>=60.0 then
    begin m:=m-60; h:=h+1; end;
  Str(h:2:0,sh);
  Str(m:4:1,sm);
  if sm[1]=' ' then sm[1]:='0';
  Result := sh+Bolinha+sm+Char(39)+Sx;
  GM := Sn*(h+m/100);   {G returns GG.MMD, just in case you want to see the decimal number}
end;

Function HourTo0_24(const H:Double):Double; //put H in 0- 24h range
begin
  Result := H;
  if (Result<0)        then Result := Result+24
  else if (Result>=24) then Result := Result-24;
end;

procedure AngleTo0_360(var A:Double); // put angle in 0..360° range
begin
  while (A<0)      do A:=A+360.0;
  while (A>=360.0) do A:=A-360.0;
end;

function getAngleTo0_360(const A:Double):Double;  //same as abovce, but different..
var aA:Double;
begin
  aA := A;
  AngleTo0_360( aA );
  Result := aA;
end;

end.
