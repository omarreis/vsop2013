Unit Om.SphericalTriangle;  //--- Spherical triangle solutions  --------------//
//-------------------------//                                                //
// programmed by Omar                                                       //
//   repository: github.com/omarreis/VSOP2013                              //
//------------------------------------------------------------------------//

interface
uses
  System.SysUtils,

  Om.Trigonometry,
  Om.AstronomicalAlgorithms;

type
  // TSphericalTriangle - Uses the law of cossines of spherical triangle
  // to calculate the sides and vertices (see diagram )
  TSphericalTriangle=class(TObject)
  private
    function CheckValidity(aa, ab, ac: double): boolean;                    //
  public                                                                    //   B----a----C
    VA,VB,VC:double;   //angle on vertices A,B e C (in degrees)             //    \       /
    LA,LB,LC:double;   //sides opposite to angles (a b e c) (in degrees)    //     \     /       <-- this on a sphere
    Constructor Create;                                                     //    c \   / b
    function    CalcSideA:double;     // needs  lb,lc,VA                    //       \ /
    function    CalcVerticeA:double;  // needs  VB,VC,la                    //        A
  end;

// In celestial navigation we have
//
//                      + P Earth Pole
//                     / \
//                    /   \
//                   /     \
//                  /       \        XZ = 90 - H  (Azimuthal Distance = 90 - Altitude)
//    GP of body X +__       \
//    (Decl,GHA)      --__    \
//                        --__ \
//                            --+ Z Assumed Position
//                                  (Lat,Lon)
//

//-----------------------------------------------
// calcPositionTriangleSimple()
// params: celestial object position Decl,GHA and navigator position LatE,LonE.
// Returns calculated Altitude and Azimuth (Acalc,Azcalc) and if the star is visible from navigator position (i.e. it has positive altitude )
Procedure calcPositionTriangleSimple({in:}Decl,GHA,LatE,LonE:Single; {out:} var Acalc,Azcalc:Single; var IsVisible:Boolean);

//-----------------------------------------------
// calcPositionTriangle() returns some intermediate results, used in celestial navigation
// may throw exception if parameters are inconsistent (ex: Decl=90)
// params:
//   celestial object position Decl,GHA
//   navigator position LatE,LonE ( the assumed position or dead rekoning )
//   Aic=corrected instrumental Altitude ( sextant altitude corrected for Parallax, Height of the eye, index error etc )
// Returns:
//   calculated Altitude and Azimuth (Acalc,Azcalc) of the celestial body
//   LHA - Local Hour Angle of the body
//   Dpos - Distance from the Navigator position to the celestial object Line of Position (LOP), in direction of the Azimuth. In Celnav this is also known as Delta or Intercept
//   IsVisible - star is visible from navigator position (i.e. it has positive altitude )
Procedure calcPositionTriangle({in:}Decl,GHA,LatE,LonE,Aic:Double;{out:}var Acalc,LHA,Dpos,Azcalc:Double;var IsVisible:Boolean);

implementation {================================================}

// Solucao do triangulo de posicao}
// Dados:
//   Decl e GHA do Astro A
//   LatE,LonE=Pos estimada
//   Aic=Altura do instrumento corrigida
// Retorna:
//  Altura calculada Ac
//  LHA
//  Dist da posicao estimada Dpos
//  Azimute calculado Azcalc }

Procedure calcPositionTriangleSimple({ins}Decl,GHA,LatE,LonE:Single; {outs=} var Acalc,Azcalc:Single; var IsVisible:Boolean);
var
  PA,PZ,AZ,       {Lados do triangulo de posicao}
  Atx,Z,x:Single;
  LHA:Double;
begin
  Acalc:=0; Azcalc:=0;
  LHA := GHA-LonE;
  PA  := 90.0-Decl;
  PZ  := 90.0-LatE;
  AZ  := Cosg(PA)*Cosg(PZ)+Sing(PA)*Sing(PZ)*Cosg(LHA);
  IsVisible := (AZ>=0);
  if IsVisible then
    begin
      AZ := ACosg(AZ);
      Acalc := 90.0-AZ;              // Converts zenithal distance to calculated Altitude Hc
      if Acalc>90 then Acalc := Acalc-180;
      if (Sing(PA)=0) then begin IsVisible:=false; exit; end;
      x := (Cosg(LHA)*Cosg(PZ)-Cosg(PA)/Sing(PA)*Sing(PZ)); {Almanac for computers Pag. B4}
      if (x=0) then  begin IsVisible:=false; exit; end;    // Error calculating Azimuth
      x := Sing(LHA)/x;
      Atx := ATang(x);
      PoeEmRange(LHA, 360.0);  {Poe LHA de 0 a 360}
      if LHA<=180.0 then  {Ajusta Azimute}
        begin
          if x>=0 then Z := 180.0+Atx else Z := 360.0+Atx;
        end
        else begin
          if x>=0 then Z:= Atx else Z := 180.0+Atx;
        end;
      Azcalc:=Z;
    end;
end;

Procedure calcPositionTriangle({ins}Decl,GHA,LatE,LonE,Aic:Double;
  {outs=} var Acalc,LHA,Dpos,Azcalc:Double; var IsVisible:Boolean);
var
  PA,PZ,AZ:Double; {Lados do triangulo de posicao}
  Atx:Double;
  Z,x:Double;     {Azimute do astro}
begin
  Acalc:=0; Azcalc:=0;
  LHA := GHA-LonE;
  PA  := 90.0-Decl;
  PZ  := 90.0-LatE;
  AZ  := Cosg(PA)*Cosg(PZ)+Sing(PA)*Sing(PZ)*Cosg(LHA);
  IsVisible := (AZ>=0);
  AZ := ACosg(AZ);
  Acalc := 90.0-AZ;       {Converte dist zenital p/ altura calculada}
  if Acalc>90 then Acalc := Acalc-180;
  Dpos := (Aic-Acalc)*60; {Calc distance to Assumed position ( Delta in nautical miles }

  if (Sing(PA)=0) then
    Raise Exception.Create('Error calculating Azimuth: Sin(PA)=0');

  x := (Cosg(LHA)*Cosg(PZ)-Cosg(PA)/Sing(PA)*Sing(PZ)); {Almanac for computers Pag. B4}
  if (x=0) then
    Raise Exception.Create('Error calculating Azimuth: x=0');
  x := Sing(LHA)/x;
  Atx := ATang(x);
  PoeEmRange(LHA,360.0);  {Poe LHA de 0 a 360}
  if LHA<=180.0 then  {Ajusta Azimute}
    begin
      if x>=0 then Z := 180.0+Atx
        else Z := 360.0+Atx;
    end
    else begin
      if x>=0 then Z:= Atx
        else Z := 180.0+Atx;
    end;
  Azcalc:=Z;
end;

{ TSphericalTriangle }

constructor TSphericalTriangle.Create;
begin
  inherited;
  VA:=-1; VB:=-1; VC:=-1; //inicializa lados e vertices com -1, indicando invalido
  LA:=-1; LB:=-1; LC:=-1;
end;

function TSphericalTriangle.CheckValidity(aa,ab,ac:double):boolean;
begin
  if (aa<0) or (ab<0) or (ac<0) then
    Raise Exception.Create('TSphericalTriangle: invalid parameter in calculation');
end;

function TSphericalTriangle.CalcSideA: double;
var aLA:double;
begin
  CheckValidity(LB,LC,VA);
  aLA:=Cosg(LB)*Cosg(LC)+sing(LB)*sing(LC)*cosg(VA);
  Result:=ACosg(aLA);
end;

function TSphericalTriangle.CalcVerticeA: double;
var aVA:double;
begin
  CheckValidity(VB,VC,LA);
  aVA:=-cosg(VB)*cosg(VC)+sing(VB)*sing(VC)*cosg(LA);
end;

end.
