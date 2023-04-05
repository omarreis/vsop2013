Unit VSOP87.PlanetDef; //---- Planet coordinates using VSOP87 ---------------------------
//--------------------//
// VSOP87 = Variations Seculaires des Orbiotes Planetaires 1987
//    by Bretagnon and Francou - Bureau des Longitudes of Paris
//
// calculates planet's ecliptical coordinates Lon,Lat and Heliocentric Radius, given a time
//
// This method is described in J.Meeus book "Astronomical Algorithms" - Chapter 32
// used as basis for this code.
//
// Note that this repository also contains newer VSOP2013 theory, more precise.
// However VSOP2013 requires a large 100 MB table of coeficients, which makes it
// difficult to use in mobile devices.
// VSOP87 gives good enough results for most apps, using a smaller set of data coeficients.
// In this implementation the coeficients are embedded as code constants,
// rather than a separate file ( as in VSOP2013 )
//
// mostly programmed by Omar
//      repository: github.com/omarreis/VSOP2013
//---------------------------------------------------------------------------------------

interface

Type

  TPlanTerm=Record  // planetary coeficients are organized in 3 dimension records
    A,B,C:Double;
  end;

  PPlanTermArr=^TPlanTermArr;
  TPlanTermArr=Array[1..100] of TPlanTerm;

  TNTermos=Array[0..5] of integer;  // Array com numero de termos de cada serie L0,L1 etc
  TPTermos=Array[0..5] of Pointer;  // Array com ptr das series de termos

  PPlanConsts=^RPlanconsts; // Contem tamanhos e apontadores p/ arrays de termos de 1 planeta
  RPlanConsts=Record
    NLTermos,NBTermos,NRTermos:TNTermos;
    LTermos,BTermos,RTermos:TPTermos;
  end;

// CalculaCoordenadasVSOP87()
// param:
//   T  Time in centuries since J2000.0 (use fn TJ2000() w/ D,M,Y,H)
// returns
//   L  ecliptical Longitude
//   B  ecliptical Latitude
//   R  radius from the Sun

Procedure CalcCoordinatesVSOP87(T:Double; Planeta:PPlanConsts;{out:} var L,B,R:Double);

implementation  //----------------------------------------------------------------------

uses
  Om.Trigonometry,
  Om.AstronomicalAlgorithms;  // date fns


Procedure CalcCoordinatesVSOP87(T:Double;Planeta:PPlanConsts;{out:} var L,B,R:Double);
var x:Double; i,j,N:integer; Ti:Double; PT:PPlanTermArr; TN:Double;
begin
  T:=T/10.0; {Tranforma T de seculos p/ milenios desde J2000}
  with Planeta^ do
    begin
      L  := 0.0;
      TN := 1.0; {TN=T^N}
      for i:=0 to 5 do
        begin
          Ti := 0.0;  N:=NLTermos[i];      {Pega numero de termos da serie}
          PT := PPlanTermArr(LTermos[i]);  {pega Ponteiro p/ coeficientes}
          for j:=1 to N do
            begin
              with PT^[j] do x := A*Cos(B+C*T);
              Ti:=Ti+x;
            end;
          L  := L+Ti*TN;
          TN := TN*T;
        end;
      L:=L/1e8*180/Pi;      {Ajusta e converte p/ Graus}
      {Correcao da pag. 207 nao foi aplicada }
      PoeEmRange(L,360.0);

      B:=0.0;
      TN:=1.0; {TN=T^N}
      for i:=0 to 5 do
        begin
          Ti := 0.0;  N:=NBTermos[i];      {Pega numero de termos da serie}
          PT := PPlanTermArr(BTermos[i]);  {pega Ponteiro p/ coeficientes}
          for j:=1 to N do begin with PT^[j] do x:=A*Cos(B+C*T); Ti:=Ti+x; end;
          B:=B+Ti*TN;
          TN:=TN*T;
        end;
      B:=B/1e8*180/Pi; {Ajusta e converte p/ Graus}
      PoeEmRange(B,360.0);

      R:=0.0;
      TN:=1.0; {TN=T^N}
      for i:=0 to 5 do
        begin
          Ti:=0.0;  N:=NRTermos[i];      {Pega numero de termos da serie}
          PT:=PPlanTermArr(RTermos[i]);  {pega Ponteiro p/ coeficientes}
          for j:=1 to N do begin with PT^[j] do x:=A*Cos(B+C*T); Ti:=Ti+x; end;
          R:=R+Ti*TN;
          TN:=TN*T;
        end;
      R:=R/1e8;
   end; {With Planeta^ ..}
end; {CalculaCoordenadas}

end.