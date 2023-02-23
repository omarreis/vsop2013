Unit Om.DeltaT; //- DeltaT is the correction between UT (Universal Time) and DT (Dynamical Time)
 //------------//
// Universal Time is not uniform. The Earth is slowing down and extra seconds are added to the UT year.
// Dynamical Time is uniform, good for measuring time intervals.
//
// Expanded the table below from  1994 to 2018 usando valores obtidos em ftp://maia.usno.navy.mil/ser7/deltat.preds
// Om: sep20: revised and expanded table from 2020 to 2058 using formula
//
//      repository: github.com/omarreis/VSOP2013
//---------------------------------------------------------------------------------------
interface

function calcDeltaT(const T : Double) : Double;  // T in centuries since J2000

implementation  //-------------------------------------------------

const
  { Tab 9.A }
  MaxDeltaTTable=219;
  DeltaTTable : array[0..MaxDeltaTTable] of integer =  // values = seconds * 10
//1620   .2   .4   .6   .8   :0   :2   :4   :6   :8
 (1240,1150,1060, 980, 910, 850, 790, 740, 700, 650,
   620, 580, 550, 530, 500, 480, 460, 440, 420, 400,
   370, 350, 330, 310, 280, 260, 240, 220, 200, 180,
   160, 140, 130, 120, 110, 100,  90,  90,  90,  90,
    90,  90,  90,  90, 100, 100, 100, 100, 100, 110,
   110, 110, 110, 110, 110, 110, 110, 120, 120, 120,
   120, 120, 130, 130, 130, 130, 140, 140, 140, 150,
   150, 150, 150, 160, 160, 160, 160, 160, 170, 170,
   170, 170, 170, 170, 170, 170, 160, 160, 150, 140,
   137, 131, 127, 125, 125, 125, 125, 125, 125, 123,
   120, 114, 106,  96,  86,  75,  66,  60,  57,  56,
    57,  59,  62,  65,  68,  71,  73,  75,  77,  78,
    79,  75,  64,  54,  29,  16, -10, -27, -36, -47,
   -54, -52, -55, -56, -58, -59, -62, -64, -61, -47,
   -27,   0,  26,  54,  77, 105, 134, 160, 182, 202,
   212, 224, 235, 239, 243, 240, 239, 239, 237, 240,
   243, 253, 262, 273, 282, 291, 300, 307, 314, 322,
   331, 340, 350, 365, 383, 402, 422, 445, 465, 485,
//                           90   92   94   96   98
   505, 522, 538, 549, 558, 569, 580, 599, 610, 620,
//2000   02   04   06   08   10   12   14   16   18
   630, 636, 643, 651, 659, 667, 676, 685, 695, 705,
//2020   02   04   06   08   10   12   14   16   18
   716, 727, 739, 751, 763, 776, 790, 803, 818, 832,
//2040   02   04   06   08   10   12   14   16   18
   847, 863, 879, 896, 913, 930, 948, 966, 985,1004);
// Om: sep20: revised table from 2000 to 2058 on using the formula below
// from https://eclipse.gsfc.nasa.gov/SEhelp/deltatpoly2004.html
// period 2005-2050
// ΔT = 62.92 + 0.32217 * t + 0.005589 * t^2
//  	where: t = y - 2000
// Ano	t	DeltaT  <-- Excel generated
// 2000	0	62.9
// 2002	2	63.6
// 2004	4	64.3
// 2006	6	65.1
// 2008	8	65.9
// 2010	10	66.7
// 2012	12	67.6
// 2014	14	68.5
// 2016	16	69.5
// 2018	18	70.5
// 2020	20	71.6
// 2022	22	72.7
// 2024	24	73.9
// 2026	26	75.1
// 2028	28	76.3
// 2030	30	77.6
// 2032	32	79.0
// 2034	34	80.3
// 2036	36	81.8
// 2038	38	83.2
// 2040	40	84.7
// 2042	42	86.3
// 2044	44	87.9
// 2046	46	89.6
// 2048	48	91.3
// 2050	50	93.0
// 2052	52	94.8
// 2054	54	96.6
// 2056	56	98.5
// 2058	58	100.4

// calcDeltaT(T)
// Param:    T : number of Julian centuries since J2000
// Returns:  DeltaT in seconds
function calcDeltaT(const T:Double) : Double;  //T em seculos desde 2000
var
  y, DDT, F : Double;
  Index : integer;
begin
  y := 2000+T*100;     // calc year
  if (y >= 2050) then  // 2050+
  begin
    // from https://eclipse.gsfc.nasa.gov/SEhelp/deltatpoly2004.html
    // period 2050-2150
    F := (y-1820)/100;
    Result := -20+32*( F*F - 0.5628*(2150-y) );
    //era: Result := 102.3 + T*(123.5 + T*32.5)
    // Esta formula produzia valores muito altos após 2000.
    // Introduzí o '-50'  para a formula "engatar" bem nos valores da tabela até 2016, pois a formula do Meeus dava
    // um pulo em relacao aos valores reais. Isso foi uma gambiarra, mas com boas intenções....
    // Result := 102.3 + T*(123.5 + T*32.5) -50;
  end
  else if (y<1620) then  // before 1620. use formulas
    begin
      if (y<948) then Result := 2715.6 + T*(573.36 + T*46.5)  //before 948
        else Result :=   50.6 + T*( 67.5 + T*22.5);           //  948-1620
    end
    else begin   { between 1620-2050 --> use table }
      Index := trunc( (y - 1620)/2 );
      if (Index > MaxDeltaTTable-1) then
        Index := MaxDeltaTTable-1;
      y   := y/2 - (Index+810);          // y = parte fracionaria do ano
      DDT := (DeltaTTable[Index+1]-DeltaTTable[Index])*y;
      Result := (DeltaTTable[Index] + DDT ) / 10;
    end;
end;

end.

