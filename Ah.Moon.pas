unit Ah.Moon;  // Moon calculations ---------------------------------------}
{-------------//                                                            }
{ This code was adapted from Andreas Hörstemeier TMoon component v2.0        }
{ for Delphi 6  - Original (c) statement                                     }
{ Copyright 1997-2001 Andreas Hörstemeier            Version 2.0 2001-07-07  }
{ this component is public domain - please check the file moon.hlp for       }
{ more detailed info on usage and distributing                               }
{ see http://www.hoerstemeier.com/delphi.htm                                 }
{ Algorithms from the book "Astronomical Algorithms" by Jean Meeus           }
{   - ELP2000 - Chapront-Touze                                               }
{   - Astronomical Algorithms, Jean Meeus (1991) Ed I                        }
{   - AA - 2nd edition         (1998) <- much expanded and corrected        }
{  Om: TMoon functionality was adapted to newer compiler and               }
{     merged w/ other AA code                                             }
{------------------------------------------------------------------------}

interface

uses
  System.SysUtils, System.Math;

type
  t_coord = record      // custom record format for returning coordinates
    longitude, latitude, radius: extended; (* lambda, beta, R *)  // geocentric
    rektaszension, declination: extended;  (* alpha, delta *)     // celestial
    parallax: extended;
    elevation, azimuth: extended;          (* h, A *)
  end;

function moon_coordinate(const aDate:TDateTime):t_coord;  // aDate in UT

implementation  //---------------------------------------

uses
  Om.Trigonometry,
  Om.AstronomicalAlgorithms;  // Om: date functions 

{ Coordinate functions }
{ Based upon Chapter 13 (12) and 22 (21) of Meeus }

const  // tables of Moon orbit coefs (60 terms)
  arg_lr:array[0..59,0..3] of shortint = (   //p 309 table 45.A
     ( 0, 0, 1, 0),
     ( 2, 0,-1, 0),
     ( 2, 0, 0, 0),
     ( 0, 0, 2, 0),
     ( 0, 1, 0, 0),
     ( 0, 0, 0, 2),
     ( 2, 0,-2, 0),
     ( 2,-1,-1, 0),
     ( 2, 0, 1, 0),
     ( 2,-1, 0, 0),
     ( 0, 1,-1, 0),
     ( 1, 0, 0, 0),
     ( 0, 1, 1, 0),
     ( 2, 0, 0,-2),
     ( 0, 0, 1, 2),
     ( 0, 0, 1,-2),
     ( 4, 0,-1, 0),
     ( 0, 0, 3, 0),
     ( 4, 0,-2, 0),
     ( 2, 1,-1, 0),
     ( 2, 1, 0, 0),
     ( 1, 0,-1, 0),
     ( 1, 1, 0, 0),
     ( 2,-1, 1, 0),
     ( 2, 0, 2, 0),
     ( 4, 0, 0, 0),
     ( 2, 0,-3, 0),
     ( 0, 1,-2, 0),
     ( 2, 0,-1, 2),
     ( 2,-1,-2, 0),
     ( 1, 0, 1, 0),
     ( 2,-2, 0, 0),
     ( 0, 1, 2, 0),
     ( 0, 2, 0, 0),
     ( 2,-2,-1, 0),
     ( 2, 0, 1,-2),
     ( 2, 0, 0, 2),
     ( 4,-1,-1, 0),
     ( 0, 0, 2, 2),
     ( 3, 0,-1, 0),
     ( 2, 1, 1, 0),
     ( 4,-1,-2, 0),
     ( 0, 2,-1, 0),
     ( 2, 2,-1, 0),
     ( 2, 1,-2, 0),
     ( 2,-1, 0,-2),
     ( 4, 0, 1, 0),
     ( 0, 0, 4, 0),
     ( 4,-1, 0, 0),
     ( 1, 0,-2, 0),
     ( 2, 1, 0,-2),
     ( 0, 0, 2,-2),
     ( 1, 1, 1, 0),
     ( 3, 0,-2, 0),
     ( 4, 0,-3, 0),
     ( 2,-1, 2, 0),
     ( 0, 2, 1, 0),
     ( 1, 1,-1, 0),
     ( 2, 0, 3, 0),
     ( 2, 0,-1,-2)
                   );
  arg_b:array[0..59,0..3] of shortint = (  // 45.B
     ( 0, 0, 0, 1),
     ( 0, 0, 1, 1),
     ( 0, 0, 1,-1),
     ( 2, 0, 0,-1),
     ( 2, 0,-1, 1),
     ( 2, 0,-1,-1),
     ( 2, 0, 0, 1),
     ( 0, 0, 2, 1),
     ( 2, 0, 1,-1),
     ( 0, 0, 2,-1),  (* !!! Error in German Meeus *)
     ( 2,-1, 0,-1),
     ( 2, 0,-2,-1),
     ( 2, 0, 1, 1),
     ( 2, 1, 0,-1),
     ( 2,-1,-1, 1),
     ( 2,-1, 0, 1),
     ( 2,-1,-1,-1),
     ( 0, 1,-1,-1),
     ( 4, 0,-1,-1),
     ( 0, 1, 0, 1),
     ( 0, 0, 0, 3),
     ( 0, 1,-1, 1),
     ( 1, 0, 0, 1),
     ( 0, 1, 1, 1),
     ( 0, 1, 1,-1),
     ( 0, 1, 0,-1),
     ( 1, 0, 0,-1),
     ( 0, 0, 3, 1),
     ( 4, 0, 0,-1),
     ( 4, 0,-1, 1),
     ( 0, 0, 1,-3),
     ( 4, 0,-2, 1),
     ( 2, 0, 0,-3),
     ( 2, 0, 2,-1),
     ( 2,-1, 1,-1),
     ( 2, 0,-2, 1),
     ( 0, 0, 3,-1),
     ( 2, 0, 2, 1),
     ( 2, 0,-3,-1),
     ( 2, 1,-1, 1),
     ( 2, 1, 0, 1),
     ( 4, 0, 0, 1),
     ( 2,-1, 1, 1),
     ( 2,-2, 0,-1),
     ( 0, 0, 1, 3),
     ( 2, 1, 1,-1),
     ( 1, 1, 0,-1),
     ( 1, 1, 0, 1),
     ( 0, 1,-2,-1),
     ( 2, 1,-1,-1),
     ( 1, 0, 1, 1),
     ( 2,-1,-2,-1),
     ( 0, 1, 2, 1),
     ( 4, 0,-2,-1),
     ( 4,-1,-1,-1),
     ( 1, 0, 1,-1),
     ( 4, 0, 1,-1),
     ( 1, 0,-1,-1),
     ( 4,-1, 0,-1),
     ( 2,-2, 0, 1)
    );
  sigma_r: array[0..59] of longint = (  // p 309
   -20905355,
    -3699111,
    -2955968,
     -569925,
       48888,
       -3149,
      246158,
     -152138,
     -170733,
     -204586,
     -129620,
      108743,
      104755,
       10321,
           0,
       79661,
      -34782,
      -23210,
      -21636,
       24208,
       30824,
       -8379,
      -16675,
      -12831,
      -10445,
      -11650,
       14403,
       -7003,
           0,
       10056,
        6322,
       -9884,
        5751,
           0,
       -4950,
        4130,
           0,
       -3958,
           0,
        3258,
        2616,
       -1897,
       -2117,
        2354,
           0,
           0,
       -1423,
       -1117,
       -1571,
       -1739,
           0,
       -4421,
           0,
           0,
           0,
           0,
        1165,
           0,
           0,
        8752
              );
  sigma_l: array[0..59] of longint = (
    6288774,
    1274027,
     658314,
     213618,
    -185116,
    -114332,
      58793,
      57066,
      53322,
      45758,
     -40923,
     -34720,
     -30383,
      15327,
     -12528,
      10980,
      10675,
      10034,
       8548,
      -7888,
      -6766,
      -5163,
       4987,
       4036,
       3994,
       3861,
       3665,
      -2689,
      -2602,
       2390,
      -2348,
       2236,
      -2120,
      -2069,
       2048,
      -1773,
      -1595,
       1215,
      -1110,
       -892,
       -810,
        759,
       -713,
       -700,
        691,
        596,
        549,
        537,
        520,
       -487,
       -399,
       -381,
        351,
       -340,
        330,
        327,
       -323,
        299,
        294,
          0
    );
  sigma_b: array[0..59] of longint = (
    5128122,
     280602,
     277693,
     173237,
      55413,
      46271,
      32573,
      17198,
       9266,
       8822,
       8216,
       4324,
       4200,
      -3359,
       2463,
       2211,
       2065,
      -1870,
       1828,
      -1794,
      -1749,
      -1565,
      -1491,
      -1475,
      -1410,
      -1344,
      -1335,
       1107,
       1021,
        833,
        777,
        671,
        607,
        596,
        491,
       -451,
        439,
        422,
        421,
       -366,
       -351,
        331,
        315,
        302,
       -283,
       -229,
        223,
        223,
       -220,
       -220,
       -185,
        181,
       -177,
        176,
        166,
       -164,
        132,
       -119,
        115,
        107
    );

function moon_coordinate(const aDate:TDateTime):t_coord;
var
  t,d,m,ms,f,e,ls,aJD : extended;
  sr,sl,sb,temp: extended;
  a1,a2,a3: extended;
  lambda,beta,delta: extended;
  Eps,DPhy,DEps,ax,ay,aRA,aDecl:Double;
  i: integer;
begin
  // t:= (julian_date(date)-2451545)/36525;
  aJD := DatetimeToJD( aDate ); //use dynamic time ( w/ DeltaT ) 
    
  t := (aJD-2451545)/36525;   //in centuries since j2000
  // calc Moon orbit elements
  (* mean elongation of the moon *)
  d := 297.8502042+(445267.1115168+(-0.0016300+(1/545868-1/113065000*t)*t)*t)*t;
  (* mean anomaly of the sun *)
  m := 357.5291092+(35999.0502909+(-0.0001536+1/24490000*t)*t)*t;
  (* mean anomaly of the moon *)
  ms:= 134.9634114+(477198.8676313+(0.0089970+(1/69699-1/1471200*t)*t)*t)*t;
  (* argument of the longitude of the moon *)
  f := 93.2720993+(483202.0175273+(-0.0034029+(-1/3526000+1/863310000*t)*t)*t)*t;
  (* correction term due to excentricity of the earth orbit *)
  e := 1.0+(-0.002516-0.0000074*t)*t;
  (* mean longitude of the moon *)
  ls := 218.3164591+(481267.88134236+(-0.0013268+(1/538841-1/65194000*t)*t)*t)*t;

  (* arguments of correction terms *)
  a1 := 119.75+131.849*t;
  a2 := 53.09+479264.290*t;
  a3 := 313.45+481266.484*t;

  (*  sr := ä r_i cos(d,m,ms,f);   !!!  gives different value than in Meeus *)
  sr:=0;
  for i:=0 to 59 do begin
    temp:=sigma_r[i]*cosg( arg_lr[i,0]*d
                           +arg_lr[i,1]*m
                           +arg_lr[i,2]*ms
                           +arg_lr[i,3]*f);
    if abs(arg_lr[i,1])=1 then temp:=temp*e;
    if abs(arg_lr[i,1])=2 then temp:=temp*e*e;
    sr:=sr+temp;
  end;
  (* sl := ä l_i sin(d,m,ms,f); *)
  sl:=0;
  for i:=0 to 59 do
  begin
    temp:=sigma_l[i]*sing( arg_lr[i,0]*d
                           +arg_lr[i,1]*m
                           +arg_lr[i,2]*ms
                           +arg_lr[i,3]*f);
    if abs(arg_lr[i,1])=1 then temp:=temp*e;
    if abs(arg_lr[i,1])=2 then temp:=temp*e*e;
    sl:=sl+temp;
  end;

  (* correction terms *)
  sl:=sl +3958*sing(a1)
         +1962*sing(ls-f)
          +318*sing(a2);
  (* sb := ä b_i sin(d,m,ms,f); *)
  sb:=0;
  for i:=0 to 59 do begin
    temp:=sigma_b[i]*sing( arg_b[i,0]*d
                           +arg_b[i,1]*m
                           +arg_b[i,2]*ms
                           +arg_b[i,3]*f);
    if abs(arg_b[i,1])=1 then temp:=temp*e;
    if abs(arg_b[i,1])=2 then temp:=temp*e*e;
    sb:=sb+temp;
  end;

  (* correction terms *)
  sb:=sb -2235*sing(ls)
          +382*sing(a3)
          +175*sing(a1-f)
          +175*sing(a1+f)
          +127*sing(ls-ms)
          -115*sing(ls+ms);
  (*@\\\*)

  lambda := ls+sl/1000000;
  lambda := getAngleTo0_360(lambda);
  beta   := sb/1000000;
  delta  := 385000.56+sr/1000;

  //return coordinates
  result.radius     := delta;
  result.longitude  := lambda;
  result.latitude   := beta;

  // calc apparent celestial coordinates
  CorrNut(t,{out:} Eps,DPhy,DEps);      {Calc Nutation correction and obliquity }
  lambda := lambda+DPhy/3600;           {Corrige lamb p/ nutacao p.213}

  ay  := Sing(lambda)*Cosg(Eps)-Tang(beta)*Sing(Eps);
  ax  := Cosg(lambda);
  aRA := Arctan2(ay,ax);     {12.3 pag. 89}  // aRA in rads
  aRA := aRA*180/Pi;                         // convert to degrees
  aRA := getAngleTo0_360( aRA );

  aDecl := Sing(beta)*Cosg(Eps)+Cosg(Beta)*Sing(Eps)*Sing(lambda);
  aDecl := ASing(aDecl); {12.4}

  result.rektaszension := aRA/360*24;     // RA degrees --> hours
  result.declination   := aDecl;
end;

end.


