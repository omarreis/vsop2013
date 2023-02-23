unit doubleVector3D;  // TVector3D_D - 3D vector w/ Double components ( RTL TVector3D has Single components )
//-------------------//

interface

uses
  System.Math;

type
  // Delphi TVector3D (System.Math.Vectors) components are Singles, not long enough to hold planet coordinate calculations
  TVector3D_D=record    // 3d vector w/ Double components
    x,y,z:Double;

    class function Create(const ax, ay, az: Double): TVector3D_D;  static;
    // basic vector arithmetic
    class operator Negative(const aVector: TVector3D_D): TVector3D_D;
    class operator Add(const aVector1, aVector2: TVector3D_D): TVector3D_D;
    class operator Subtract(const aVector1, aVector2: TVector3D_D): TVector3D_D;
    class operator Multiply(const aFactor: Double; const aVector: TVector3D_D): TVector3D_D;
    class operator Multiply(const aVector: TVector3D_D; const aFactor: Double): TVector3D_D;
    class operator Divide(const aVector: TVector3D_D; const aFactor: Double): TVector3D_D;
    // TODO: dot and vector products

    function Length: Double;
    function Normalize: TVector3D_D;

    function getLatLonRadius(var aLat,aLon,aRadius:Double):boolean;    // convert cartesian x,y,z ---> lat,lon,radius ( spherical coordinates )
  end;

function Vector3D_D(const X, Y, Z: Double): TVector3D_D;

implementation        //-----------------------------

function Vector3D_D(const X, Y, Z: Double): TVector3D_D;
begin
  Result.x := X;
  Result.y := Y;
  Result.z := Z;
end;

{ TVector3D_D }

class function TVector3D_D.Create(const ax, ay, az: Double): TVector3D_D;
begin
  Result.x := ax;
  Result.y := ay;
  Result.z := az;
end;

class operator TVector3D_D.Negative(const aVector: TVector3D_D): TVector3D_D;
begin
  Result.x := -aVector.x;
  Result.y := -aVector.y;
  Result.z := -aVector.z;
end;

class operator TVector3D_D.Add(const aVector1, aVector2: TVector3D_D): TVector3D_D;
begin
  Result.x := aVector1.x + aVector2.x;
  Result.y := aVector1.y + aVector2.y;
  Result.z := aVector1.z + aVector2.z;
end;

class operator TVector3D_D.Subtract(const aVector1, aVector2: TVector3D_D): TVector3D_D;
begin
  Result.x := aVector1.x - aVector2.x;
  Result.y := aVector1.y - aVector2.y;
  Result.z := aVector1.z - aVector2.z;
end;

class operator TVector3D_D.Multiply(const aFactor: Double; const aVector: TVector3D_D): TVector3D_D;
begin
  Result := aVector * aFactor;
end;

class operator TVector3D_D.Multiply(const aVector: TVector3D_D; const aFactor: Double): TVector3D_D;
begin
  Result.x := aVector.x * aFactor;
  Result.y := aVector.y * aFactor;
  Result.z := aVector.z * aFactor;
end;

function TVector3D_D.Normalize: TVector3D_D;  // ret versor ( mag=1.0 )
var aLen:Double;
begin
  aLen   := Self.Length;
  if (aLen<>0) then   Result := Self * (1/aLen)
     else Result := Self;         // zero vector
end;

class operator TVector3D_D.Divide(const aVector: TVector3D_D; const aFactor: Double): TVector3D_D;
begin
  Result := aVector * ( 1 / aFactor );
end;

function TVector3D_D.getLatLonRadius(var aLat, aLon, aRadius: Double): boolean;  // lat,lon in degrees, radius in au
const rad2deg=180/Pi;
begin
  aRadius := Length;        // in AU
  if aRadius<>0 then
    begin
      aLat   := System.Math.ArcSin( z/aRadius )*rad2deg;    // isso memo ??
      aLon   := System.Math.ArcTan2( y, x )*rad2deg;        // ou vice-versa ?
      Result := true;
    end
    else Result := false;   // zero radius --> no lat,lon
end;

function TVector3D_D.Length: Double;
begin
  Result := Sqrt( x*x + y*y + z*z );
end;

end.
