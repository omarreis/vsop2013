unit quaternionRotations;  // by oMAR Quaternions are used to rotate a 3d object
// in multiple axis.
// Warning: Using RotationAngle (Euler angles) can generate gymbal locks and weird movements
// set20:

interface

uses
  System.Math.Vectors,   // TQuaternion3D
  FMX.Controls3D;

// Usage sample:
//  Q:TQuaiternion3D;
//  ToQuaternion({roll:}aSensorVec.z,{pitch:} aSensorVec.y  {aSensorVec.y},{yaw:}aSensorVec.x, Q );
//  dummyAirliner.SetMatrix(Q);   // rotate camera pointing to boat using quaternion

type  // helper class to help manipulating the matrix instead of changing RotationAngle
  TControl3DHelper = class helper for TControl3D
    procedure SetMatrix(const M: TMatrix3D);
  end;

procedure  ToQuaternion(const yaw,pitch,roll:Single; var q:TQuaternion3D);   // yaw (Z), pitch (Y), roll (X)

implementation  //----------------------------------------------------------

function deg2rad(const d:Single):Single;
begin
  Result := d/180*3.14159265;
end;

// from wikipedia https://en.wikipedia.org/wiki/Conversion_between_quaternions_and_Euler_angles
// by experience I found that this is more like ToQuaternion(roll,pitch,yaw)   or (z,x,y)
// confusing ..
procedure  ToQuaternion(const yaw,pitch,roll:Single; var q:TQuaternion3D);   // yaw (Z), pitch (Y), roll (X)
var cy,sy,cp,sp,cr,sr,y,p,r:Single;
begin  // Abbreviations for the various angular functions
  y := deg2rad(yaw);   // deg to rad
  p := deg2rad(pitch);
  r := deg2rad(roll);

  cy := cos(y * 0.5);   sy := sin(y * 0.5);    // memoise trigs
  cp := cos(p * 0.5);   sp := sin(p * 0.5);
  cr := cos(r * 0.5);   sr := sin(r * 0.5);

  q.RealPart   := cr * cp * cy + sr * sp * sy; // mk quaternion
  q.ImagPart.x := sr * cp * cy - cr * sp * sy;
  q.ImagPart.y := cr * sp * cy + sr * cp * sy;
  q.ImagPart.z := cr * cp * sy - sr * sp * cy;
end;

{ TControl3DHelper }

procedure TControl3DHelper.SetMatrix(const M: TMatrix3D);
begin
  FLocalMatrix := M;
  RecalcAbsolute;
  RebuildRenderingList;
  Repaint;
end;

end.

