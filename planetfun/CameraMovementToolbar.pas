unit CameraMovementToolbar;

interface

// maps clicks on a image containing camera manipulation buttons
// see file /vsop2013/PlanetFun/Images/CameraManipulationMenu.png
// virtual button centers mapped using Windows Paint app:
// ix  btn           center
// 0 rot left         80,80
// 1 rot right       300,80
// 2 move up         200,180
// 3 move right      300,290
// 4 move down       195,390
// 5 move left        80,290
// 6 plus move far    64,440
// 7 minus move near 330,440

function toolbarCameraManipulation_mapClickToButton(const x,y:Single):integer; //get index of btn clicked

implementation

uses
  System.Types;

function toolbarCameraManipulation_mapClickToButton(const x,y:Single):integer; //get index of btn clicked
var i:integer; btns:Array[0..7] of TPointF; dmin,d,dx,dy:single;
begin
  btns[0] := PointF( 80,80 );
  btns[1] := PointF(300,80 );
  btns[2] := PointF(200,180);
  btns[3] := PointF(300,290);
  btns[4] := PointF(195,390);
  btns[5] := PointF( 80,290);
  btns[6] := PointF( 64,440);
  btns[7] := PointF(330,440);
  Result := -1;                //-1 = none
  dmin   := 500*500+400*400;   // dmin starts w/ image size squared
  for i:= 0 to 7 do   // search btn nearest to x,y
     begin
       dx := btns[i].X-x;
       dy := btns[i].Y-y;
       d  := dx*dx+dy*dy;     // dist^2
       if (d<dmin) then
         begin
           Result := i;
           dmin   := d;
         end;
     end;
end;

end.
