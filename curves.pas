// Centripetal Catmul-Rom, other variants seem pretty useless, centripetal variant produces no self intresections and seems to follow control points most closely.
// https://en.wikipedia.org/wiki/Centripetal_Catmull%E2%80%93Rom_spline
// Copyright 2018 Jernej L, based on wikipedia's c# example code which is assumed to be in public domain.

unit curves;

{$ifdef fpc}
{$mode delphi}
{$endif}

interface

uses math;

const alpha = 0.5; //set from 0-1

type

Vector2 = packed record
	x, y: single;
end;

  TCurveSegment = array of Vector2;

function Makevector2(const x, y: single): Vector2; {$ifdef fpc} inline; {$endif}
function interpolateCurve(const points: TCurveSegment; start_segment: integer; var outpoints: TCurveSegment; amountOfPoints: integer; add_last_point: boolean): single;

// todo: to calculate approximate length quickly, get the first and last point, and 1 point inbetween and then use https://en.wikipedia.org/wiki/Arc_length

implementation

uses SysUtils;

function Makevector2(const x, y: single): Vector2; {$ifdef fpc} inline; {$endif}
begin

  result.x:= x;
  result.y:= y;

end;

function GetT(t: single; p0: Vector2; p1: Vector2): single;
var
    a, b, c: single;
begin
    a := power((p1.x - p0.x), 2.0) + power((p1.y - p0.y), 2.0);
    b := power(a, 0.5);
    c := power(b, alpha);

    result := (c + t);

end;

(*
points			- array with control points, remember that previous and next points are automatic control points for the inbetween segment.
start_segment	- start segment number, an array can never use first an last control point to interpolate on, so segment 1 = start_segment index 0.
outpoints		- var out parameter where your points will be returned.
amountOfPoints	- number of desired output points (see remarks below, you might not always get amout you wish)
add_last_point	- add last point of spline to outputs array - if you are interpreting data in a sequence, you might not want the last point as it's also the next point of next segment (and you will have same point twice)
					but if you are rendering the points or line, you might want points for a specific segment from start to finish.

result			- segment's actual length.
*)

function interpolateCurve(const points: TCurveSegment; start_segment: integer; var outpoints: TCurveSegment; amountOfPoints: integer; add_last_point: boolean): single;
var
    t0, t1, t2, t3: single;
    segment_length: single;
    t: single;
	A1, A2, A3, B1, B2, C: Vector2;
	first_point_index: integer;
	first_curve_point: integer;
	last_curve_point: integer;
	last_point_idx: integer;
	pointidx: integer;
begin

	first_point_index:= 0 + start_segment;
	first_curve_point:= 1 + start_segment;
	last_curve_point:= 2 + start_segment;
	last_point_idx:= 3 + start_segment;

	t0 := 0.0;
	t1 := GetT(t0, points[first_point_index], points[first_curve_point]);
	t2 := GetT(t1, points[first_curve_point], points[last_curve_point]);	
	t3 := GetT(t2, points[last_curve_point], points[last_point_idx]);

    segment_length := ((t2 - t1) / amountOfPoints);

	t := t1;

	setlength(outpoints, amountOfPoints + 1); // expected amout of points (it might not be actual amout in the end, it depends on your curve control points.)
	pointidx:= 0;

    while (true) do
	begin
	
		A1.x := (t1 - t) / (t1 - t0) * points[first_point_index].x + (t - t0) / (t1 - t0) * points[first_curve_point].x;
		A1.y := (t1 - t) / (t1 - t0) * points[first_point_index].y + (t - t0) / (t1 - t0) * points[first_curve_point].y;

		A2.x := (t2 - t) / (t2 - t1) * points[first_curve_point].x + (t - t1) / (t2 - t1) * points[last_curve_point].x;
		A2.y := (t2 - t) / (t2 - t1) * points[first_curve_point].y + (t - t1) / (t2 - t1) * points[last_curve_point].y;

		A3.x := (t3 - t) / (t3 - t2) * points[last_curve_point].x + (t - t2) / (t3 - t2) * points[last_point_idx].x;
		A3.y := (t3 - t) / (t3 - t2) * points[last_curve_point].y + (t - t2) / (t3 - t2) * points[last_point_idx].y;

		B1.x := (t2 - t) / (t2 - t0) * A1.x + (t - t0) / (t2 - t0) * A2.x;
		B1.y := (t2 - t) / (t2 - t0) * A1.y + (t - t0) / (t2 - t0) * A2.y;

		B2.x := (t3 - t) / (t3 - t1) * A2.x + (t - t1) / (t3 - t1) * A3.x;
		B2.y := (t3 - t) / (t3 - t1) * A2.y + (t - t1) / (t3 - t1) * A3.y;

		C.x := (t2 - t) / (t2 - t1) * B1.x + (t - t1) / (t2 - t1) * B2.x;
		C.y := (t2 - t) / (t2 - t1) * B1.y + (t - t1) / (t2 - t1) * B2.y;

		// Add point		
		outpoints[pointidx] := C;
		pointidx:= pointidx + 1; // next point

		t := t + segment_length;

		if not (t < t2) then begin
		
			// add last point.
			if add_last_point = true then begin
				setlength(outpoints, pointidx + 1);
				outpoints[pointidx] := points[last_curve_point];
			end else begin
			    setlength(outpoints, pointidx); // final
			end;
			
			break;
			
		end;

	end;

	result:= t;

end;

initialization

set8087CW($133F);

end.

