unit u_curvestest;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, curves, ExtCtrls, StdCtrls;

type
  TForm1 = class(TForm)
    PaintBox1: TPaintBox;
    Memo1: TMemo;
    procedure PaintBox1Paint(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.PaintBox1Paint(Sender: TObject);
var
	testsegment: TCurveSegment;
	outpoints: TCurveSegment;
	i: integer;
	segment: integer;
	len: single;
const
	zoom = 100;
begin

	setlength(testsegment, 8);

	testsegment[0]:= makevector2(1,0.5);
	testsegment[1]:= makevector2(2,2);
	testsegment[2]:= makevector2(3,1);
	testsegment[3]:= makevector2(4,0.5);
	testsegment[4]:= makevector2(5,1);
	testsegment[5]:= makevector2(6,2);
	testsegment[6]:= makevector2(7,5);
	testsegment[7]:= makevector2(7,7);

	Memo1.lines.clear;

	PaintBox1.canvas.pen.Color:= clblack;
	
	for segment:= 0 to high(testsegment) - 3 do begin

		len:= interpolateCurve(testsegment, segment, outpoints, 10, true); // get 10 points on the curve segment
		
		Memo1.lines.add(format('segment %d, length %0.2f', [ segment, len ]));
		
		for i:= 0 to high(outpoints) do begin

			Memo1.lines.add(format('%d %d', [ round(outpoints[i].x * zoom), round(outpoints[i].y * zoom) ]));

			if (i = 0) and (segment = 0) then
				PaintBox1.Canvas.MoveTo(round(outpoints[i].x * zoom), round(outpoints[i].y * zoom))
			else
				PaintBox1.Canvas.LineTo(round(outpoints[i].x * zoom), round(outpoints[i].y * zoom));	

			PaintBox1.Canvas.Rectangle(
				round(outpoints[i].x * zoom) - 3,
				round(outpoints[i].y * zoom) - 3,
				round(outpoints[i].x * zoom) + 3,
				round(outpoints[i].y * zoom) + 3
			);
				
		end;

	end;

	

	// render control points
	for i:= 0 to high(testsegment) do begin
	
		if (i = 0) or (i = high(testsegment)) then
			PaintBox1.canvas.pen.Color:= clRed
		else
			PaintBox1.canvas.pen.Color:= clblue;
	
		PaintBox1.Canvas.Rectangle(
			round(testsegment[i].x * zoom) - 3,
			round(testsegment[i].y * zoom) - 3,
			round(testsegment[i].x * zoom) + 3,
			round(testsegment[i].y * zoom) + 3
		);
	
	end;

end;

end.

