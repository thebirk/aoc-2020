with Ada.Strings.Unbounded;
with Ada.Text_IO; use Ada;
with GNAT.Regpat; use GNAT;

procedure Day02 is
    type Input_Index is range 0..999;
    type Input_Array is array (Input_Index) of Strings.Unbounded.Unbounded_String;

    type Line is record
        Low: Integer;
        High: Integer;
        Char: Character;
        Password: Strings.Unbounded.Unbounded_String;
    end record;
    
    Entry_Matcher: Regpat.Pattern_Matcher := Regpat.Compile("([0-9]+)-([0-9]+) ([a-z]): ([a-z]+)");

    procedure Get_Input (A : in out Input_Array) is
		I    : Input_Index := 0;	
		File : Text_IO.File_Type;
	begin
		Text_IO.Open (File => File,
		          Mode => Text_IO.In_File,
		          Name => "input.txt");
		while not Text_IO.End_Of_Line (File) loop
			A (I) := Ada.Strings.Unbounded.To_Unbounded_String( Text_IO.Get_Line (File) );
			exit when I = Input_Index'Last;
			I := I + 1;
		end loop;
	end;

    function Get_Entry(Input: Input_Array; Index: Input_Index) return Line is
        use Strings.Unbounded;
        Matches: Regpat.Match_Array(0 .. Regpat.Paren_Count(Entry_Matcher));
    begin
        Regpat.Match(Entry_Matcher, To_String( Input(Index) ), Matches);

        return (
            Low => Integer'Value(To_String(Unbounded_Slice(
                Source => Input(Index),
                Low => Matches(1).First,
                High => Matches(1).Last
            ))),
            High => Integer'Value(To_String(Unbounded_Slice(
                Source => Input(Index),
                Low => Matches(2).First,
                High => Matches(2).Last
            ))),
            Char => Element(Unbounded_Slice(
                Source => Input(Index),
                Low => Matches(3).First,
                High => Matches(3).Last
            ), 1),
            Password => Unbounded_Slice(
                Source => Input(Index),
                Low => Matches(4).First,
                High => Matches(4).Last
            )
        );
    end;

    Input: Input_Array;
    E: Line;
begin
    Get_Input(Input);

    Text_IO.Put_Line("hello " & Strings.Unbounded.To_String( input(14) ));

    E := Get_Entry(Input, 14);
    Text_IO.Put_Line("Low:" & Integer'Image(E.Low));
    Text_IO.Put_Line("High:" & Integer'Image(E.High));
    Text_IO.Put_Line("Char:" & E.Char);
    Text_IO.Put_Line("Password:" & Strings.Unbounded.To_String(E.Password));
end Day02;
