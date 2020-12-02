with Ada.Strings.Unbounded;
with Ada.Text_IO; use Ada;
with GNAT.Regpat; use GNAT;

procedure Day02 is
    Entry_Matcher: Regpat.Pattern_Matcher := Regpat.Compile("([0-9]+)-([0-9]+) ([a-z]): ([a-z]+)");

    type Input_Index is range 0..999;
    type Input_Array is array (Input_Index) of Strings.Unbounded.Unbounded_String;

    type Line is record
        Low: Integer;
        High: Integer;
        Char: Character;
        Password: Strings.Unbounded.Unbounded_String;
    end record;
    
    procedure Get_Input (A : in out Input_Array) is
		I    : Input_Index := 0;	
		File : Text_IO.File_Type;
	begin
		Text_IO.Open (File => File,
		          Mode => Text_IO.In_File,
		          Name => "input.txt");
		while not Text_IO.End_Of_Line (File) loop
			A (I) := Strings.Unbounded.To_Unbounded_String( Text_IO.Get_Line (File) );
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

    procedure Part1(Input: Input_Array) is
        use Strings.Unbounded;
        Item: Line;
        Valid_Total: Integer := 0;
        Character_Total: Integer := 0;
    begin
        for Index in Input'Range loop
            Character_Total := 0;

            Item := Get_Entry(Input, Index);

            for Password_Index in 1..Length(Item.Password) loop
                if Element(Item.Password, Password_Index) = Item.Char then
                    Character_Total := Character_Total + 1;
                end if;
            end loop;
                
            if Character_Total >= Item.Low and Character_Total <= Item.High then
                Valid_Total := Valid_Total + 1;
            end if;
        end loop;

        Text_IO.Put_Line("part 1 answer:" & Integer'Image(Valid_Total));
    end;

    procedure Part2(Input: Input_Array) is
        use Strings.Unbounded;
        Item: Line;
        Valid_Total: Integer := 0;
        Character_Total: Integer := 0;
    begin
        for Index in Input'Range loop
            Character_Total := 0;

            Item := Get_Entry(Input, Index);

            if Element(Item.Password, Item.Low) = Item.Char then
                Character_Total := Character_Total + 1;
            end if;

            if Element(Item.Password, Item.High) = Item.Char then
                Character_Total := Character_Total + 1;
            end if;

            if Character_Total = 1 then
                Valid_Total := Valid_Total + 1;
            end if;
        end loop;

        Text_IO.Put_Line("part 2 answer:" & Integer'Image(Valid_Total));
    end;

    Input: Input_Array;
begin
    Get_Input(Input);
    Part1(Input);
    Part2(Input);
end Day02;
