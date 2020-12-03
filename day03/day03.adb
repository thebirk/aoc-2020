with Ada.Text_IO;
use Ada;

procedure day03 is
    type Row is range 0..323;
    type Column is mod 31;

    type Element is (Open, Tree);
    type Map is array(Row, Column) of Element;

    function Get_Input return Map is
        File: Text_IO.File_Type;
        Result: Map;
        Current_Row: Row := 0;
        Row_String: String(1..31);
        Column_Element: Element;
    begin
        Text_IO.Open (File => File,
                  Mode => Text_IO.In_File,
                  Name => "input.txt");
        while not Text_IO.End_Of_Line (File) loop
            Row_String := Text_IO.Get_Line (File);

            for Column_Index in Row_String'Range loop
                case Row_String(Column_Index) is
                    when '.' => Column_Element := Open;
                    when '#' => Column_Element := Tree;
                    when others => raise Constraint_Error;
                end case;
                Result(Current_Row, Column(Column_Index-1)) := Column_Element;
            end loop;

            exit when Current_Row = Row'Last;
            Current_Row := Current_Row + 1;
        end loop;

        return Result;
    end;

    procedure Part1(Input: Map) is
        X: Column := 0;
        Y: Row := 0;
        Tree_Count: Integer := 0;
    begin
        loop
            X := X + 3;
            Y := Y + 1;

            if Input(Y, Column(X)) = Tree then
                Tree_Count := Tree_Count + 1;
            end if;

            exit when Y = Row'Last;
        end loop;

        Text_IO.Put_Line("part 1 answer:" & Integer'Image(Tree_Count));
    end;

    function Test_Slope(Input: Map; XSlope: Column; YSlope: Row) return Long_Long_Integer is
        X: Column := 0;
        Y: Row := 0;
        Tree_Count: Long_Long_Integer := 0;
    begin
        loop
            exit when Integer(Y) + Integer(YSlope) > Integer(Row'Last);

            X := X + XSlope;
            Y := Y + YSlope;

            if Input(Y, X) = Tree then
                Tree_Count := Tree_Count + 1;
            end if;
        end loop;

        return Tree_Count;
    end;

    procedure Part2(Input: Map) is
    begin
        Text_IO.Put_Line("part 2 answer:" & Long_Long_Integer'Image(
            Test_Slope(Input, 1, 1) *
            Test_Slope(Input, 3, 1) *
            Test_Slope(Input, 5, 1) *
            Test_Slope(Input, 7, 1) *
            Test_Slope(Input, 1, 2)
        ));
    end;

    Input: Map;
begin
    Input := Get_Input;
    Part1(Input);
    Part2(Input);
end;