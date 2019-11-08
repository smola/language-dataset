package body Commands is

    function Construct_Date_Time (D : Day; H : Hour; M : Minute) return Comm_Rec
    is
        SData : Comm_Rec (Set_Day_Time);
    begin
        SData.Dy := Day'Pos (D);
        SData.Hr := H;
        SData.Min := M;
        return SData;
    end Construct_Date_Time;

    function Construct_Drive_Special (Special : Drive_Special;
                                      V       : Velocity)
                                      return Comm_Rec
    is
        SData : Comm_Rec (Drive);
    begin
        case Special is
        when Straight =>
            SData.Rad.Value := 32767;
        when CW =>
            SData.Rad.Value := -1;
        when CCW =>
            SData.Rad.Value := 1;
        end case;
        SData.Vel.Value := V;
        return SData;
    end Construct_Drive_Special;


end Commands;
