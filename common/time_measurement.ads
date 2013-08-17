package Time_Measurement is

   type Clock_Id_Type is new Natural range 0..20;

   Clock_Id_Null : constant Clock_Id_Type := 0;

   function Register( Name : in String ) return Clock_Id_Type;

   procedure Start_Lap( Id : Clock_Id_Type );
   procedure Stop_Lap( Id : in Clock_Id_Type );
   procedure Cancel_Lap(Id : in Clock_Id_Type );
   function To_String( Id : in Clock_Id_type ) return String;
   procedure Update_Summary;

end Time_Measurement;
