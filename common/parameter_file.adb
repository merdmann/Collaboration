with Ada.Text_IO;                       use Ada.Text_IO;
--with Ada.Characters.Handling;           use Ada.Characters.Handling;
with Ada.Strings;                       use Ada.Strings;
with Ada.Strings.Unbounded;             use Ada.Strings.Unbounded;
with Ada.Unchecked_Deallocation;


package body Parameter_File is

   type AVP_Type is record
         Attribute  : Unbounded_String := Null_Unbounded_String;
         Value      : Unbounded_String := Null_Unbounded_String;
   end record;

   type Object_Table_Type is array(1..1000) of AVP_Type;

   type Object_Data_Type is record
         Next  : Natural := 0;
         OT    : Object_Table_Type;
         Count : Natural := 0;
      end record;

   procedure Free is new Ada.Unchecked_Deallocation(
      Object_Data_Type, Object_Data_Access);

   ----------------
   -- Initialize --
   ----------------
   procedure Initialize( This : in out Object ) is
      Data : constant Object_Data_Access := new Object_Data_Type;
   begin
      Data.Next := 0;
      This.Data := Data;
   end Initialize;

   --------------
   -- Finalize --
   --------------
   procedure Finalize( This : in out Object ) is
   begin
      Free( This.Data );
   end Finalize;

   ----------------
   -- Array_Name --
   ----------------
   function Array_Name(Name : in String; I : in Natural) return String is
      N : constant String := Natural'Image(I);
   begin
      return Name & N(2..N'Last) ;
   end Array_Name;

   --------------
   -- Register --
   --------------
   function Register(
      This  : in Object;
      Name  : in String ) return Natural is
      Data  : Object_Data_Access renames This.Data;
      Id    : constant Natural := Data.Next + 1;
   begin
      Data.OT(Id).Attribute := To_Unbounded_String(Name);
      Data.Next := Id;
      return id;
   end Register;

   --------------
   -- Register --
   --------------
   function Register_Array(
      This   : in Object;
      Name   : in String;
      Length : in Natural ) return Natural is
      Data   : Object_Data_Access renames This.Data;
      Id     : Natural;
   begin
      if Data.Count < Length then
         Data.Count := Data.Count + 1;
         Id := Register(This, Array_Name(Name,Data.Count) );
      else
         Data.Count := 0;
      end if;

      return Id;
   end Register_Array;

   -----------
   -- Value --
   -----------
   function Value( This  : in Object; Id : in Natural) return String is
      Data  : Object_Data_Access renames This.Data;
   begin
      return To_String(Data.OT(Id).Value);
   end Value;

   ----------
   -- Load --
   ----------
   procedure Load( This : in out Object; Name : in String ) is
      Cfg  : File_Type;
      Data : Object_Data_Access renames This.Data;

      -- get the parameter name in a line
      function Get_Name(
         Src    : in String ) return Unbounded_String is
         Result : Unbounded_String := Null_Unbounded_String;
      begin
         for I in 1..Src'Length loop
            exit when Src(I) = '=' or Src(I) = '#';
            if Src(I) < ' ' then
               Result := Result & " ";
            else
               Result := Result & Src(I);
            end if;
         end loop;

         return Trim( Trim( Result, Left), Right );
      end Get_Name;

      -- get the value in a line
      function Get_Value(
         Src    : in String ) return Unbounded_String is

         Result : Unbounded_String := Null_Unbounded_String;
         Start  : constant Natural := Index( To_Unbounded_String(Src), "=" ) + 1;
      begin
         if Start > 1 then
            for I in Start..Src'Length loop
               exit when ( Src(I) = '#' or Src(I) = ';' );

               if Src(I) < ' ' then
                  Result := Result & " ";
               else
                  Result := Result & Src(I);
               end if;
            end loop;
            return Trim( Trim( Result, Left ), Right );
         end if;

         return Null_Unbounded_String;
      end Get_Value;

      procedure Process_Line(S : in String ) is
         Attribute : Unbounded_String;
         Value     : Unbounded_String;
      begin
         Attribute := Get_Name( S );
         Value     := Get_Value( S );

         for I in Data.OT'Range loop
            if Attribute = Data.OT(I).Attribute then
               Data.OT(I).Value := Value;
               exit;
            end if;
         end loop;

      end Process_Line;
   begin
      Open( File => Cfg, Name => Name & ".cfg", Mode => In_File );

      while not End_Of_File(Cfg) loop
         declare
            Line   : String( 1..200);
            Length : Natural := 0;
         begin
            Get_Line(Cfg, Line, Length );

            if length > 0 and then Line(1) /= '#' then
               Process_Line( Line(1..Length) );
            end if;
         end;
      end loop;

      Close( Cfg );

   end Load;

   ---------------------
   -- List_Parameters --
   ---------------------
   procedure List_Parameters(
      This : in Object ) is
      Data : Object_Data_Access renames This.Data;
   begin
      for I in Data.OT'Range loop
         declare
            Attribute : String renames To_String(Data.OT(I).Attribute);
            Value     : String renames To_String(Data.OT(I).Value);
         begin
            if Attribute /= "" then
               Put_Line( "# " & Attribute & "=" & Value );
            end if;
         end;
      end loop;
   end List_Parameters;

   ----------------------
   -- Is_Parameter_Set --
   ----------------------
   function Is_Parameter_Set(
      This : in Object; Id: in Natural ) return Boolean is
      S    : constant String := Value( This, Id);
   begin
      return S /= "";
   end Is_Parameter_Set ;

   ---------------------
   -- Range_Parameter --
   ---------------------
   function Range_Parameter(
      This    : in Object;
      Id      : in Natural;
      Default : in Parameter_Type ) return Parameter_Type is
      S       : constant String := Value( This, Id);
   begin
      if S /= "" then
         return Parameter_Type'Value(S);
      else
         return Default;
      end if;
   end Range_Parameter;

   ----------------------
   -- Digits_Parameter --
   ----------------------
   function Digits_Parameter(
      This    : in Object;
      Id      : in Natural;
      Default : in Parameter_Type ) return Parameter_Type is
      S       : constant String := Value( This, Id);
   begin
      if S /= "" then
         return Parameter_Type'Value(S);
      else
         return Default;
      end if;
   end Digits_Parameter;

   ----------------------
   -- Digits_Parameter --
   ----------------------
   function Enumeration_Parameter(
      This    : in Object;
      Id      : in Natural;
      Default : in Parameter_Type ) return Parameter_Type is
      S       : constant String := Value( This, Id);
   begin
      if S /= "" then
         return Parameter_Type'Value(S);
      else
         return Default;
      end if;
   end Enumeration_Parameter;

   ---------------
   -- Parameter --
   ---------------
   function Parameter(
      This    : in Object;
      Id      : in Natural;
      Default : in String := "") return String is

      S    : constant String := Value( This, Id);
   begin
      if S /= "" then
         return S;
      else
         return Default;
      end if;
   end Parameter ;

end Parameter_File;
