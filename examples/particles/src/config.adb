--  ************************************************************************ --
--  *****                       Collaboration                          ***** --
--  *****               A simple parallelism framework                 ***** --
--  ************************************************************************ --
--
--  Copyright (C) 2013 Michael Erdmann
--
--  Collaboration is free software; you can redistribute it and/or modify it
--  under terms of the  GNU General Public License as published  by the Free
--  Software Foundation;  either version 2,  or (at your option) any later
--  version.  Tasklet is distributed in the hope that it will be useful, but
--  WITHOUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY
--  or FITNESS FOR A PARTICULAR PURPOSE.
--  See the GNU General Public License for more details. You should have
--  received  a copy of the GNU General Public License distributed with GNAT;
--  see file COPYING.  If not, write to  the Free Software Foundation,
--  59 Temple Place - Suite 330,  Boston, MA 02111-1307, USA.                                                      --
--
--  As a special exception,  if other files  instantiate  generics from this
--  unit, or you link  this unit with other files  to produce an executable,
--  this  unit  does not  by itself cause  the resulting  executable  to  be
--  covered  by the  GNU  General  Public  License.  This exception does not
--  however invalidate  any other reasons why  the executable file  might be
--  covered by the  GNU Public License.
--  You Should Have Received A Copy of The GNU General Public License
--  Along with This Program.  If not, See <Http://Www.Gnu.Org/Licenses/>.
--

with Ada.Text_IO;                use Ada.Text_IO;
with Parameter_File;             use Parameter_File;

with Log;                        use Log;
with Ada.Numerics.Float_Random;  use  Ada.Numerics.Float_Random;

with Ada.Numerics.Generic_Elementary_Functions;
use  Ada.Numerics;

package body Config is

   package Numerics is new Generic_Elementary_Functions( Value_Type );
   use Numerics;

   function Parameter is new Range_Parameter( Parameter_Type => Integer );
   function Parameter is new Digits_Parameter( Parameter_Type => Float );
   function Parameter is new Digits_Parameter( Parameter_Type => Value_Type );

   type Init_Mode_Type is ( Random, Predefined );

   function Parameter is new Enumeration_Parameter( Parameter_Type => Init_Mode_Type );

   -- In case of the init mode = Random the initial locations of the particles are
   -- calculated by means of the a random generator
   G               : Generator;        -- random generator

   Params          : Parameter_File.Object;

   CFG_Workers     : constant Natural := Register( Params, "Workers" );
   CFG_Particles   : constant Natural := Register( Params, "N");
   CFG_DT          : constant Natural := Register( Params, "DT");
   CFG_RS          : constant Natural := Register( Params, "RS");

   CFG_Iterations  : constant Natural := Register( Params, "Iterations");
   CFG_TS          : constant Natural := Register( Params, "TS");

   -- configuration variables
   CFG_Unit        : constant Natural := Register( Params, "Unit");
   CFG_M0          : constant Natural := Register( Params, "M0");
   CFG_MMAX        : constant Natural := Register( Params, "MMAX");
   CFG_Report      : constant Natural := Register( Params, "Report");
   CFG_Init_Mode   : constant Natural := Register( Params, "Init_Mode");

   CFG_P           : constant array(1..10) of Natural :=
     (others => Register_Array(Params,"P",10));

   Seconds_Per_Day : constant Natural := 24 * 3600;

   Init_Mode       : Init_Mode_Type := Random;

   ------------------------
   -- Get_Particle_State --
   ------------------------
   procedure Get_Particle_State(
      Line   : in String; T : out Value_Type; PP : out Particle_Type ) is
      -- load a complete state vector from the dump format
      Length : constant Natural := Line'Length;
      P      : Natural := Line'First;

      Item  : String( 1..20 );

      procedure Get_Field( Item : out String ) is
         I : Natural := Item'First;
      begin
         Item := (others => ' ');

         while P < Length and Line(P) /= ';' and Line(P) /= ',' loop
            Item(I) := Line(P);
            I := I + 1;
            P := P + 1;
         end loop;

         P := P + 1;
      end Get_Field;

   begin
      Get_Field(Item);   T    := Value_Type'Value(Item);
      Get_Field(Item);   PP.Mass := Value_Type'Value(Item);
      Get_Field(Item);   PP.X(1) := Value_Type'Value(Item);
      Get_Field(Item);   PP.X(2) := Value_Type'Value(Item);
      Get_Field(Item);   PP.X(3) := Value_Type'Value(Item);
      Get_Field(Item);   PP.V(1) := Value_Type'Value(Item);
      Get_Field(Item);   PP.V(2) := Value_Type'Value(Item);
      Get_Field(Item);   PP.V(3) := Value_Type'Value(Item);
   end Get_Particle_State;

   ------------------
   -- Get_Particle --
   ------------------
   procedure Get_Particle(
      Id : in Integer;
      T  : out Value_Type;
      S  : out Particle_Type ) is
   begin
      Get_Particle_State(Parameter(Params, CFG_P(id) ), T, S);
   end Get_Particle;

   ----------------
   -- Initialize --
   ----------------
   procedure Initialize( Name : in String ) is
   begin
      Load( Params, Name );
      List_Parameters(Params);

      N  := Parameter( Params, Cfg_Particles, 20 );
      DT := Parameter( Params, CFG_DT, 0.1);

      Init_Mode := Parameter( Params, CFG_Init_Mode, Random );

      Unit := Parameter( Params, Cfg_Unit, 100.00);
      M0   := Parameter( Params, Cfg_M0, 1.0);
      MMAX := Parameter( Params, Cfg_MMAX, 3);

      RS  := Parameter( Params, Cfg_RS, 0.0);

      -- In case the TS parameter is set the report interval indicates only the number
      -- reports to be generated.
      if Is_Parameter_Set( Params, CFG_TS ) then
         declare
            TS : constant Value_Type := Parameter( Params, Cfg_TS, 1.0 );
         begin
            Log.Comment("TS is set");
            Max_Iterations  := Integer( TS * Value_Type(Seconds_Per_Day) / DT );
            Report_Interval := Max_Iterations / Parameter( Params, Cfg_Report, 1000 );
         end;
      else
         Report_Interval := Parameter( Params, Cfg_Report, 20 );
         Max_Iterations := Parameter( Params, Cfg_Iterations, 1000 );
      end if;

      -- if not in config file the platform will select
      Workers := Parameter( Params, Cfg_Workers, 0);

      Log.Comment( "Report_Interval=" & Natural'Image(Report_Interval) );
      Log.Comment( "Max_Iterations=" & Natural'Image(Max_Iterations) );
   end Initialize;

   -------------------
   -- Restore_State --
   -------------------
   procedure Restore_State(
      Name : in String; X : in State_Vector_Access; T : out Value_Type ) is
      -- load a complete state vector from the dump format
      File_Name : constant String := Name & ".init" ;
      Input     : File_Type;

      Line      : String( 1..1024 );
      Length    : Natural := 0;
   begin
      Open( File => Input, Name => File_Name, Mode => In_File );

      for I in 1..N loop
         exit when End_Of_File(Input);

         Get_Line(Input, Line, Length );
         Get_Particle_State( Line(1..Length), T, X(I) );
      end loop;

      Close( Input );

   exception
      when others =>
         T := 0.0;
   end Restore_State;

   -----------------------
   -- Initial_Condition --
   -----------------------

   procedure Setup_Initial_Values(
      State_File    : in String;
      Initial_State : in State_Vector_Access ) is
   begin
      Reset(G);

      if State_File /= "" then
         Restore_State( State_File, Initial_State, T );
      end if;
      Comment( "Starting Time:" & Value_Type'Image(T));

      if T = 0.0 then
         -- No state has been loaded; setup the initial conditions
         case Init_Mode is
            -- ........................................................
            when Random =>
               for I in 1..N loop
                  Initial_State(I).X(1) := Value_Type(Unit * (0.5-Random(G)));
                  Initial_State(i).X(2) := Value_Type(Unit * (0.5-Random(G)));
                  Initial_State(I).X(3) := Value_Type(Unit * (0.5-Random(G)));
                  Initial_State(I).V    := Null_Vector;
                  Initial_State(I).Mass := Value_Type(M0 * Float(Random(G))*Float(MMAX));
               end loop;
            -- ........................................................
            when Predefined =>
               for I in 1..N loop
                  Get_Particle_State(Parameter(Params, CFG_P(I),""), T, Initial_State(I) );
               end loop;
         end case;
      end if;

   end Setup_Initial_Values;

end ;
