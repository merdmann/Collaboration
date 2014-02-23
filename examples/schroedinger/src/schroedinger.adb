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
--
with Ada.Text_IO;             	use Ada.Text_IO;
with Ada.Numerics;            	use Ada.Numerics;
with Ada.Exceptions;          	use Ada.Exceptions;
with Ada.Command_Line;        	use Ada.Command_Line;
with Ada.Numerics.Generic_Elementary_Functions;
with Ada.Numerics.Generic_Complex_Types;
with Ada.Numerics.Generic_Complex_Elementary_Functions;


with Vector_Space;            	use Vector_Space;
with Types;                   	use Types;
with Config;                  	use Config;
with Log;                     	use Log;
with Time_Measurement;        	use Time_Measurement;
with Timers;                  	use Timers;
with Partitioned_Workpackage; 	use Partitioned_Workpackage;
with Partitioned_Data;	 	use Partitioned_Data;

with Computation;		use Computation;
with Types;			use Types;

procedure Schroedinger is
   package Complex_Functions is new
     Ada.Numerics.Generic_Complex_Elementary_Functions (Types.Complex_Types);
   use Complex_Functions;

   package Numerics is new Generic_Elementary_Functions( Value_Type );
   use Numerics;

   use Types.Complex_Types;

   Step : Natural := 1;

   ----------------
   -- Dump_State --
   ----------------
   procedure Dump_State(
      T : in Value_Type;
      X : in State_Vector_Access) is
   begin
      for i in X'Range loop
         declare
            X1 : Value_Type := Value_Type(I mod N) * DX;
            X2 : Value_Type := Value_Type(Integer(I / N)) * DX;
            X3 : Value_Type := Value_Type(Integer(I / N**2))* DX;
         begin
            Put_Line(  Value_Type'Image(T) & ";"
                     & Value_Type'Image(X1) & ";"
                     & Value_Type'Image(X2) & ";"
                     & Value_Type'Image(X3) & ";"
                     & Value_Type'Image(Re(X(i).Phi)) & ";"
                     & Value_Type'Image(Im(X(i).Phi))
            );
         end;
      end loop;
   end Dump_State;

   ----------------
   -- Dump_State --
   ----------------
   procedure Initial_State(
      X : in out State_Vector_Access) is
   begin
      for i in X'Range loop
         declare
            X1 : Value_Type := Value_Type(I mod N) * DX;
            X2 : Value_Type := Value_Type(Integer(I / N)) * DX;
            X3 : Value_Type := Value_Type(Integer(I / N**2))* DX;
         begin
            Set_Re( X(i).Phi, 0.0 );
            Set_Im( X(i).Phi, 0.0 );
         end;
      end loop;
   end Initial_State;

   RC : Integer := 0;

begin
   if Argument_Count < 1 then
      Put_Line("usage: particle config state");
      return;
   end if;

   Log.Open( Argument(1) );

   -- +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ --
   -- ++++++++++++++++++     load the configuration       +++++++++++++++++ --
   -- +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ --
   Config.Initialize( Argument(1) );
   -- +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ --
   -- ++++++++++++++++++       Run Calculation            +++++++++++++++++ --
   -- +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ --
   Log.Comment("Max_Iterations=" & Natural'Image(Max_Iterations));

   declare
      C : Computation.Handle := new Computation.Object_Type(N);
      P : Partitioned_Workpackage.Object_Type(Config.Workers);
   begin
      C.X1 := new State_Vector_Type(1..N);
      C.X2 := new State_Vector_Type(1..N);

      Dump_State( T, C.X1 );

      Start_Lap( T_Total );
      while Step < Max_Iterations loop
         Compute( P, Partitioned_Data.Handle(C), Report_Interval );
         T := T + Value_Type(Report_Interval) * DT;

         Dump_State(T, C.X1 );

         Step := Step + Report_Interval;
      end loop;
      Stop_Lap( T_Total );
      Done(P);

      --if Argument_Count > 2 then
      --   Comment( "Saving State for " & Value_Type'Image(T));
      --   Save_State( Argument(3), C.X2, T );
      --end if;
   end;

   pragma Debug( Log.Comment("Particle.Main:Done"));

   -- dump the time measurements
   Update_Summary;
   Log.Comment( To_String( T_Total ));
   Log.Comment( To_String( T_Iteration ));
   Log.Comment( To_String( T_Wait_For_Workers ));
   Log.Comment( To_String( T_Wait_For_Ready ));
   Log.Comment( To_String( T_Processing ));
   Log.Comment( To_String( T_Vector ));
   Log.Comment( To_String( T_Forces ));
   Log.Comment( To_String( T_Integration ));

   Log.Comment( To_String( T_Output ));
   Log.Comment( To_String( T_Sleep ));

   Log.Close;

   Log.Comment("Particle.Main: done " & Natural'Image(Error_Counter));
exception
   when E : others =>
      Error("Exception *** " & Exception_Name( E ) & ":" & Exception_Message( E ));
      Log.Close;
end Schroedinger;
