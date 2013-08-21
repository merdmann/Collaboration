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
with Ada.Text_IO;			use Ada.Text_IO;
with Ada.Exceptions;                   	use Ada.Exceptions;
with Ada.Numerics.Generic_Elementary_Functions;
with Unchecked_Deallocation;

with Log;
with Config;                           	use Config;
with Vector_Space;                     	use Vector_Space;

with Time_Measurement;                 	use Time_Measurement;
with Timers;                           	use Timers;

package body Computation is

   package Value_Functions is new Ada.Numerics.Generic_Elementary_Functions (
     Value_Type);
   use Value_Functions;

   -------------
   -- Compute --
   -------------
   procedure Compute( This : in out Object_Type; I : in Natural ) is
      X1   : State_Vector_Access renames This.X1;
      X2   : State_Vector_Access renames This.X2;

      function To_String( State : in Particle_Type ) return String is
      begin
         return "[ X=" & To_String( State.X ) & ", " &
             "V=" & To_String( State.V ) & ", " &
             "M=" & Value_Type'Image( State.Mass ) & " ]" ;
      end To_String;

      -----------
      -- Force --
      -----------
      function Force( J : in Integer ) return Vector_Type is
         -- interaction between particle I anf J
         DX : constant Vector_Type := X1(J).X - X1(I).X;
         R  : constant Value_Type := Norm(DX);
         G0 : constant Value_Type := 6.67428E-11;
         F  : constant Value_Type := G0 * X1(I).Mass * X1(J).Mass/R**2;
      begin
         return (F - exp(-R**2/(2.0*RS**2)))*DX;
      exception
         when E : others =>
            Log.Error("Exception while calculating force *** " & Exception_Name( E ) & " " &
                         Exception_Message( E ) & " : " &
                         "X1=" & To_String( X1(I) ) & ", " &
                         "X2=" & To_String( X1(J) ) & ", " &
                         "DX=" & To_String( DX ) & ", " &
                         "RS=" & Value_Type'Image( RS ) &
                         Integer'Image(I) & "/" & Integer'Image(J));
            raise;
      end Force;

      DV : Vector_Type;
      DX : Vector_Type;
      K  : Vector_Type := Null_Vector;

   begin
      Pragma Debug( Start_Lap( T_Integration ) );

      for J in 1..N loop
         if I /= J then
            K := K + Force(J);
         end if;
      end loop;

      DV := ( DT / X1(I).Mass) * K;
      X2(I).V := X1(I).V + DV;

      DX := DT * X2(I).V + DT**2 / ( 2.0 * X1(I).Mass)* K;
      X2(I).X := X1(I).X + DX;
      X2(I).Mass := X1(I).Mass;

      Pragma Debug( Stop_Lap( T_Integration ) );

   exception
      when E : others =>
         Log.Error("Exception while processing vector *** " & Exception_Name( E ) & ":" &
                   Exception_Message( E ) & " for element" & Natural'Image(I) &
                   " " & To_String( X1(I) ) );
         raise;
   end Compute;

   -------------
   -- Combine --
   -------------
   procedure Combine( This : in out Object_Type ) is
      Tmp     : constant State_Vector_Access := This.X1;
   begin
      pragma Debug( Log.Comment("Combine") );

      This.X1 := This.X2;
      This.X2 := Tmp;

   end Combine;

end Computation;
