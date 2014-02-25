--  ************************************************************************ --
--  *****                       Collaboration                          ***** --
--  *****               A simple parallelism framework                 ***** --
--  ************************************************************************ --
--
--  Copyright (C) 2013,2014 Michael Erdmann
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

with Ada.Numerics.Generic_Elementary_Functions;
with Ada.Numerics.Generic_Complex_Types;
with Ada.Numerics.Generic_Complex_Elementary_Functions;

with Coordinates;			use Coordinates;

package body Computation is
   package Complex_Functions is new
     Ada.Numerics.Generic_Complex_Elementary_Functions (Types.Complex_Types);
   use Complex_Functions;

   package Numerics is new Ada.Numerics.Generic_Elementary_Functions( Value_Type );
   use Numerics;

   use Types.Complex_Types;

   -------------
   -- Compute --
   -------------
   procedure Compute( This : in out Object_Type; I : in Natural ) is
      X0,Y0,Z0 : Natural;
      DP : Complex;
   begin
      Pragma Debug( Put_Line( Integer'Image(I) ) );

      To_Coordinate(I, X0, Y0, Z0);


      Pragma Debug( Start_Lap( T_Integration ) );

      -- compute here

      Pragma Debug( Stop_Lap( T_Integration ) );

   exception
      when E : others =>
         Log.Error("Exception while processing vector *** " & Exception_Name( E ) & ":" &
                   Exception_Message( E ) & " for element" & Natural'Image(I));
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
