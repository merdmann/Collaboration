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
--  For more details please consult the README file
--
with Partitioned_Data;        use Partitioned_Data;
with Vector_Space;            use Vector_Space;
with Types;                   use Types;

package Computation is
   -----------------
   -- Object_Type --
   -----------------
   type Object_Type is new Partitioned_Data.Object_Type with record
      PZ : Natural := 0;
      X1 : State_Vector_Access := null;
      X2 : State_Vector_Access := null;
   end record;

   type Handle is access all Object_Type;

   procedure Compute( This : in out Object_Type; I : in Natural );
   procedure Combine( This : in out Object_Type );

end Computation;
