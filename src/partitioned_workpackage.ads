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

with Ada.Finalization;        use Ada.Finalization;
with Partitioned_Data;        use Partitioned_Data;

package Partitioned_Workpackage is

   Auto_Partitioning : constant Natural := 0;

   type Object_Type( Partitions : Natural ) is new Controlled with private;

   -- Iterate Nbr_Of_Iterations on the Partintioned_Data set.
   procedure Compute(
      This: in out Object_Type;
      Ctx : in Partitioned_Data.Handle;
      Nbr_Of_Iterations : in Positive);

   -- this stops the commputing
   procedure Done(This : in out Object_Type );

private
   type Object_Data_Type;
   type Object_Data_Access is access all Object_Data_Type;

   type Object_Type( Partitions : Natural ) is new Controlled with record
      Data : Object_Data_Access := null;
   end record;

   procedure Initialize( This : in out Object_Type );
   procedure Finalize( This : in out Object_Type );

end Partitioned_Workpackage;
