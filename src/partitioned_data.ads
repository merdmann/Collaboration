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
with Unchecked_Deallocation;

package Partitioned_Data is

   -- From this type the context for all partitioned computations
   -- is to be derived. N SPecifies the max number of partitions to
   -- be procesed.
   type Object_Type(N : Positive ) is abstract new Controlled with private;
   type Handle is access all Object_Type'Class;

   -- Split the working data set into Nbr partitions. The default implementation
   -- simply set the partition counter to 1.
   procedure Partitioning( This : in out Object_Type; Nbr : in Natural );

   -- compute the result for a given partition
   procedure Compute( This : in out Object_Type; Partition : in Natural ) is abstract;

   -- fetch the work package (partition) return a partition number. If not
   -- provided by the application this returns the next partition.
   function Fetch( This : in Object_Type) return Natural;

   -- If fetch exceeds the range of avaiable partitions this value will be
   -- returned. All sucessive calls to Fetch returning the same value.
   Last_Partition : constant Natural := 0;

   -- combine the computing results of all partitions
   procedure Combine( This : in out Object_Type ) is abstract;

private

   type Object_Data_Type;
   type Object_Data_Access is access all Object_Data_Type;

   type Object_Type(N : Positive) is abstract new Controlled with record
      Data : Object_Data_Access := null;
   end record;

   procedure Initialize( This : in out Object_Type );
   procedure FInalize( This : in out Object_Type );


end Partitioned_Data;
