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
with System.Task_Info;			use System.Task_Info;

package body Partitioned_Data is

   ---------------------------
   -- Parition_Counter_Type --
   ---------------------------
   protected type Partition_Counter_Type( Max : Positive ) is
      procedure Next( Value : out Natural ) ;
      procedure Reset;
   private
      Counter : Natural := 0;
   end Partition_Counter_Type;

   protected body Partition_Counter_Type is

      procedure Next( Value : out Natural ) is
      begin
         Counter := Counter + 1;
         if Counter > Max then
            Value := Last_Partition;
         else
            Value := Counter;
         end if;
      end Next;

      procedure Reset is
      begin
         Counter := 0;
      end Reset;
   end Partition_Counter_Type;

   ----------------------
   -- Object_Data_Type --
   ----------------------
   type Object_Data_Type( N : Positive ) is record
      Partition : Partition_Counter_Type(N);
   end record;

   ----------
   -- Free --
   ----------
   procedure Free is new Unchecked_Deallocation(Object_Data_Type,
                                                Object_Data_Access);

   ----------------
   -- Initialize --
   ----------------
   procedure Initialize( This : in out Object_Type ) is
   begin
      This.Data := new Object_Data_Type(This.N);
   end Initialize;

   --------------
   -- Finalize --
   --------------
   procedure Finalize( This : in out Object_Type ) is
   begin
      Free( This.Data );
   end Finalize;

   ----------------
   -- Partitioning --
   ----------------
   procedure Partitioning( This : in out Object_Type; Nbr : in Natural ) is
   begin
      This.Data.Partition.Reset;
   end Partitioning;

   -----------
   -- Fetch --
   -----------
   function Fetch( This : in Object_Type) return Natural is
      Data : Object_Data_Access renames This.Data;
      Result : Natural := 1;
   begin
      Data.Partition.Next(Result);
      return Result;
   end Fetch;

end Partitioned_Data;
