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
with Ada.Text_IO;          	use Ada.Text_IO;
with Ada.Exceptions;		use Ada.Exceptions;
with Unchecked_Deallocation;

with Collaboration;             use Collaboration;
with Log;			use Log;

with Timers;			use Timers;
with Time_Measurement;		use Time_Measurement;

with Scheduling;		use Scheduling;
with System.Task_Info;		use System.Task_Info;
--with System.Multiprocessors;	use System.Multiprocessors;

package body Partitioned_Workpackage is

   Nbr_Of_Threads : constant Positive := 2;

   task type Computing_Task(
      Data : Object_Data_Access );
      -- CPU_Range : id) with CPU => Id;

   type Task_Handle is access Computing_Task;
   type Computing_Task_Array is array( Positive range <> ) of Task_Handle;

   ----------------------
   -- Object_Data_Type --
   ----------------------
   type Object_Data_Type( Partitions : Positive ) is record
      Current : Partitioned_Data.Handle := null;
      Nbr_Iterations : Positive := 1;
      B   : Collaboration.Object_Type(Partitions);
      H   : Computing_Task_Array(1..Partitions) := (others =>null);
   end record;

   ----------
   -- Free --
   ----------
   procedure Free is new Unchecked_Deallocation(
      Object_Data_Type,
      Object_Data_Access);

   --------------------
   -- Computing_Task --
   --------------------
   task body Computing_Task is
      B       : Collaboration.Object_Type renames Data.B;
      Current : Partitioned_Data.Handle renames Data.Current;
      RC      : Integer;
   begin
      RC := Set_Scheduling( Sched_BATCH );

      loop
         Join(B);

         loop
            declare
               P : Natural := Fetch( Current.all) ;
            begin
               exit when P = Last_Partition;

               Start_Lap( T_Processing );
               Compute( Current.all, P );
               Stop_Lap( T_Processing );
            end;
         end loop;
      end loop;

   exception
      when Termination =>
         Trace(B, "Terminated");

       when E : others =>
         Trace(B, "Exception " & Exception_Name( E )
               & ":" &  Exception_Message( E )
                 -- & " in worker " & CPU_Range'Image(Id)
              );
   end;

   ----------------
   -- Initialize --
   ----------------
   procedure Initialize( This : in out Object_Type ) is
      Data : Object_Data_Access := null;
      M    : Positive := Number_Of_Processors;
   begin
      if This.Partitions > 0 and  This.Partitions < Number_Of_Processors then
         M := This.Partitions;
      end if;

      Data := new Object_Data_Type(M);
      for i in 1..M loop
         --Data.H(i) := new Computing_Task(Data, CPU_Range(i));
          Data.H(i) := new Computing_Task(Data);
      end loop;

      This.Data := Data;
   end Initialize;

   --------------
   -- Finalize --
   --------------
   procedure Finalize( This : in out Object_Type ) is
      Data : Object_Data_Access renames This.Data;
   begin
      Free(Data);
   end Finalize;

   -------------
   -- Compute --
   -------------
   procedure Compute(
      This : in out Object_Type;
      ctx  : in Partitioned_Data.Handle;
      Nbr_Of_Iterations : in Positive) is

      Data : Object_Data_Access renames This.Data;
   begin
      Data.Nbr_Iterations := Nbr_Of_Iterations;
      Data.Current := ctx;

      Partitioning(ctx.all, Nbr_Of_Threads);

      for i in 1..Nbr_of_Iterations loop
    	 Partitioning(ctx.all, Nbr_Of_Threads);

         Start_Lap( T_Iteration );
         Start_Lap( T_Wait_For_Workers );
         Fork(Data.B);
         Join(Data.B);
         Stop_Lap( T_Wait_For_Workers);

         Combine(ctx.all);

         Stop_Lap( T_Iteration );
      end loop;
   end Compute;

   ----------
   -- Done --
   ----------
   procedure Done(This : in out Object_Type ) is
      Data : Object_Data_Access renames This.Data;
   begin
      Done( Data.B );
   end Done;

end Partitioned_Workpackage;
