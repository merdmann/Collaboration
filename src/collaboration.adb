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
with Ada.Text_IO;                   use Ada.Text_IO;
with Unchecked_Deallocation;
with Ada.Task_Identification;	use Ada.Task_Identification;
with Log;			use Log;

package body Collaboration is

   type State_Type is ( S_Close, S_Open, S_Shutdown );
   ---------------
   -- Gate_Type --
   ---------------
   protected type Gate_Type( Size : Positive ) is
      -- worker entries
      entry Join_Unit;

      -- main task entries
      entry Wait;
      procedure Start;
      procedure Shutdown;

      -- logging
      procedure Logger( S : in String );
   private
      State  : State_Type  := S_Close; -- indicate that computing should start
   end Gate_Type;

   protected body Gate_Type is
      -- Worker API
      entry Join_Unit when ( State = S_Open or State = S_Shutdown ) is
      begin
         if State = S_Shutdown then
            raise Termination;
         end if;

         if Join_Unit'Count = 0 then
            State := S_Close;
         end if;
      end Join_Unit;

      -- Main thread API
      entry Wait when State = S_Close and Join_Unit'Count = Size is
      begin
         null;
      end Wait;

      procedure Start is
      begin
         State := S_Open;
      end Start;

      procedure Shutdown is
      begin
         State := S_Shutdown;
         Logger("Shutting down");
      end Shutdown;

      procedure Logger( S : in String ) is
      begin
         pragma Debug( Log.Comment(  State_Type'Image(State) & " "
                     & Natural'Image(Join_Unit'Count) & " "
                      & S ) );
         null;
      end Logger;

   end Gate_Type;

   ----------------------
   -- Object_Data_Type --
   ----------------------
   type Object_Data_Type( N : Positive ) is record
      Main_Task : Task_ID;
      Unit      : Gate_Type(N);
   end record;

   ----------
   -- Free --
   ----------
   procedure Free is new Unchecked_Deallocation(Object_Data_Type,
                                                Object_Data_Access);

   -----------
   -- Trace --
   -----------
   procedure Trace( This : in out Object_Type'Class; S : in String ) is
      U : Gate_Type renames This.Data.Unit;
   begin
      U.Logger(S);
   end Trace;

   ----------------
   -- Initialize --
   ----------------
   procedure Initialize( This : in out Object_Type ) is
      Data : constant Object_Data_Access := new Object_Data_Type(This.Size);
   begin
      This.Data := Data;

      Data.Main_Task := Current_Task;
   end Initialize;

   --------------
   -- Finalize --
   --------------
   procedure Finalize( This : in out Object_Type ) is
   begin
      Free( This.Data );
   end Finalize;

   ----------
   -- Join --
   ----------
   procedure Join(This : in out Object_Type'Class ) is
      U : Gate_Type renames This.Data.Unit;
   begin
      pragma Debug( Trace(This,"Join --->"));

      if This.Data.Main_Task = Current_Task then
 	 U.Wait;
      else
 	 U.Join_Unit;
      end if;

      pragma Debug( Trace(This, "Join <---") );
   end Join;

   ----------
   -- Fork --
   ----------
   procedure Fork( This : in out Object_Type'Class ) is
      U : Gate_Type renames This.Data.Unit;
   begin
      pragma Debug( Trace(This,"Fork --->") );
      U.Wait;
      U.Start;
      pragma Debug( Trace(This,"Fork <---"));
   end Fork;

   ----------
   -- Done --
   ----------
   procedure Done( This : in out Object_Type'Class ) is
      U : Gate_Type renames This.Data.Unit;
   begin
      U.Shutdown;
   end Done;

end Collaboration;
