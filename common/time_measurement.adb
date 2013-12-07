--  ************************************************************************ --
--  *****           P A R T I C L E  S I M U L A T O R  (PSim)         ***** --
--  *****               FOR  CLASSICAL PARTICLES                       ***** --
--  ************************************************************************ --
--  $Id: particle.adb 24 2010-10-14 16:24:58Z  $
--
--  Copyright (C) 2012 Michael Erdmann
--
--  This is free software;  you can redistribute it  and/or modify it under
--  terms of the  GNU General Public License as published  by the Free Soft-
--  ware  Foundation;  either version 2,  or (at your option) any later
--  version.  Psim is distributed in the hope that it will be useful, but
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
with Ada.Real_Time;                 use Ada.Real_Time;
with Ada.Calendar.Formatting;
with Ada.Task_Attributes;
with Ada.Task_Termination;          use Ada.Task_Termination;
with Ada.Task_Identification;       use Ada.Task_Identification;
with Ada.Exceptions;                use Ada.Exceptions;
with Ada.Strings.Unbounded;         use Ada.Strings.Unbounded;

use Ada;

with Log;                           use Log;

package body Time_Measurement is

   -- This information is stored in each task. The information is merged into the
   -- global summary after the task has terminated.

   -----------------------
   -- Timer_Record_Type --
   -----------------------
   type Timer_Record_Type is record
      Samples     : Natural := 0;
      Start_Time  : Time;
      Elapsed     : Duration := 0.0;
   end record;

   Null_Timer : constant Timer_Record_Type := ( Samples => 0, Start_Time => Time_First, Elapsed => 0.0);

   type Clock_Array_Type  is array(Clock_Id_Type) of Timer_Record_Type;
   type Clock_Array_Access is access all Clock_Array_Type;

   package Per_Task is new Ada.Task_Attributes( Attribute => Clock_Array_Access,
                                        Initial_value => null);
   use Per_Task;


   -- This information is common to all tasks. The information will be updated
   -- by each task upon termination.

   -----------------------
   -- Summary_Item_Type --
   -----------------------
   type Summary_Item_Type is record
      Name     : Unbounded_String := Null_Unbounded_String;
      Samples  : Natural := 0;
      Elapsed  : Duration := 0.0;
   end record;

   type Summary_Array_Type is array( Clock_Id_Type ) of Summary_Item_Type;

   ------------------
   -- Summary_Type --
   ------------------
   protected type Summary_Type is
      function To_String( Id : in Clock_Id_Type ) return String;
      procedure Finish(C: Cause_Of_Termination; T: Task_Id; X: Exception_Occurrence);
      procedure Update( Id : in Clock_Id_Type; Samples : in Natural; Elapsed : in Duration );
      procedure Register( Name : in String; Result : out Clock_Id_Type);

   private
      Clock : Summary_Array_Type ;
   end Summary_Type;

   protected body Summary_Type is

      procedure Update( Id : in Clock_Id_Type; Samples : in Natural; Elapsed : in Duration ) is
      begin
         Clock(id).Samples := Clock(Id).Samples + Samples;
         Clock(Id).Elapsed := Clock(Id).Elapsed + Elapsed;
      end Update;

      procedure Register( Name : in String; Result : out Clock_Id_Type) is
      begin
         for Id in Clock_Id_Type loop
            if Id /= Clock_Id_Null and Clock(id).Name = Null_Unbounded_String then
               Clock(Id).Name := To_Unbounded_String(Name);
               Result := Id;
               exit;
            end if;
         end loop;
      end Register;

      function To_String( Id : in Clock_Id_Type ) return String is

         function Format( S : in String ) return String is
            Result : String(1..20) := ( others => ' ');
            J      : Natural := S'First;
         begin
            for i in Result'Range loop
               if J in S'Range then
                  Result(I) := S(J);
                  J := J + 1;
               else
                  exit;
               end if;
            end loop;

            return Result;
         end ;

      begin
         if Clock(id).Samples > 0 then
            return
              Format(To_String( Clock(Id).Name ) ) & " [" & Clock_Id_Type'Image(Id) & " ]  : " &
              "Total: " & Calendar.Formatting.Image
                  (Elapsed_Time => Clock(Id).Elapsed, Include_Time_Fraction => True) & ", " &
              "Lap: "  & Duration'Image( Clock(Id).Elapsed/Duration(Clock(Id).Samples) ) & " " &
               ", Laps: " & Natural'Image(Clock(Id).Samples);
         else
            return  Format(To_String( Clock(Id).Name )) & " [" & Clock_Id_Type'Image(Id) & " ] ** undef ** ";
         end if;
      end To_String;

      ------------
      -- Finish --
      ------------
      procedure Finish(C: Cause_Of_Termination; T: Task_Id; X: Exception_Occurrence) is
         My_Clock : constant Clock_Array_Access := Per_Task.Value(T);
      begin
         for Id in Clock_Id_Type loop
            if My_Clock(Id).Samples > 0 then
               Clock(id).Samples := Clock(Id).Samples + My_Clock(Id).Samples;
               Clock(Id).Elapsed := Clock(Id).Elapsed + My_Clock(Id).Elapsed;
            end if;
         end loop;
      end Finish;

   end Summary_Type;

   Summary : Summary_Type ;


   -------------------
   -- Per_Task_Data --
   -------------------
   function Per_Task_Data return Clock_Array_Access is
      Clock : Clock_Array_Access := Per_Task.Value;
   begin
      if Clock = null then
         Clock := new Clock_Array_Type;
         Set_Value( Clock );
         Set_Specific_Handler( Current_Task, Summary.Finish'Access );
      end if;
      return Clock ;
   end Per_Task_Data;

   ---------------
   -- To_String --
   ---------------
   function To_String( Id : in Clock_Id_Type ) return String is
   begin
      return Summary.To_String(Id);
   end To_String;

   --------------
   -- Register --
   --------------
   function Register( Name : in String ) return Clock_Id_Type is
      Result : Clock_Id_Type ;
   begin
      Summary.Register(Name, Result);
      return Result;
   end Register;

   ----------------
   -- Start_lap --
   ----------------
   procedure Start_Lap( Id : in Clock_Id_Type ) is
      Clock : constant Clock_Array_Access := Per_Task_Data;
   begin
      pragma Debug( Log.Comment("----- Start Lap: " & To_String(id)) );
      Clock(Id).Start_Time := Real_Time.Clock;
   end Start_Lap;

   --------------
   -- Stop_lap --
   --------------
   procedure Stop_Lap( Id : in Clock_Id_Type ) is
      Clock   : constant Clock_Array_Access := Per_Task_Data;
      Current : constant Real_Time.Time := Real_Time.Clock;
   begin
      Clock(Id).Elapsed := Clock(id).Elapsed +
        Real_Time.To_Duration(Real_Time."-" (Left => Current, Right => Clock(Id).Start_Time));

      Clock(Id).Start_Time := Current;
                   Clock(id).Samples := Clock(Id).Samples + 1;

      pragma Debug( Log.Comment("----- Stop Lap: " & To_String(Id)) );
   end Stop_Lap;

   ----------------
   -- Cancel_Lap --
   ----------------
   procedure Cancel_Lap(Id : in Clock_Id_Type ) is
      Clock   : constant Clock_Array_Access := Per_Task_Data;
      Current : constant Real_Time.Time := Real_Time.Clock;
   begin
      null;
   end Cancel_Lap;


   --------------------
   -- Update_Summary --
   --------------------
   procedure Update_Summary is
      My_Clock : constant Clock_Array_Access := Per_Task_Data;
   begin
      for Id in Clock_Id_Type loop
         if My_Clock(Id).Samples > 0 then
            Summary.Update( Id, My_Clock(id).Samples, My_Clock(Id).Elapsed);
            My_Clock(Id) := Null_Timer;
         end if;
      end loop;
   end Update_Summary;

end Time_Measurement;
