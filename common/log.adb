--  ************************************************************************ --
--  *****           P A R T I C L E  S I M U L A T O R  (PSim)         ***** --
--  *****               FOR  CLASSICAL PARTICLES                       ***** --
--  ************************************************************************ --
--  $Id: log.adb 19 2010-10-05 19:43:13Z  $
--
--  Copyright (C) 2010 Michael Erdmann
--
--  PSim is free software;  you can redistribute it  and/or modify it under
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
with Ada.Text_IO;             use Ada.Text_IO;
with Ada.Calendar;            use Ada.Calendar;
with Ada.Task_Identification; use Ada.Task_Identification;


package body Log is

   Report_File   : File_Type ;
   Last_Progress : Time := Clock;

   function Format( S : in String ) return String is
   begin
      return Image( Current_Task ) & " " & S ;
   end Format;

   -------------
   -- Comment --
   -------------
   procedure Comment( S : in String ) is
      T : constant String := Format(S);
   begin
      Put_Line(T);

      if Is_Open( Report_File ) then
         Put_Line(Report_File, T);
      end if;
   end;

   ------------
   -- Result --
   ------------
   Procedure Result( S: in String ) is
   begin
      Put_Line(S);
   end ;

   -----------
   -- Error --
   -----------
   procedure Error( S : in String ) is
      T : constant String := Format(S);
   begin
      Put_Line(T);
      if Is_Open( Report_File ) then
         Put_Line(Report_File, T);
      end if;
   end ;

   ---------------------
   -- Report_Progress --
   ---------------------
   procedure Report_Progress( Progress : in Float ) is
      Used : constant Duration := Clock - Last_Progress;
   begin
      if Is_Open( Report_File ) then
         Put_Line( Report_File, Float'Image(100.0*Progress) & ";" & Duration'Image(Used));
         Flush( Report_File );
      end if;
      Last_Progress := Last_Progress + Used;
   end Report_Progress;

   ----------
   -- Open --
   ----------
   procedure Open( Name : in String ) is
   begin
      Create( File => Report_File, Name => Name, Mode => Out_File );
   end Open;

   -----------
   -- Close --
   -----------
   procedure Close is
   begin
      Close(Report_File);
   end Close;
end ;
