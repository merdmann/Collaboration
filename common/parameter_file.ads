--  ************************************************************************ --
--  *****           P A R T I C L E  S I M U L A T O R  (PSim)         ***** --
--  *****               FOR  CLASSICAL PARTICLES                       ***** --
--  ************************************************************************ --
--  $Id: parameter_file.ads 10 2010-09-11 16:01:04Z  $
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
with Ada.Finalization;                  use Ada.Finalization;

package Parameter_File is
   type Object is new Controlled with private;
   type Handle is access all Object'Class;

   function Register(This : in Object; Name : in String ) return Natural;
   procedure Load( This : in out Object; Name : in String );
   function Value( This : in Object; Id : in Natural ) return String;

   function Parameter(This : in Object; Id: in Natural; Default : in String := "" ) return String;
   procedure List_Parameters( This : in Object );
   function Is_Parameter_Set(This : in Object; Id: in Natural ) return Boolean;
   function Register_Array(This : in Object; Name : in String; Length : in Natural ) return Natural;

   generic
      type Parameter_Type is (<>);
   function Range_Parameter(
      This : in Object;
      Id   : in Natural; Default : in Parameter_Type ) return Parameter_Type ;

   generic
      type Parameter_Type is digits <>;
   function Digits_Parameter(
      This : in Object;
      Id   : in Natural; Default : in Parameter_Type ) return Parameter_Type ;

   generic
      type Parameter_Type is (<>);
   function Enumeration_Parameter(
      This : in Object;
      Id   : in Natural; Default : in Parameter_Type ) return Parameter_Type ;

private
   type Object_Data_Type;
   type Object_Data_Access is access all Object_Data_Type;

   procedure Initialize( This : in out Object );
   procedure Finalize( This : in out Object );

   type Object is new Controlled with record
      Data : Object_Data_Access := null;
   end record;

end;
