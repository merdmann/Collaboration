-------------------------------------------------------------------------------
--                                                                           --
--  Filename        : $Id: vector_space.ads 14 2010-09-28 04:17:13Z  $
--  Description     :                                                        --
--  Author          : Michael Erdmann                                        --
--  Created         : 26.4.2003                                              --
--  Last Modified By: $Author: merdmann $                                    --
--  Last Modified On: $Date: 2007-07-15 11:27:23 +0200 (So, 15 Jul 2007) $                           --
--  Status          : $State: Exp $                                          --
--                                                                           --
--  Copyright (C) 2010 Michael Erdmann                                       --
--                                                                           --
--  HD is free software;  you can redistribute it  and/or modify it under    --
--  terms of the  GNU General Public License as published  by the Free Soft- --
--  ware  Foundation;  either version 2,  or (at your option) any later ver- --
--  sion. HD is distributed in the hope that it will be useful, but WITH-    --
--  OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY --
--  or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License --
--  for  more details.  You should have  received  a copy of the GNU General --
--  Public License  distributed with HD;  see file COPYING.  If not, write --
--  to  the Free Software Foundation,  59 Temple Place - Suite 330,  Boston, --
--  MA 02111-1307, USA.                                                      --
--                                                                           --
--  As a special exception, if other files  instantiate  generics from this  --
--  unit, or you link  this unit with other files  to produce an executable, --
--  this  unit  does not  by itself cause  the resulting  executable  to  be --
--  covered  by the  GNU  General  Public  License.  This exception does not --
--  however invalidate  any other reasons why  the executable file  might be --
--  covered by the  GNU Public License.                                      --
--                                                                           --
--  Contact                                                                  --
--  =======                                                                  --
--  Michael Erdmann <michael.erdmann@snafu.de>                               --
--                                                                           --
-------------------------------------------------------------------------------
package Vector_Space is

   type Value_Type is digits 8  range -1.0E255 .. 1.0E+255;

   type Vector_Type is array(1..3) of Value_Type;

   function "+"(
      A : in Vector_Type;
      B : in Vector_Type ) return Vector_Type;

   function "-"(
      A : in Vector_Type;
      B : in Vector_Type ) return Vector_Type;

   function "*"(
      A : in Value_Type;
      B : in Vector_Type ) return Vector_Type;

   function Norm(
      A : in Vector_Type ) return Value_Type;

   function Scalar_Product(
      A,B : in Vector_Type ) return Value_Type;

   function Angle( X, Y : Vector_Type ) return Value_Type;

   function To_String( X : Vector_Type ) return String;


   Null_Vector : constant Vector_Type := ( others => 0.0 );
end Vector_Space;

