-------------------------------------------------------------------------------
--                                                                           --
--  Filename        : $Source: /public/cvs/CellSim/util/util-Vector_Typespace.adb,v $
--  Description     :                                                        --
--  Author          : Michael Erdmann                                        --
--  Created         : 26.4.2003                                              --
--  Last Modified By: $Author: merdmann $                                    --
--  Last Modified On: $Date: 2007-07-15 11:27:23 +0200 (So, 15 Jul 2007) $                           --
--  Status          : $State: Exp $                                          --
--                                                                           --
--  Copyright (C) 2003 Michael Erdmann                                       --
--                                                                           --
--  CELLS is free software;  you can redistribute it  and/or modify it under --
--  terms of the  GNU General Public License as published  by the Free Soft- --
--  ware  Foundation;  either version 2,  or (at your option) any later ver- --
--  sion.  GNAT is distributed in the hope that it will be useful, but WITH- --
--  OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY --
--  or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License --
--  for  more details.  You should have  received  a copy of the GNU General --
--  Public License  distributed with GNAT;  see file COPYING.  If not, write --
--  to  the Free Software Foundation,  59 Temple Place - Suite 330,  Boston, --
--  MA 02111-1307, USA.                                                      --
--                                                                           --
--  As a special exception,  if other files  instantiate  generics from this --
--  unit, or you link  this unit with other files  to produce an executable, --
--  this  unit  does not  by itself cause  the resulting  executable  to  be --
--  covered  by the  GNU  General  Public  License.  This exception does not --
--  however invalidate  any other reasons why  the executable file  might be --
--  covered by the  GNU Public License.                                      --
--                                                                           --
--                                                                           --
--  Functional Description                                                   --
--  ======================                                                   --
--                                                                           --
--                                                                           --
--  Restrictions                                                             --
--  ============                                                             --
--  None                                                                     --
--                                                                           --
--  References                                                               --
--  ==========                                                               --
--  None                                                                     --
--                                                                           --
--  Contact                                                                  --
--  =======                                                                  --
--  Michael Erdmann <michael.erdmann@snafu.de>                               --
--                                                                           --
-------------------------------------------------------------------------------
with Ada.Numerics.Generic_Elementary_Functions;
use  Ada.Numerics;

package body Vector_Space is

   package Numerics is new Generic_Elementary_Functions( Value_Type );
   use Numerics;

   ---------
   -- Add --
   ---------
   function "+"(
      A : in Vector_Type;
      B : in Vector_Type ) return Vector_Type is
      -- add two Vector_Types
      R : Vector_Type;
   begin
      for i in R'Range loop
         R(i) := A(i) + B(i);
      end loop;

      return R;
   end "+";

   ---------
   -- Add --
   ---------
   function "-"(
      A : in Vector_Type;
      B : in Vector_Type ) return Vector_Type is
      -- subtract two Vector_Types
      R : Vector_Type;
   begin
      for i in R'Range loop
         R(i) := A(i) - B(i);
      end loop;

      return R;
   end "-";

   --------------
   -- Multiply --
   --------------
   function "*"(
      A : in Value_Type;
      B : in Vector_Type ) return Vector_Type is
      -- multiply by a factor
      R : Vector_Type;
   begin
      for i in R'Range loop
         R(i) := A * B(i);
      end loop;

      return R;
   end "*";

   --------------------
   -- Scalar_Product --
   --------------------
   function Scalar_Product(
      A,B : in Vector_Type ) return Value_Type is
      R   : Value_Type := 0.0;
   begin
      for i in A'Range loop
         R := R + A(i)*B(i);
      end loop;

      return R;
   end Scalar_Product;

   ----------
   -- Norm --
   ----------
   function Norm(
      A : in Vector_Type ) return Value_Type is
   begin
      return Sqrt( Scalar_Product( A,A ) );
   end Norm;

   -----------
   -- Angle --
   -----------
   function Angle( X, Y : Vector_Type ) return Value_Type is
   begin
      return Arccos( Scalar_Product( X,Y ) / ( Norm(X) * Norm(Y) ) );
   end Angle;

   function To_String( X : in Vector_Type ) return String is
   begin
      return "<" & Value_Type'Image(X(1)) & " " & Value_Type'Image(X(1)) & " " & Value_Type'Image(X(1)) & ">";
   end To_String;

end Vector_Space;
