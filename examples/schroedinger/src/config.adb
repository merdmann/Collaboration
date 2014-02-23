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
--  You Should Have Received A Copy of The GNU General Public License
--  Along with This Program.  If not, See <Http://Www.Gnu.Org/Licenses/>.
--

with Ada.Text_IO;                use Ada.Text_IO;
with Parameter_File;             use Parameter_File;

with Log;                        use Log;
with Ada.Numerics.Float_Random;  use  Ada.Numerics.Float_Random;

with Ada.Numerics.Generic_Elementary_Functions;
use  Ada.Numerics;

package body Config is

   package Numerics is new Generic_Elementary_Functions( Value_Type );
   use Numerics;

   function Parameter is new Range_Parameter( Parameter_Type => Integer );
   function Parameter is new Digits_Parameter( Parameter_Type => Float );
   function Parameter is new Digits_Parameter( Parameter_Type => Value_Type );


   -- In case of the init mode = Random the initial locations of the particles are
   -- calculated by means of the a random generator
   G               : Generator;        -- random generator

   Params          : Parameter_File.Object;

   CFG_Workers     : constant Natural := Register( Params, "Workers" );
   CFG_N	   : constant Natural := Register( Params, "N");
   CFG_DT          : constant Natural := Register( Params, "DT");

   CFG_Iterations  : constant Natural := Register( Params, "Iterations");
   CFG_Report      : constant Natural := Register( Params, "Report" );

   ----------------
   -- Initialize --
   ----------------
   procedure Initialize( Name : in String ) is
   begin
      Load( Params, Name );
      List_Parameters(Params);

      N     := Parameter( Params, Cfg_N, 20 );
      DT    := Parameter( Params, CFG_DT, 0.1);

      Max_Iterations := Parameter( Params, CFG_Iterations, 1000);
      Report_Interval := Parameter( Params, CFG_Report, 20 );


      -- if not in config file the platform will select
      Workers := Parameter( Params, Cfg_Workers, 0);

      Log.Comment( "Report_Interval=" & Natural'Image(Report_Interval) );
      Log.Comment( "Max_Iterations=" & Natural'Image(Max_Iterations) );
   end Initialize;


end ;
