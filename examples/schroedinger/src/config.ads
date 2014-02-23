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
with Vector_Space;         use Vector_Space;
with Types;                use Types;

package Config is

   -- Number of workers to be used

   Workers 	   : Natural := 0;
   -- ++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   -- +++++ Parameters controlling the iteration process +++++
   -- ++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   -- number of iterations
   Max_Iterations  : Natural := 100;
   -- the number of iterations per reported result
   Report_Interval : Natural := 5;
   -- time steps per iteration
   DT              : Value_Type   := 0.0001;

   -- interval between cells
   DX 		   : Value_Type   := 0.01;

   -- ++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   -- +++++          Simulation Parameters               +++++
   -- ++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   -- number of particles
   N               : Natural := 0;

   -- ++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   -- +++++         Process Related Parameters           +++   ++
   -- ++++++++++++++++++++++++++++++++++++++++++++++++++++++++

   -- current time
   T               : Value_Type := 0.0;

   procedure Initialize( Name : in String );

end Config;
