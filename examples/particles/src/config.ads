--  ************************************************************************ --
--  *****           P A R T I C L E  S I M U L A T O R  (PSim)         ***** --
--  *****               FOR  CLASSICAL PARTICLES                       ***** --
--  ************************************************************************ --
--  $Id: config.ads 24 2010-10-14 16:24:58Z  $
--
--  This File is Part of PSim.THis file provides the configuration data
--  for a given calculation run.
--
--  Copyright (C) 2010 Michael Erdmann
--
--  PSim is Free Software: You Can Redistribute It and/or Modify
--  It Under The Terms of The GNU General Public License As Published By
--  The Free Software Foundation, Either Version 3 of The License, or
--  (at Your Option) Any Later Version.
--
--  This Program is Distributed in The Hope That It Will Be Useful,
--  But WITHOUT ANY WARRANTY; Without Even The Implied Warranty of
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See The
--  GNU General Public License for More Details.
--
--  You Should Have Received A Copy of The GNU General Public License
--  Along with This Program.  If not, See <Http://Www.Gnu.Org/Licenses/>.
--
--  As a special exception,  if other files  instantiate  generics from this
--  unit, or you link  this unit with other files  to produce an executable,
--  this  unit  does not  by itself cause  the resulting  executable  to  be
--  covered  by the  GNU  General  Public  License.  This exception does not
--  however invalidate  any other reasons why  the executable file  might be
--  covered by the  GNU Public License.
--
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

   -- ++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   -- +++++          Simulation Parameters               +++++
   -- ++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   -- number of particles
   N               : Natural := 0;
   -- screening
   RS              : Value_Type := 0.0;
   -- size of the box where particles placed by random
   Unit            : Float   := 1.00;
   -- Max. mass of the particles
   M0              : Float   := 1.00;
   -- number of steps
   MMAX            : Integer := 3;

   -- ++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   -- +++++         Process Related Parameters           +++   ++
   -- ++++++++++++++++++++++++++++++++++++++++++++++++++++++++

   -- current time
   T               : Value_Type := 0.0;

   procedure Initialize( Name : in String );

   procedure Get_Particle(
      Id : in Integer;
      T  : out Value_Type;
      S  : out Particle_Type );

   procedure Setup_Initial_Values(
       State_File    : in String;
       Initial_State : in State_Vector_Access ) with
                  Pre => Initial_State /= null;

end Config;
