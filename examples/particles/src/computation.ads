with Partitioned_Data;        use Partitioned_Data;
with Vector_Space;            use Vector_Space;
with Types;                   use Types;

package Computation is

  type Object_Type is new Partitioned_Data.Object_Type with record
      PZ : Natural := 0;
      X1 : State_Vector_Access := null;
      X2 : State_Vector_Access := null;
   end record;

   type Handle is access all Object_Type;

   procedure Compute( This : in out Object_Type; Partition : in Natural );
   procedure Combine( This : in out Object_Type );

end Computation;
