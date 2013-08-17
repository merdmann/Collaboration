with Time_Measurement;                   use Time_Measurement;

package Timers is
   T_Total            : constant Clock_Id_Type := Register("T_Total");
   T_Wait_For_Ready   : constant Clock_Id_Type := Register("T_Wait_For_Ready");
   T_Wait_For_Workers : constant Clock_Id_Type := Register("T_Wait_For_Workers");
   T_Iteration        : constant Clock_Id_Type := Register("T_Iteration");
   T_Processing       : constant Clock_Id_Type := Register("T_Processing");
   T_Output           : constant Clock_Id_Type := Register("T_Output");
   T_Vector           : constant Clock_Id_Type := Register("T_Vector");
   T_Integration      : constant Clock_Id_Type := Register("T_Integration");
   T_Sleep            : constant Clock_Id_Type := Register("T_Sleep");
   T_Forces           : constant Clock_Id_Type := Register("T_Forces");
end Timers;
