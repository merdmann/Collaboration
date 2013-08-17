BEGIN {
  itertion = "";
  vector = "";
  wait = "";
  total = "";
}

/T_Iteration/ { iteration=$10 }
/T_Vector/ { vector=$10 }
/T_Wait_For_Workers/ { wait=$10 }
/T_Total/ { total=$10 }

END {
   print N,";",vector,";",iteration,";",wait, ";", total;
}
