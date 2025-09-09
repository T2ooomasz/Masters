-- ========================================
-- ZADANIE 2: SEMAFOR UOGÓLNIONY (COUNTING)
-- ========================================

package body Semafor_Uogolniony is
   
   protected body Counting_Semaphore is
      
      entry P when Count > 0 is
      begin
         Count := Count - 1;
      end P;
      
      procedure V is
      begin
         Count := Count + 1;
      end V;
      
      function Current_Value return Natural is
      begin
         return Count;
      end Current_Value;
      
   end Counting_Semaphore;
   
end Semafor_Uogolniony;