-- ========================================
-- ZADANIE 2: SEMAFOR UOGÓLNIONY (COUNTING)
-- ========================================

package body Semafor_Uogolniony is
   
   protected body Counting_Semaphore is
      
      entry P (Ile : Positive) when Count >= Ile is
      begin
         Count := Count - Ile;
      end P;
      
      procedure V (Ile : Positive) is
      begin
         Count := Count + Ile;
      end V;
      
      function Current_Value return Natural is
      begin
         return Count;
      end Current_Value;
      
   end Counting_Semaphore;
   
end Semafor_Uogolniony;
