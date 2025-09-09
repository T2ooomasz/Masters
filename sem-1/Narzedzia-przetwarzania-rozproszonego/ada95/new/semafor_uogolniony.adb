-- ========================================
-- ZADANIE 2: SEMAFOR UOGÓLNIONY (COUNTING)
-- ========================================

with Ada.Text_IO;

package body Semafor_Uogolniony is
   
   protected body Counting_Semaphore is
      
      entry P (Ile : Positive) when True is  -- Barrier always true for initial entry
      begin
         if Count >= Ile then
            Count := Count - Ile;
         else
            requeue Wait (Ile);  -- Requeue to Wait if not enough; preserves queue order
         end if;
      end P;
      
      entry Wait (Ile : Positive) when Count >= Ile is  -- Barrier now valid (uses queued Ile)
      begin
         Count := Count - Ile;
      end Wait;
      
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