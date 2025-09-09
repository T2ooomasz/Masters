-- ========================================
-- ZADANIE 2: SEMAFOR UOGÓLNIONY (COUNTING)
-- ========================================

with Ada.Text_IO;

package body Semafor_Uogolniony is
   
   protected body Counting_Semaphore is
      
      entry P (Ile : Positive) when Count >= 0 is
      begin
         if Count >= Ile then
            Count := Count - Ile;
            Ada.Text_IO.Put_Line("Zasób zajęty. Pozostało:" & Natural'Image(Count));
         else
         requeue P;
         end if;
      end P;
      
      procedure V (Ile : Positive) is
      begin
         Count := Count + Ile;
         Ada.Text_IO.Put_Line("Zasób zwolniony. Dostępne:" & Natural'Image(Count));
      end V;
      
      function Current_Value return Natural is
      begin
         return Count;
      end Current_Value;
      
   end Counting_Semaphore;
   
end Semafor_Uogolniony;
