-- ========================================
-- ZADANIE 2: SEMAFOR UOGÓLNIONY (COUNTING)
-- ========================================

-- Semafor uogólniony (liczący) to rozszerzenie semafora binarnego.
-- Może mieć wartość całkowitą >= 0, reprezentującą liczbę dostępnych zasobów.
-- Pozwala na jednoczesny dostęp N zadaniom do N identycznych zasobów.

with Ada.Text_IO;

package body Semafor_Uogolniony is
   
   protected body Counting_Semaphore is
      
      -- Implementacja operacji P() (zajmij zasób)
      entry P (Ile : Positive) when Count >= Ile is  -- Warunek: wykonuj tylko gdy są dostępne zasoby
      begin
         -- Zmniejszamy licznik - jeden zasób mniej dostępny
         Count := Count - Ile;
         Ada.Text_IO.Put_Line("Zasób zajęty. Pozostało:" & Natural'Image(Count));
      end P;
      
      -- Implementacja operacji V() (zwolnij zasób)
      procedure V (Ile : Positive) is
      begin
         -- Zwiększamy licznik - jeden zasób więcej dostępny
         Count := Count + Ile;
         Ada.Text_IO.Put_Line("Zasób zwolniony. Dostępne:" & Natural'Image(Count));
      end V;
      
      -- Zwraca aktualną wartość licznika
      function Current_Value return Natural is
      begin
         return Count;
      end Current_Value;
      
   end Counting_Semaphore;
   
end Semafor_Uogolniony;

