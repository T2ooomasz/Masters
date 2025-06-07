-- ========================================
-- ZADANIE 2: SEMAFOR UOGÓLNIONY (COUNTING)
-- ========================================

-- Semafor uogólniony (liczący) to rozszerzenie semafora binarnego.
-- Może mieć wartość całkowitą >= 0, reprezentującą liczbę dostępnych zasobów.
-- Pozwala na jednoczesny dostęp N zadaniom do N identycznych zasobów.

package Semafor_Uogolniony is
   
   -- Obiekt chroniony implementujący semafor liczący
   protected type Counting_Semaphore(Initial_Count : Natural) is
      -- P() - operacja "zajmij zasób" 
      -- Zmniejsza licznik o 1. Jeśli licznik = 0, blokuje zadanie
      entry P;
      
      -- V() - operacja "zwolnij zasób"
      -- Zwiększa licznik o 1, potencjalnie odblokowując oczekujące zadania
      procedure V;
      
      -- Funkcja do sprawdzenia aktualnej wartości (do debugowania)
      function Current_Value return Natural;
      
   private
      -- Licznik dostępnych zasobów
      -- Initial_Count to parametr dyskryminujący - ustala początkową wartość
      Count : Natural := Initial_Count;
   end Counting_Semaphore;
   
end Semafor_Uogolniony;

package body Semafor_Uogolniony is
   
   protected body Counting_Semaphore is
      
      -- Implementacja operacji P() (zajmij zasób)
      entry P when Count > 0 is  -- Warunek: wykonuj tylko gdy są dostępne zasoby
      begin
         -- Zmniejszamy licznik - jeden zasób mniej dostępny
         Count := Count - 1;
         Ada.Text_IO.Put_Line("Zasób zajęty. Pozostało:" & Natural'Image(Count));
      end P;
      
      -- Implementacja operacji V() (zwolnij zasób)
      procedure V is
      begin
         -- Zwiększamy licznik - jeden zasób więcej dostępny
         Count := Count + 1;
         Ada.Text_IO.Put_Line("Zasób zwolniony. Dostępne:" & Natural'Image(Count));
      end V;
      
      -- Zwraca aktualną wartość licznika
      function Current_Value return Natural is
      begin
         return Count;
      end Current_Value;
      
   end Counting_Semaphore;
   
end Semafor_Uogolniony;

