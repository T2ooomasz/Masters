-- =====================================
-- PLIK: semafor_uogolniony.ads
-- SPECYFIKACJA SEMAFORA LICZĄCEGO  
-- =====================================

package Semafor_Uogolniony is
   
   -- Obiekt chroniony implementujący semafor liczący
   -- Semafor liczący może mieć wartość całkowitą >= 0
   -- Reprezentuje liczbę dostępnych identycznych zasobów
   -- Pozwala na jednoczesny dostęp N zadaniom do N zasobów
   protected type Counting_Semaphore(Initial_Count : Natural) is
      -- P() - operacja "zajmij zasób" 
      -- Zmniejsza licznik o 1. Jeśli licznik = 0, blokuje zadanie
      entry P;
      
      -- V() - operacja "zwolnij zasób"
      -- Zwiększa licznik o 1, potencjalnie odblokowując oczekujące zadania
      procedure V;
      
      -- Funkcja do sprawdzenia aktualnej wartości (przydatna do debugowania)
      function Current_Value return Natural;
      
   private
      -- Licznik dostępnych zasobów
      -- Initial_Count to parametr dyskryminujący - ustala początkową wartość przy tworzeniu
      Count : Natural := Initial_Count;
   end Counting_Semaphore;
   
end Semafor_Uogolniony;