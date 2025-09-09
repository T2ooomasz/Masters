package Semafor_Uogolniony is
   
   -- Obiekt chroniony implementujący semafor liczący
   protected type Counting_Semaphore(Initial_Count : Natural) is
      -- P() - operacja "zajmij zasób" 
      -- Zmniejsza licznik o 1. Jeśli licznik = 0, blokuje zadanie
      entry P (Ile : Positive);
      
      -- V() - operacja "zwolnij zasób"
      -- Zwiększa licznik o 1, potencjalnie odblokowując oczekujące zadania
      procedure V (Ile : Positive);
      
      -- Funkcja do sprawdzenia aktualnej wartości (do debugowania)
      function Current_Value return Natural;
      
   private
      -- Licznik dostępnych zasobów
      -- Initial_Count to parametr dyskryminujący - ustala początkową wartość
      Count : Natural := Initial_Count;
   end Counting_Semaphore;
   
end Semafor_Uogolniony;
