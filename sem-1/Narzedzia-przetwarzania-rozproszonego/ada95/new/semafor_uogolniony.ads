package Semafor_Uogolniony is
   
   -- Obiekt chroniony implementujący semafor liczący
   protected type Counting_Semaphore(Initial_Count : Natural) is
      -- P() - operacja "zajmij zasób" (atomically acquire 'Ile' resources)
      entry P (Ile : Positive);
      
      -- V() - operacja "zwolnij zasób" (atomically release 'Ile' resources)
      procedure V (Ile : Positive);
      
      -- Funkcja do sprawdzenia aktualnej wartości (do debugowania)
      function Current_Value return Natural;
      
   private
      -- Private entry for requeuing when not enough resources
      entry Wait (Ile : Positive);
      
      -- Licznik dostępnych zasobów
      Count : Natural := Initial_Count;
   end Counting_Semaphore;
   
end Semafor_Uogolniony;