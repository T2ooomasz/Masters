-- =====================================
-- PLIK: semafor_uogolniony.adb
-- IMPLEMENTACJA SEMAFORA LICZĄCEGO
-- =====================================

with Ada.Text_IO;
package body Semafor_Uogolniony is
   
   protected body Counting_Semaphore is
      
      -- Implementacja operacji P() (zajmij zasób)
      -- "when Count > 0" oznacza: wykonuj tylko gdy są dostępne zasoby
      entry P when Count > 0 is  
      begin
         -- Zmniejszamy licznik - jeden zasób mniej dostępny
         Count := Count - 1;
         Ada.Text_IO.Put_Line("Zasób zajęty. Pozostało:" & Natural'Image(Count));
      end P;
      
      -- Implementacja operacji V() (zwolnij zasób)
      procedure V is
      begin
         -- Zwiększamy licznik - jeden zasób więcej dostępny
         -- To może odblokować zadania czekające w kolejce P()
         Count := Count + 1;
         Ada.Text_IO.Put_Line("Zasób zwolniony. Dostępne:" & Natural'Image(Count));
      end V;
      
      -- Zwraca aktualną wartość licznika (tylko do odczytu)
      function Current_Value return Natural is
      begin
         return Count;
      end Current_Value;
      
   end Counting_Semaphore;
   
end Semafor_Uogolniony;
