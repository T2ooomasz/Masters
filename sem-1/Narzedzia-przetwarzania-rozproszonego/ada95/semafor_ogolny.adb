-- =====================================
-- PLIK: semafor_ogolny.adb  
-- IMPLEMENTACJA SEMAFORA BINARNEGO
-- =====================================

with Ada.Text_IO;
package body Semafor_Ogolny is
   
   protected body Binary_Semaphore is
      
      -- Implementacja operacji P() (zajmij)
      -- "when Available" to warunek ochrony - entry może być wykonane tylko gdy Available = True
      entry P when Available is  
      begin
         -- Gdy zadanie wchodzi tutaj, automatycznie zajmuje semafor
         -- Ustawiamy Available na False, co blokuje inne zadania w P()
         Available := False;
         Ada.Text_IO.Put_Line("Semafor binarny zajęty");
      end P;
      
      -- Implementacja operacji V() (zwolnij) 
      procedure V is
      begin
         -- Zwalniamy semafor - inne zadania mogą teraz wejść do P()
         -- Ustawienie Available := True może odblokować zadania czekające w kolejce P()
         Available := True;
         Ada.Text_IO.Put_Line("Semafor binarny zwolniony");
      end V;
      
   end Binary_Semaphore;
   
end Semafor_Ogolny;