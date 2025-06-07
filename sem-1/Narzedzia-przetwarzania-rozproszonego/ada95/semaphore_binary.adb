-- =====================================
-- ZADANIE 1: SEMAFOR OGÓLNY (BINARY)
-- =====================================

-- Semafor ogólny (binarny) to mechanizm synchronizacji, który może mieć tylko 
-- dwa stany: wolny (1) lub zajęty (0). Działa jak mutex - tylko jeden proces
-- może jednocześnie przejść przez sekcję krytyczną.

with Ada.Text_IO;
package body Semafor_Ogolny is
   
   -- Obiekt chroniony implementujący semafor binarny
   protected type Binary_Semaphore is
      -- P() - operacja "zajmij" (z niem. "proberen" - próbować)
      -- Jeśli semafor jest wolny (True), zajmuje go i pozwala przejść
      -- Jeśli zajęty (False), blokuje zadanie do momentu zwolnienia
      entry P;
      
      -- V() - operacja "zwolnij" (z niem. "verhogen" - zwiększyć)  
      -- Zwalnia semafor, pozwalając innemu zadaniu na wejście
      procedure V;
      
   private
      -- Zmienna stanu semafora: True = wolny, False = zajęty
      Available : Boolean := True;  -- Początkowo semafor jest wolny
   end Binary_Semaphore;
   
end Semafor_Ogolny;

package body Semafor_Ogolny is
   
   protected body Binary_Semaphore is
      
      -- Implementacja operacji P() (zajmij)
      entry P when Available is  -- Warunek: wykonuj tylko gdy Available = True
      begin
         -- Gdy zadanie wchodzi tutaj, automatycznie zajmuje semafor
         Available := False;
         Ada.Text_IO.Put_Line("Semafor zajęty przez zadanie");
      end P;
      
      -- Implementacja operacji V() (zwolnij) 
      procedure V is
      begin
         -- Zwalniamy semafor - inne zadania mogą teraz wejść do P()
         Available := True;
         Ada.Text_IO.Put_Line("Semafor zwolniony");
      end V;
      
   end Binary_Semaphore;
   
end Semafor_Ogolny;

