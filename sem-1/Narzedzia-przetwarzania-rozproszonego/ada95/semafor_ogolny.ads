-- =====================================
-- PLIK: semafor_ogolny.ads
-- SPECYFIKACJA SEMAFORA BINARNEGO
-- =====================================

package Semafor_Ogolny is
   
   -- Obiekt chroniony implementujący semafor binarny
   -- Semafor binarny może mieć tylko dwa stany: wolny (True) lub zajęty (False)
   -- Działa jak mutex - tylko jeden proces może jednocześnie przejść przez sekcję krytyczną
   protected type Binary_Semaphore is
      -- P() - operacja "zajmij" (z niemieckiego "proberen" - próbować)
      -- Jeśli semafor jest wolny, zajmuje go i pozwala przejść
      -- Jeśli zajęty, blokuje zadanie do momentu zwolnienia
      entry P;
      
      -- V() - operacja "zwolnij" (z niemieckiego "verhogen" - zwiększyć)  
      -- Zwalnia semafor, pozwalając innemu zadaniu na wejście
      procedure V;
      
   private
      -- Zmienna stanu semafora: True = wolny, False = zajęty
      -- Początkowo semafor jest wolny
      Available : Boolean := True;
   end Binary_Semaphore;
   
end Semafor_Ogolny;