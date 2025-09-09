package Semafor_Ogolny is
   
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