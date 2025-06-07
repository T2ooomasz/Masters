-- =====================================
-- ZADANIE 1: SEMAFOR OGÓLNY (BINARY)
-- =====================================

-- Semafor ogólny (binarny) to mechanizm synchronizacji, który może mieć tylko 
-- dwa stany: wolny (1) lub zajęty (0). Działa jak mutex - tylko jeden proces
-- może jednocześnie przejść przez sekcję krytyczną.

with Ada.Text_IO;
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

-- =====================================
-- PROGRAM TESTUJĄCY OBA SEMAFORY
-- =====================================

with Ada.Text_IO;
with Semafor_Ogolny;
with Semafor_Uogolniony;

procedure Test_Semaforow is
   
   -- Tworzenie instancji semaforów
   Sem_Bin : Semafor_Ogolny.Binary_Semaphore;        -- Semafor binarny
   Sem_Count : Semafor_Uogolniony.Counting_Semaphore(3); -- Semafor z 3 zasobami
   
   -- Zadanie testujące semafor binarny
   task type Binary_Task is
      entry Start(ID : Integer);
   end Binary_Task;
   
   -- Zadanie testujące semafor liczący  
   task type Counting_Task is
      entry Start(ID : Integer);
   end Counting_Task;
   
   task body Binary_Task is
      Task_ID : Integer;
   begin
      accept Start(ID : Integer) do
         Task_ID := ID;
      end Start;
      
      Ada.Text_IO.Put_Line("Zadanie" & Integer'Image(Task_ID) & " próbuje wejść...");
      
      -- Zajmujemy semafor (sekcja krytyczna)
      Sem_Bin.P;
      
      Ada.Text_IO.Put_Line("Zadanie" & Integer'Image(Task_ID) & " w sekcji krytycznej");
      delay 2.0;  -- Symulacja pracy w sekcji krytycznej
      
      -- Zwalniamy semafor
      Sem_Bin.V;
      
      Ada.Text_IO.Put_Line("Zadanie" & Integer'Image(Task_ID) & " zakończone");
   end Binary_Task;
   
   task body Counting_Task is
      Task_ID : Integer;
   begin
      accept Start(ID : Integer) do
         Task_ID := ID;
      end Start;
      
      Ada.Text_IO.Put_Line("Zadanie" & Integer'Image(Task_ID) & " chce zasób...");
      
      -- Zajmujemy zasób
      Sem_Count.P;
      
      Ada.Text_IO.Put_Line("Zadanie" & Integer'Image(Task_ID) & " ma zasób");
      delay 1.0;  -- Symulacja używania zasobu
      
      -- Zwalniamy zasób
      Sem_Count.V;
      
      Ada.Text_IO.Put_Line("Zadanie" & Integer'Image(Task_ID) & " zakończone");
   end Counting_Task;
   
   -- Tablice zadań do testowania
   Binary_Tasks : array(1..3) of Binary_Task;
   Counting_Tasks : array(1..5) of Counting_Task;
   
begin
   Ada.Text_IO.Put_Line("=== TEST SEMAFORA BINARNEGO ===");
   Ada.Text_IO.Put_Line("Tylko jedno zadanie może być w sekcji krytycznej");
   
   -- Uruchamiamy 3 zadania dla semafora binarnego
   for I in Binary_Tasks'Range loop
      Binary_Tasks(I).Start(I);
   end loop;
   
   delay 8.0;  -- Czekamy na zakończenie testów binarnych
   
   Ada.Text_IO.Put_Line("");
   Ada.Text_IO.Put_Line("=== TEST SEMAFORA LICZĄCEGO ===");
   Ada.Text_IO.Put_Line("Maksymalnie 3 zadania mogą jednocześnie mieć zasób");
   
   -- Uruchamiamy 5 zadań dla semafora liczącego (więcej niż dostępnych zasobów)
   for I in Counting_Tasks'Range loop
      Counting_Tasks(I).Start(I);
   end loop;
   
   -- Program kończy się automatycznie gdy wszystkie zadania się zakończą
   
end Test_Semaforow;

-- =====================================
-- WYJAŚNIENIE TEORETYCZNE
-- =====================================

-- SEMAFOR BINARNY (OGÓLNY):
-- - Może mieć wartość 0 lub 1
-- - Używany do ochrony sekcji krytycznej (mutex)
-- - P() blokuje jeśli semafor = 0, ustawia na 0 jeśli był 1
-- - V() ustawia semafor na 1
-- - Gwarantuje wyłączność dostępu

-- SEMAFOR LICZĄCY (UOGÓLNIONY):  
-- - Może mieć wartość >= 0
-- - Używany do zarządzania pulą identycznych zasobów
-- - P() zmniejsza licznik, blokuje jeśli licznik = 0
-- - V() zwiększa licznik
-- - Pozwala na N jednoczesnych dostępów dla N zasobów

-- DLACZEGO TO ROZWIĄZANIE JEST POPRAWNE:
-- 1. Obiekty chronione w Ada gwarantują wzajemne wykluczenie
-- 2. Entry z warunkiem realizuje semantykę blokowania P()
-- 3. Procedure V() zawsze może być wykonana (odblokowanie)
-- 4. Stan semafora jest prawidłowo zarządzany
-- 5. Brak możliwości wyścigów (race conditions) dzięki ochronie