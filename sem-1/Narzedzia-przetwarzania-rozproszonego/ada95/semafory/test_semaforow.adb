- =====================================
-- PLIK: test_semaforow.adb
-- PROGRAM GŁÓWNY TESTUJĄCY OBA SEMAFORY
-- =====================================

with Ada.Text_IO;
with Semafor_Ogolny;
with Semafor_Uogolniony;

procedure Test_Semaforow is
   
   -- Tworzenie instancji semaforów
   Sem_Bin : Semafor_Ogolny.Binary_Semaphore;          -- Semafor binarny
   Sem_Count : Semafor_Uogolniony.Counting_Semaphore(3); -- Semafor z 3 zasobami
   
   -- Definicja typu zadania testującego semafor binarny
   task type Binary_Task is
      entry Start(ID : Integer);  -- Punkt wejścia do przekazania ID zadania
   end Binary_Task;
   
   -- Definicja typu zadania testującego semafor liczący  
   task type Counting_Task is
      entry Start(ID : Integer);  -- Punkt wejścia do przekazania ID zadania
   end Counting_Task;
   
   -- Implementacja zadania dla semafora binarnego
   task body Binary_Task is
      Task_ID : Integer;  -- ID zadania otrzymane przez Start
   begin
      -- Czekamy na otrzymanie ID zadania
      accept Start(ID : Integer) do
         Task_ID := ID;
      end Start;
      
      Ada.Text_IO.Put_Line("Zadanie bin" & Integer'Image(Task_ID) & " próbuje wejść...");
      
      -- Zajmujemy semafor (wejście do sekcji krytycznej)
      -- Jeśli semafor zajęty, zadanie będzie czekać tutaj
      Sem_Bin.P;
      
      Ada.Text_IO.Put_Line("Zadanie bin" & Integer'Image(Task_ID) & " w sekcji krytycznej");
      delay 2.0;  -- Symulacja pracy w sekcji krytycznej (2 sekundy)
      
      -- Zwalniamy semafor (wyjście z sekcji krytycznej)
      Sem_Bin.V;
      
      Ada.Text_IO.Put_Line("Zadanie bin" & Integer'Image(Task_ID) & " zakończone");
   end Binary_Task;
   
   -- Implementacja zadania dla semafora liczącego
   task body Counting_Task is
      Task_ID : Integer;  -- ID zadania otrzymane przez Start
   begin
      -- Czekamy na otrzymanie ID zadania
      accept Start(ID : Integer) do
         Task_ID := ID;
      end Start;
      
      Ada.Text_IO.Put_Line("Zadanie count" & Integer'Image(Task_ID) & " chce zasób...");
      
      -- Zajmujemy zasób z puli
      -- Jeśli wszystkie zasoby zajęte (Count = 0), zadanie będzie czekać
      Sem_Count.P;
      
      Ada.Text_IO.Put_Line("Zadanie count" & Integer'Image(Task_ID) & " ma zasób");
      delay 1.5;  -- Symulacja używania zasobu (1.5 sekundy)
      
      -- Zwalniamy zasób z powrotem do puli
      Sem_Count.V;
      
      Ada.Text_IO.Put_Line("Zadanie count" & Integer'Image(Task_ID) & " zakończone");
   end Counting_Task;
   
   -- Deklaracje tablic zadań do testowania
   Binary_Tasks : array(1..3) of Binary_Task;    -- 3 zadania dla semafora binarnego
   Counting_Tasks : array(1..5) of Counting_Task; -- 5 zadań dla semafora liczącego
   
begin
   Ada.Text_IO.Put_Line("=== TEST SEMAFORA BINARNEGO ===");
   Ada.Text_IO.Put_Line("Tylko jedno zadanie może być w sekcji krytycznej jednocześnie");
   Ada.Text_IO.New_Line;
   
   -- Uruchamiamy 3 zadania dla semafora binarnego
   -- Każde zadanie otrzymuje swoje unikalne ID
   for I in Binary_Tasks'Range loop
      Binary_Tasks(I).Start(I);
   end loop;
   
   delay 8.0;  -- Czekamy na zakończenie testów binarnych (3 zadania * ~2s każde)
   
   Ada.Text_IO.New_Line;
   Ada.Text_IO.Put_Line("=== TEST SEMAFORA LICZĄCEGO ===");
   Ada.Text_IO.Put_Line("Maksymalnie 3 zadania mogą jednocześnie mieć zasób");
   Ada.Text_IO.Put_Line("Uruchamiamy 5 zadań - 2 będą musiały czekać");
   Ada.Text_IO.New_Line;
   
   -- Uruchamiamy 5 zadań dla semafora liczącego
   -- To więcej niż dostępnych zasobów (3), więc część zadań będzie czekać
   for I in Counting_Tasks'Range loop
      Counting_Tasks(I).Start(I);
   end loop;
   
   -- Program kończy się automatycznie gdy wszystkie zadania się zakończą
   -- Ada automatycznie czeka na zakończenie wszystkich zadań
   
end Test_Semaforow;

-- =====================================
-- INSTRUKCJE KOMPILACJI I URUCHOMIENIA
-- =====================================

-- Aby skompilować i uruchomić program:
-- 
-- 1. Zapisz każdy blok kodu w osobnym pliku:
--    - semafor_ogolny.ads (specyfikacja semafora binarnego)
--    - semafor_ogolny.adb (implementacja semafora binarnego)  
--    - semafor_uogolniony.ads (specyfikacja semafora liczącego)
--    - semafor_uogolniony.adb (implementacja semafora liczącego)
--    - test_semaforow.adb (program główny)
--
-- 2. Kompiluj w kolejności:
--    gnatmake test_semaforow.adb
--
-- 3. Uruchom:
--    ./test_semaforow
--
-- OCZEKIWANE ZACHOWANIE:
-- - Dla semafora binarnego: zadania wykonują się sekwencyjnie (jedno po drugim)
-- - Dla semafora liczącego: maksymalnie 3 zadania działają równolegle, 
--   pozostałe 2 czekają na zwolnienie zasobu

-- =====================================
-- WYJAŚNIENIE DLACZEGO TO JEST POPRAWNE
-- =====================================

-- POPRAWNOŚĆ SEMAFORA BINARNEGO:
-- 1. Stan Available kontroluje dostęp - tylko gdy True, P() może być wykonane
-- 2. P() atomowo zmienia stan na False, gwarantując wyłączność
-- 3. V() zawsze może być wykonane i ustawia stan na True
-- 4. Brak możliwości deadlock - V() zawsze odblokowuje P()

-- POPRAWNOŚĆ SEMAFORA LICZĄCEGO:
-- 1. Count reprezentuje liczbę dostępnych zasobów
-- 2. P() może być wykonane tylko gdy Count > 0, zapewnia dostępność zasobu
-- 3. P() atomowo zmniejsza Count, rezerwując zasób
-- 4. V() atomowo zwiększa Count, zwalniając zasób
-- 5. Nie ma możliwości przekroczenia początkowej liczby zasobów

-- BEZPIECZEŃSTWO OBIEKTÓW CHRONIONYCH:
-- - Ada gwarantuje wzajemne wykluczenie dla operacji na obiekcie chronionym
-- - Entry z warunkiem realizuje semantykę blokowania/odblokowywania
-- - Brak możliwości wyścigów (race conditions)
-- - Automatyczne zarządzanie kolejkami oczekujących zadań