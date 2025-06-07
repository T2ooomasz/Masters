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
