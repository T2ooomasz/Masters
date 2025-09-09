-- ===============================================
-- PLIK: demo_semaforow.adb (SEKWENCYJNA WERSJA)
-- Kompilacja i uruchomienie (w terminalu):
-- > gnatmake demo_semaforow.adb
-- > ./demo_semaforow
-- ===============================================

with Ada.Text_IO;                use Ada.Text_IO;
with Ada.Numerics.Discrete_Random;
with Ada.Task_Identification;    use Ada.Task_Identification;

-- Importujemy nasze pakiety z semaforami
with Semafor_Ogolny;
with Semafor_Uogolniony;

procedure Demo_Semaforow is
   
   --------------------------------------------------------------------
   -- CZĘŚĆ 1: Demonstracja semafora binarnego (działa jak zamek)
   --------------------------------------------------------------------
   
   procedure Demonstruj_Semafor_Binarny is
      -- Tworzymy jeden semafor binarny, który będzie chronił "sekcję krytyczną"
      Mutex : Semafor_Ogolny.Binary_Semaphore;
      
      -- Definicja typu zadania, które będzie próbowało uzyskać dostęp do sekcji
      task type Zadanie_Binarne;
      
      -- Implementacja zadania
      task body Zadanie_Binarne is
         My_ID : constant Task_Id := Current_Task;
      begin
         Put_Line("Zadanie " & Image(My_ID) & " startuje i próbuje wejść do sekcji.");
         
         Mutex.P;  -- Poproś o dostęp. Zablokuje zadanie, jeśli sekcja jest zajęta.
         
         -- --- POCZĄTEK SEKCJI KRYTYCZNEJ ---
         -- Tylko jedno zadanie może być tutaj w danym momencie.
         Put_Line("!!! Zadanie " & Image(My_ID) & " JEST W SEKCJI KRYTYCZNEJ !!!");
         delay 2.0; -- Symulacja pracy w sekcji krytycznej
         Put_Line("    Zadanie " & Image(My_ID) & " kończy pracę w sekcji krytycznej");
         -- --- KONIEC SEKCJI KRYTYCZNEJ ---
         
         Mutex.V;  -- Zwolnij dostęp, aby inne zadanie mogło wejść.
         
         Put_Line("Zadanie " & Image(My_ID) & " zakończyło pracę.");
      end Zadanie_Binarne;
      
      -- Tworzymy tablicę zadań - proste i działa
      Zadania : array (1 .. 3) of Zadanie_Binarne;
      
   begin
      Put_Line("=== Utworzono 3 zadania dla semafora binarnego ===");
      -- Ta procedura zakończy się dopiero gdy wszystkie zadania się zakończą
      -- (automatyczne czekanie na zadania potomne)
   end Demonstruj_Semafor_Binarny;

   --------------------------------------------------------------------
   -- CZĘŚĆ 2: Demonstracja semafora uogólnionego (zarządzanie pulą zasobów)
   --------------------------------------------------------------------
   
   procedure Demonstruj_Semafor_Uogolniony is
      -- Tworzymy semafor liczący z pulą 3 dostępnych zasobów
      Pula_Zasobow : Semafor_Uogolniony.Counting_Semaphore(Initial_Count => 3);

      -- Definicja typu zadania, które będzie prosiło o zasoby
      task type Zadanie_Uogolnione;
      
      task body Zadanie_Uogolnione is
         -- Ograniczamy zakres losowości, aby uniknąć deadlocka
         subtype Losowy_Zakres is Positive range 1 .. 2;  -- Maksymalnie 2 zasoby
         package Losowe_Zasoby is new Ada.Numerics.Discrete_Random(Losowy_Zakres);
         Gen : Losowe_Zasoby.Generator;
         Ile_Potrzeba : Positive;
         My_ID : constant Task_Id := Current_Task;
      begin
         Losowe_Zasoby.Reset(Gen);
         Ile_Potrzeba := Losowe_Zasoby.Random(Gen);
         
         Put_Line("Zadanie " & Image(My_ID) & " potrzebuje" & Positive'Image(Ile_Potrzeba) & " zasobów.");
         
         Pula_Zasobow.P(Ile_Potrzeba); -- Poproś o 'Ile_Potrzeba' zasobów.
         
         -- Posiadamy zasoby, wykonujemy pracę
         Put_Line("!!! Zadanie " & Image(My_ID) & " dostało zasoby i pracuje... !!!");
         delay 1.5; -- Symulacja pracy
         
         Pula_Zasobow.V(Ile_Potrzeba); -- Zwróć zasoby do puli.
         
         Put_Line("Zadanie " & Image(My_ID) & " zakończyło pracę i zwróciło zasoby.");
      end Zadanie_Uogolnione;
      
      -- Tworzymy tablicę zadań - proste i działa
      Zadania : array (1 .. 4) of Zadanie_Uogolnione;
      
   begin
      Put_Line("=== Utworzono 4 zadania dla semafora uogólnionego ===");
      -- Ta procedura zakończy się dopiero gdy wszystkie zadania się zakończą
   end Demonstruj_Semafor_Uogolniony;

begin
   New_Line;
   Put_Line("##########################################");
   Put_Line("# DEMONSTRACJA SEMAFORÓW W ADA          #");
   Put_Line("# WERSJA SEKWENCYJNA                    #");
   Put_Line("##########################################");
   New_Line;
   
   Put_Line("--- START DEMONSTRACJI SEMAFORA BINARNEGO ---");
   Put_Line("Semafor binarny działa jak mutex - tylko jeden proces może");
   Put_Line("jednocześnie przejść przez sekcję krytyczną.");
   New_Line;
   
   -- FAZA 1: Semafor binarny
   Demonstruj_Semafor_Binarny;
   
   New_Line;
   Put_Line("--- ZAKOŃCZONO DEMONSTRACJĘ SEMAFORA BINARNEGO ---");
   Put_Line("--- Wszystkie zadania binarne zostały zakończone ---");
   New_Line;
   delay 1.0; -- Krótka pauza między demonstracjami dla czytelności
   
   Put_Line("--- START DEMONSTRACJI SEMAFORA UOGÓLNIONEGO ---");
   Put_Line("Semafor liczący zarządza pulą zasobów - wiele procesów może");
   Put_Line("jednocześnie korzystać z zasobów, ale w ograniczonej liczbie.");
   New_Line;
   
   -- FAZA 2: Semafor uogólniony (uruchomi się dopiero po zakończeniu fazy 1)
   Demonstruj_Semafor_Uogolniony;
   
   New_Line;
   Put_Line("--- ZAKOŃCZONO DEMONSTRACJĘ SEMAFORA UOGÓLNIONEGO ---");
   Put_Line("--- Wszystkie zadania uogólnione zostały zakończone ---");
   New_Line;
   Put_Line("##########################################");
   Put_Line("# DEMONSTRACJA ZAKOŃCZONA POMYŚLNIE     #");
   Put_Line("##########################################");
end Demo_Semaforow;