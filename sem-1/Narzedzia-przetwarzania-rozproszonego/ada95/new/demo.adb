-- ===============================================
-- PLIK: demo.adb
-- Kompilacja i uruchomienie (w terminalu):
-- > gnatmake demo.adb
-- > ./demo
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
      -- --- KONIEC SEKCJI KRYTYCZNEJ ---
      
      Mutex.V;  -- Zwolnij dostęp, aby inne zadanie mogło wejść.
      
      Put_Line("Zadanie " & Image(My_ID) & " zakończyło pracę.");
   end Zadanie_Binarne;
   
   -- Tworzymy 3 zadania, które będą rywalizować o dostęp
   Zadania_1 : array (1 .. 3) of Zadanie_Binarne;

   --------------------------------------------------------------------
   -- CZĘŚĆ 2: Demonstracja semafora uogólnionego (zarządzanie pulą zasobów)
   --------------------------------------------------------------------
   
   -- Tworzymy semafor liczący z pulą 5 dostępnych zasobów
   Pula_Zasobow : Semafor_Uogolniony.Counting_Semaphore(Initial_Count => 5);

   -- Definicja typu zadania, które będzie prosiło o zasoby
   task type Zadanie_Uogolnione;
   
   task body Zadanie_Uogolnione is
      package Losowe_Zasoby is new Ada.Numerics.Discrete_Random(Positive range 1 .. 3);
      Gen : Losowe_Zasoby.Generator;
      Ile_Potrzeba : Positive;
      My_ID : constant Task_Id := Current_Task;
   begin
      Losowe_Zasoby.Reset(Gen);
      Ile_Potrzeba := Losowe_Zasoby.Random(Gen);
      
      Put_Line("Zadanie " & Image(My_ID) & " potrzebuje " & Positive'Image(Ile_Potrzeba) & " zasobów.");
      
      Pula_Zasobow.P(Ile_Potrzeba); -- Poproś o 'Ile_Potrzeba' zasobów.
      
      -- Posiadamy zasoby, wykonujemy pracę
      Put_Line("!!! Zadanie " & Image(My_ID) & " dostało zasoby i pracuje... !!!");
      delay 3.0; -- Symulacja pracy
      
      Pula_Zasobow.V(Ile_Potrzeba); -- Zwróć zasoby do puli.
      
      Put_Line("Zadanie " & Image(My_ID) & " zakończyło pracę i zwróciło zasoby.");
   end Zadanie_Uogolnione;
   
   -- Tworzymy 5 zadań, które będą rywalizować o zasoby z puli
   Zadania_2 : array (1 .. 5) of Zadanie_Uogolnione;
   
begin
   New_Line;
   Put_Line("--- START DEMONSTRACJI SEMAFORA BINARNEGO ---");
   -- Czekamy, aż zadania z części 1 się utworzą (nie jest to konieczne, ale dla czytelności)
   delay 0.1; 
   New_Line;
   Put_Line("Czekanie na zakończenie zadań binarnych...");
   -- Główny program poczeka, aż wszystkie zadania typu Zadanie_Binarne się zakończą
   
   delay 8.0; -- Dajemy im czas na wykonanie
   
   New_Line;
   Put_Line("--- START DEMONSTRACJI SEMAFORA UOGÓLNIONEGO ---");
   -- Podobnie jak wyżej, czekamy na zakończenie zadań z części 2.
   -- Główny program naturalnie czeka na zakończenie wszystkich swoich "dzieci" (zadań).
end Demo_Semaforow;