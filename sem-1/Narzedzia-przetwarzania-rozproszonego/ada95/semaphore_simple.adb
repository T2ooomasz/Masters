with Ada.Text_IO; use Ada.Text_IO;

procedure Semaphore_simple is
   protected type Semaphore (Initial : Natural := 1) is
      entry P;
      procedure V;
   private
      Count : Natural := Initial;
   end Semaphore;

   protected body Semaphore is
      entry P when Count > 0 is
      begin
         Count := Count - 1;
      end P;

      procedure V is
      begin
         Count := Count + 1;
      end V;
   end Semaphore;

   S : Semaphore(0);  -- Semafor zainicjalizowany na 0

   task T1;
   task T2;

   task body T1 is
   begin
      Put_Line("T1: Czekam na semaforze");
      S.P;  -- Czeka, aż semafor będzie > 0
      Put_Line("T1: Semafor odblokowany");
   end T1;

   task body T2 is
   begin
      Put_Line("T2: Zwalniam semafor");
      S.V;  -- Zwiększa semafor, odblokowując T1
   end T2;

begin
   null;  -- Główny program nic nie robi
end Semaphore_simple;