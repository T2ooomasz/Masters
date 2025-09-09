package Semafor_Uogolniony is
   
   protected type Counting_Semaphore (Initial_Count : Natural) is
      -- Acquire Ile resources (may block until available)
      entry P (Ile : Positive);
      
      -- Release Ile resources
      procedure V (Ile : Positive);
      
      -- Debug helper
      function Current_Value return Natural;
      
   private
      entry Wait (Ile : Positive);  -- private requeue target
      Count : Natural := Initial_Count;
   end Counting_Semaphore;

end Semafor_Uogolniony;