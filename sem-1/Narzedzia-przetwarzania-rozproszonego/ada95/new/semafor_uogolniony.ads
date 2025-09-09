package Semafor_Uogolniony is
   
   protected type Counting_Semaphore (Initial_Count : Natural) is
      -- Acquire Ile resources (block until available)
      entry P (Ile : Positive);
      
      -- Release Ile resources
      procedure V (Ile : Positive);
      
      -- Debug helper
      function Current_Value return Natural;
      
   private
      Count : Natural := Initial_Count;
   end Counting_Semaphore;

end Semafor_Uogolniony;