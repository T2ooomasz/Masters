package Semafor_Uogolniony is
   
   protected type Counting_Semaphore (Initial_Count : Natural) is
      -- Acquire exactly 1 resource
      entry P;
      
      -- Release exactly 1 resource
      procedure V;
      
      -- Debug helper
      function Current_Value return Natural;
      
   private
      Count : Natural := Initial_Count;
   end Counting_Semaphore;

end Semafor_Uogolniony;