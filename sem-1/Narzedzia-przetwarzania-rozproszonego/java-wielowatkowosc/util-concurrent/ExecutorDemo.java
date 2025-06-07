// =============================================================================
// DEMO RÓŻNYCH EGZEKUTORÓW - ExecutorDemo.java
// =============================================================================

import java.util.concurrent.*;
import java.util.List;
import java.util.ArrayList;

/**
 * Klasa demonstrująca różne typy egzekutorów dostępnych w java.util.concurrent.
 * Pokazuje charakterystyki i zastosowania każdego typu.
 */
public class ExecutorDemo {
    
    public static void main(String[] args) {
        System.out.println("🔧 DEMONSTRACJA RÓŻNYCH TYPÓW EGZEKUTORÓW\n");
        
        IBuffer buffer = new ConcurrentBuffer(5);
        
        try {
            // 1. SingleThreadExecutor
            demonstrateSingleThreadExecutor(buffer);
            Thread.sleep(2000);
            
            // 2. FixedThreadPool
            demonstrateFixedThreadPool(buffer);
            Thread.sleep(2000);
            
            // 3. CachedThreadPool
            demonstrateCachedThreadPool(buffer);
            Thread.sleep(2000);
            
            // 4. ScheduledThreadPool
            demonstrateScheduledThreadPool(buffer);
            Thread.sleep(5000);
            
            // 5. WorkStealingPool (Java 8+)
            demonstrateWorkStealingPool(buffer);
            Thread.sleep(2000);
            
        } catch (InterruptedException e) {
            System.err.println("Demo przerwane: " + e.getMessage());
        }
        
        System.out.println("\n✅ Demonstracja zakończona!");
    }
    
    /**
     * SingleThreadExecutor - jeden wątek, kolejność wykonania zachowana.
     */
    private static void demonstrateSingleThreadExecutor(IBuffer buffer) 
            throws InterruptedException {
        System.out.println("1️⃣ SingleThreadExecutor - jeden wątek roboczy");
        
        ExecutorService executor = Executors.newSingleThreadExecutor();
        
        // Wszystkie zadania będą wykonywane sekwencyjnie
        for (int i = 1; i <= 3; i++) {
            final int taskId = i;
            executor.submit(() -> {
                System.out.println("   Zadanie " + taskId + " w wątku: " + 
                    Thread.currentThread().getName());
                buffer.put(taskId);
            });
        }
        
        shutdownAndWait(executor, "SingleThreadExecutor");
    }
    
    /**
     * FixedThreadPool - stała liczba wątków, idealne dla znanych obciążeń.
     */
    private static void demonstrateFixedThreadPool(IBuffer buffer) 
            throws InterruptedException {
        System.out.println("2️⃣ FixedThreadPool - stała pula 2 wątków");
        
        ExecutorService executor = Executors.newFixedThreadPool(2);
        
        // Maksymalnie 2 zadania równolegle
        for (int i = 1; i <= 4; i++) {
            final int taskId = i;
            executor.submit(() -> {
                System.out.println("   Zadanie " + taskId + " w wątku: " + 
                    Thread.currentThread().getName());
                buffer.put(taskId + 10);
                try { Thread.sleep(500); } catch (InterruptedException e) {}
            });
        }
        
        shutdownAndWait(executor, "FixedThreadPool");
    }
    
    /**
     * CachedThreadPool - dynamiczne tworzenie wątków, idealne dla krótkich zadań.
     */
    private static void demonstrateCachedThreadPool(IBuffer buffer) 
            throws InterruptedException {
        System.out.println("3️⃣ CachedThreadPool - dynamiczne wątki");
        
        ExecutorService executor = Executors.newCachedThreadPool();
        
        // Każde zadanie może dostać swój wątek
        for (int i = 1; i <= 3; i++) {
            final int taskId = i;
            executor.submit(() -> {
                System.out.println("   Zadanie " + taskId + " w wątku: " + 
                    Thread.currentThread().getName());
                buffer.put(taskId + 20);
            });
        }
        
        shutdownAndWait(executor, "CachedThreadPool");
    }
    
    /**
     * ScheduledThreadPool - zadania okresowe i opóźnione.
     */
    private static void demonstrateScheduledThreadPool(IBuffer buffer) 
            throws InterruptedException {
        System.out.println("4️⃣ ScheduledThreadPool - zadania okresowe");
        
        ScheduledExecutorService executor = Executors.newScheduledThreadPool(2);
        
        // Zadanie z opóźnieniem
        executor.schedule(() -> {
            System.out.println("   Zadanie opóźnione wykonane!");
            buffer.put(100);
        }, 1, TimeUnit.SECONDS);
        
        // Zadanie okresowe
        ScheduledFuture<?> periodicTask = executor.scheduleAtFixedRate(() -> {
            System.out.println("   Zadanie okresowe: " + System.currentTimeMillis());
        }, 0, 1, TimeUnit.SECONDS);
        
        // Zatrzymaj po 3 sekundach
        executor.schedule(() -> {
            periodicTask.cancel(false);
            System.out.println("   Zadanie okresowe zatrzymane");
        }, 3, TimeUnit.SECONDS);
        
        Thread.sleep(4000);
        shutdownAndWait(executor, "ScheduledThreadPool");
    }
    
    /**
     * WorkStealingPool - work-stealing algorithm, wykorzystuje wszystkie rdzenie CPU.
     */
    private static void demonstrateWorkStealingPool(IBuffer buffer) 
            throws InterruptedException {
        System.out.println("5️⃣ WorkStealingPool - work-stealing algorithm");
        
        ExecutorService executor = Executors.newWorkStealingPool();
        
        List<Future<?>> futures = new ArrayList<>();
        
        // Zadania o różnym czasie wykonania
        for (int i = 1; i <= 4; i++) {
            final int taskId = i;
            Future<?> future = executor.submit(() -> {
                try {
                    Thread.sleep(taskId * 200); // Różne czasy wykonania
                    System.out.println("   WorkStealing zadanie " + taskId + 
                        " w wątku: " + Thread.currentThread().getName());
                    buffer.put(taskId + 30);
                } catch (InterruptedException e) {
                    Thread.currentThread().interrupt();
                }
            });
            futures.add(future);
        }
        
        // Czekaj na wszystkie zadania
        for (Future<?> future : futures) {
            try {
                future.get();
            } catch (ExecutionException e) {
                System.err.println("Błąd zadania: " + e.getCause());
            }
        }
        
        shutdownAndWait(executor, "WorkStealingPool");
    }
    
    /**
     * Pomocnicza metoda do zamykania egzekutorów.
     */
    private static void shutdownAndWait(ExecutorService executor, String name) 
            throws InterruptedException {
        executor.shutdown();
        if (!executor.awaitTermination(5, TimeUnit.SECONDS)) {
            System.err.println("   ⚠️ " + name + " nie zamknął się w czasie");
            executor.shutdownNow();
        }
        System.out.println("   ✅ " + name + " zamknięty\n");
    }
}
