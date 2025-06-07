// =============================================================================
// GŁÓWNA APLIKACJA TESTOWA - ProducerConsumerApp.java
// =============================================================================

import java.util.concurrent.*;
import java.util.concurrent.atomic.AtomicInteger;

/**
 * Główna aplikacja demonstrująca działanie Producer-Consumer pattern
 * z wykorzystaniem java.util.concurrent i egzekutorów.
 */
public class ProducerConsumerApp {
    
    // Konfiguracja symulacji
    private static final int BUFFER_SIZE = 10;          // Rozmiar bufora
    private static final int PRODUCERS_COUNT = 3;       // Liczba producentów
    private static final int CONSUMERS_COUNT = 3;       // Liczba konsumentów
    private static final int ITEMS_PER_PRODUCER = 20;   // Elementy na producenta
    private static final int ITEMS_PER_CONSUMER = 20;   // Elementy na konsumenta
    private static final long PRODUCER_DELAY = 100;     // Opóźnienie producenta (ms)
    private static final long CONSUMER_DELAY = 150;     // Opóźnienie konsumenta (ms)
    
    public static void main(String[] args) {
        System.out.println("🚀 Uruchamianie systemu Producer-Consumer");
        System.out.println("📊 Konfiguracja:");
        System.out.println("   - Rozmiar bufora: " + BUFFER_SIZE);
        System.out.println("   - Liczba producentów: " + PRODUCERS_COUNT);
        System.out.println("   - Liczba konsumentów: " + CONSUMERS_COUNT);
        System.out.println("   - Elementy na producenta: " + ITEMS_PER_PRODUCER);
        System.out.println("   - Elementy na konsumenta: " + ITEMS_PER_CONSUMER);
        System.out.println();
        
        // Tworzenie bufora
        IBuffer buffer = new ConcurrentBuffer(BUFFER_SIZE);
        
        // Tworzenie różnych typów egzekutorów dla demonstracji
        
        // 1. FixedThreadPool dla producentów - stała liczba wątków
        ExecutorService producerExecutor = Executors.newFixedThreadPool(PRODUCERS_COUNT);
        
        // 2. CachedThreadPool dla konsumentów - dynamiczne tworzenie wątków
        ExecutorService consumerExecutor = Executors.newCachedThreadPool();
        
        // 3. ScheduledExecutorService dla zadań monitorujących
        ScheduledExecutorService monitorExecutor = Executors.newScheduledThreadPool(1);
        
        try {
            // Uruchomienie zadania monitorującego stan bufora
            startBufferMonitoring(buffer, monitorExecutor);
            
            // Tworzenie i uruchamianie producentów
            System.out.println("🏭 Uruchamianie producentów...");
            AtomicInteger producerStartValue = new AtomicInteger(101);
            
            for (int i = 1; i <= PRODUCERS_COUNT; i++) {
                String name = "P" + i;
                int startValue = producerStartValue.getAndAdd(ITEMS_PER_PRODUCER);
                
                ProducerTask producer = new ProducerTask(
                    name, buffer, ITEMS_PER_PRODUCER, startValue, PRODUCER_DELAY
                );
                
                producerExecutor.submit(producer);
            }
            
            // Małe opóźnienie przed uruchomieniem konsumentów
            Thread.sleep(200);
            
            // Tworzenie i uruchamianie konsumentów
            System.out.println("🛒 Uruchamianie konsumentów...");
            for (int i = 1; i <= CONSUMERS_COUNT; i++) {
                String name = "C" + i;
                
                ConsumerTask consumer = new ConsumerTask(
                    name, buffer, ITEMS_PER_CONSUMER, CONSUMER_DELAY
                );
                
                consumerExecutor.submit(consumer);
            }
            
            // Demonstracja różnych sposobów zamykania egzekutorów
            
            // Zamknij producentów i czekaj na zakończenie
            producerExecutor.shutdown();
            if (!producerExecutor.awaitTermination(30, TimeUnit.SECONDS)) {
                System.err.println("⚠️ Producenci nie zakończyli w czasie - forsowanie zamknięcia");
                producerExecutor.shutdownNow();
            }
            
            // Zamknij konsumentów i czekaj na zakończenie
            consumerExecutor.shutdown();
            if (!consumerExecutor.awaitTermination(30, TimeUnit.SECONDS)) {
                System.err.println("⚠️ Konsumenci nie zakończyli w czasie - forsowanie zamknięcia");
                consumerExecutor.shutdownNow();
            }
            
            // Zamknij monitor
            monitorExecutor.shutdown();
            
            System.out.println();
            System.out.println("🎉 Wszystkie zadania zakończone pomyślnie!");
            
            // Podsumowanie
            printSummary(buffer);
            
        } catch (InterruptedException e) {
            System.err.println("❌ Główny wątek przerwany: " + e.getMessage());
            
            // Forsuj zamknięcie wszystkich egzekutorów
            producerExecutor.shutdownNow();
            consumerExecutor.shutdownNow();
            monitorExecutor.shutdownNow();
            
        } catch (Exception e) {
            System.err.println("❌ Nieoczekiwany błąd: " + e.getMessage());
            e.printStackTrace();
        }
    }
    
    /**
     * Uruchamia okresowe monitorowanie stanu bufora.
     * @param buffer bufor do monitorowania
     * @param executor egzekutor dla zadań okresowych
     */
    private static void startBufferMonitoring(IBuffer buffer, ScheduledExecutorService executor) {
        if (buffer instanceof ConcurrentBuffer) {
            ConcurrentBuffer concurrentBuffer = (ConcurrentBuffer) buffer;
            
            executor.scheduleAtFixedRate(() -> {
                System.out.println("📊 Stan bufora: " + 
                    concurrentBuffer.size() + "/" + BUFFER_SIZE + 
                    (concurrentBuffer.isEmpty() ? " [PUSTY]" : 
                     concurrentBuffer.isFull() ? " [PEŁNY]" : " [OK]"));
            }, 1, 2, TimeUnit.SECONDS);
        }
    }
    
    /**
     * Wyświetla podsumowanie działania systemu.
     * @param buffer bufor do sprawdzenia stanu końcowego
     */
    private static void printSummary(IBuffer buffer) {
        System.out.println("📈 PODSUMOWANIE:");
        System.out.println("   - Wyprodukowano łącznie: " + (PRODUCERS_COUNT * ITEMS_PER_PRODUCER) + " elementów");
        System.out.println("   - Skonsumowano łącznie: " + (CONSUMERS_COUNT * ITEMS_PER_CONSUMER) + " elementów");
        
        if (buffer instanceof ConcurrentBuffer) {
            ConcurrentBuffer concurrentBuffer = (ConcurrentBuffer) buffer;
            System.out.println("   - Pozostało w buforze: " + concurrentBuffer.size() + " elementów");
        }
        
        System.out.println("   - Egzekutory użyte: FixedThreadPool, CachedThreadPool, ScheduledThreadPool");
        System.out.println("   - Mechanizm synchronizacji: ArrayBlockingQueue");
    }
}