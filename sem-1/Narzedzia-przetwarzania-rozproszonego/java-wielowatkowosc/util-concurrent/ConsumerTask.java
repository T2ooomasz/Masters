// =============================================================================
// ZADANIE KONSUMENTA - ConsumerTask.java
// =============================================================================

import java.util.concurrent.ThreadLocalRandom;

/**
 * Zadanie konsumenta implementujące Runnable.
 * Każdy konsument pobiera określoną liczbę wartości z bufora.
 */
public class ConsumerTask implements Runnable {
    
    private final String name;              // Nazwa konsumenta
    private final IBuffer buffer;           // Referencja do bufora
    private final int itemsCount;           // Liczba elementów do skonsumowania
    private final long delayMs;             // Opóźnienie między operacjami (ms)
    
    /**
     * Konstruktor zadania konsumenta.
     * @param name nazwa konsumenta
     * @param buffer bufor do odczytu
     * @param itemsCount liczba elementów do skonsumowania
     * @param delayMs opóźnienie między operacjami w milisekundach
     */
    public ConsumerTask(String name, IBuffer buffer, int itemsCount, long delayMs) {
        this.name = name;
        this.buffer = buffer;
        this.itemsCount = itemsCount;
        this.delayMs = delayMs;
    }
    
    /**
     * Główna logika konsumenta.
     * Konsumuje określoną liczbę elementów z opcjonalnymi opóźnieniami.
     */
    @Override
    public void run() {
        System.out.println("🛒 Konsument " + name + " rozpoczyna pracę...");
        
        for (int i = 0; i < itemsCount; i++) {
            try {
                int value = buffer.get();
                
                System.out.println("🛒 Konsument " + name + " skonsumował: " + value);
                
                // Symulacja czasu konsumpcji z małą losowością
                if (delayMs > 0) {
                    long actualDelay = delayMs + ThreadLocalRandom.current().nextLong(-5, 6);
                    Thread.sleep(Math.max(1, actualDelay));
                }
                
            } catch (InterruptedException e) {
                Thread.currentThread().interrupt();
                System.err.println("❌ Konsument " + name + " przerwany!");
                break;
            } catch (Exception e) {
                System.err.println("❌ Błąd konsumenta " + name + ": " + e.getMessage());
                break;
            }
        }
        
        System.out.println("✅ Konsument " + name + " zakończył pracę.");
    }
}