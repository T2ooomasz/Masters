// =============================================================================
// ZADANIE PRODUCENTA - ProducerTask.java
// =============================================================================

import java.util.concurrent.ThreadLocalRandom;

/**
 * Zadanie producenta implementujące Runnable.
 * Każdy producent wstawia określoną liczbę wartości do bufora.
 */
public class ProducerTask implements Runnable {
    
    private final String name;              // Nazwa producenta
    private final IBuffer buffer;           // Referencja do bufora
    private final int itemsCount;           // Liczba elementów do wyprodukowania
    private final int startValue;           // Wartość początkowa
    private final long delayMs;             // Opóźnienie między operacjami (ms)
    
    /**
     * Konstruktor zadania producenta.
     * @param name nazwa producenta
     * @param buffer bufor do zapisu
     * @param itemsCount liczba elementów do wyprodukowania
     * @param startValue wartość początkowa sekwencji
     * @param delayMs opóźnienie między operacjami w milisekundach
     */
    public ProducerTask(String name, IBuffer buffer, int itemsCount, 
                       int startValue, long delayMs) {
        this.name = name;
        this.buffer = buffer;
        this.itemsCount = itemsCount;
        this.startValue = startValue;
        this.delayMs = delayMs;
    }
    
    /**
     * Główna logika producenta.
     * Produkuje określoną liczbę elementów z opcjonalnymi opóźnieniami.
     */
    @Override
    public void run() {
        System.out.println("🏭 Producent " + name + " rozpoczyna pracę...");
        
        for (int i = 0; i < itemsCount; i++) {
            try {
                int value = startValue + i;
                buffer.put(value);
                
                System.out.println("🏭 Producent " + name + " wyprodukował: " + value);
                
                // Symulacja czasu produkcji z małą losowością
                if (delayMs > 0) {
                    long actualDelay = delayMs + ThreadLocalRandom.current().nextLong(-10, 11);
                    Thread.sleep(Math.max(1, actualDelay));
                }
                
            } catch (InterruptedException e) {
                Thread.currentThread().interrupt();
                System.err.println("❌ Producent " + name + " przerwany!");
                break;
            } catch (Exception e) {
                System.err.println("❌ Błąd producenta " + name + ": " + e.getMessage());
                break;
            }
        }
        
        System.out.println("✅ Producent " + name + " zakończył pracę.");
    }
}