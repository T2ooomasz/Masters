// =============================================================================
// KLASA KONSUMENTA - Consumer.java
// =============================================================================

/**
 * Konsument pobiera określoną liczbę wartości z bufora i wyświetla je.
 */
public class Consumer extends Thread {
    
    private final String name;          // Nazwa konsumenta (C1, C2, C3)
    private final IBuffer buffer;       // Referencja do współdzielonego bufora
    private final int itemsCount;       // Liczba elementów do skonsumowania
    private final long delayMs;         // Opóźnienie między operacjami
    
    /**
     * Konstruktor konsumenta.
     * @param name nazwa konsumenta
     * @param buffer współdzielony bufor
     * @param itemsCount liczba elementów do skonsumowania
     * @param delayMs opóźnienie między get() w milisekundach
     */
    public Consumer(String name, IBuffer buffer, int itemsCount, long delayMs) {
        this.name = name;
        this.buffer = buffer;
        this.itemsCount = itemsCount;
        this.delayMs = delayMs;
        
        // Ustaw nazwę wątku dla łatwiejszego debugowania
        setName("Consumer-" + name);
    }
    
    /**
     * Główna pętla konsumenta.
     * Konsumuje itemsCount elementów z bufora.
     */
    @Override
    public void run() {
        System.out.println("🛒 Konsument " + name + " rozpoczyna konsumpcję " + 
                         itemsCount + " elementów");
        
        try {
            for (int i = 0; i < itemsCount; i++) {
                // Pobierz z bufora (może zablokować jeśli bufor pusty)
                int value = buffer.get();
                
                System.out.println("🛒 " + name + " skonsumował: " + value);
                
                // Symulacja czasu konsumpcji
                if (delayMs > 0) {
                    Thread.sleep(delayMs);
                }
            }
            
            System.out.println("✅ Konsument " + name + " zakończył konsumpcję");
            
        } catch (InterruptedException e) {
            System.err.println("❌ Konsument " + name + " przerwany!");
            Thread.currentThread().interrupt();
        } catch (Exception e) {
            System.err.println("❌ Błąd konsumenta " + name + ": " + e.getMessage());
            e.printStackTrace();
        }
    }
}