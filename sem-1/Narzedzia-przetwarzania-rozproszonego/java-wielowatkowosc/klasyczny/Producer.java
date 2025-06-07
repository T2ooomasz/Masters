// =============================================================================
// KLASA PRODUCENTA - Producer.java
// =============================================================================

/**
 * Producent wstawia sekwencję 100 wartości do bufora.
 * Każdy producent ma unikalny zakres wartości, żeby łatwiej śledzić dane.
 */
public class Producer extends Thread {
    
    private final String name;          // Nazwa producenta (P1, P2, P3)
    private final IBuffer buffer;       // Referencja do współdzielonego bufora
    private final int startValue;       // Pierwsza wartość w sekwencji
    private final int itemsCount;       // Liczba elementów do wyprodukowania
    private final long delayMs;         // Opóźnienie między operacjami
    
    /**
     * Konstruktor producenta.
     * @param name nazwa producenta
     * @param buffer współdzielony bufor
     * @param startValue pierwsza wartość sekwencji
     * @param itemsCount liczba elementów do wyprodukowania
     * @param delayMs opóźnienie między put() w milisekundach
     */
    public Producer(String name, IBuffer buffer, int startValue, 
                   int itemsCount, long delayMs) {
        this.name = name;
        this.buffer = buffer;
        this.startValue = startValue;
        this.itemsCount = itemsCount;
        this.delayMs = delayMs;
        
        // Ustaw nazwę wątku dla łatwiejszego debugowania
        setName("Producer-" + name);
    }
    
    /**
     * Główna pętla producenta.
     * Produkuje itemsCount kolejnych wartości począwszy od startValue.
     */
    @Override
    public void run() {
        System.out.println("🏭 Producent " + name + " rozpoczyna produkcję " + 
                         itemsCount + " elementów od " + startValue);
        
        try {
            for (int i = 0; i < itemsCount; i++) {
                int value = startValue + i;
                
                // Wstaw do bufora (może zablokować jeśli bufor pełny)
                buffer.put(value);
                
                System.out.println("🏭 " + name + " wyprodukował: " + value);
                
                // Symulacja czasu produkcji
                if (delayMs > 0) {
                    Thread.sleep(delayMs);
                }
            }
            
            System.out.println("✅ Producent " + name + " zakończył produkcję");
            
        } catch (InterruptedException e) {
            System.err.println("❌ Producent " + name + " przerwany!");
            Thread.currentThread().interrupt();
        } catch (Exception e) {
            System.err.println("❌ Błąd producenta " + name + ": " + e.getMessage());
            e.printStackTrace();
        }
    }
}