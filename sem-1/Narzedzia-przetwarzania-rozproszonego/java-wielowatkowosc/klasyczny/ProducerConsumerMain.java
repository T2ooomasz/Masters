// =============================================================================
// GŁÓWNA APLIKACJA - ProducerConsumerMain.java
// =============================================================================

/**
 * Główna klasa uruchamiająca system Producer-Consumer zgodnie ze specyfikacją:
 * - 3 producentów, każdy produkuje 100 elementów
 * - 3 konsumentów, każdy konsumuje 100 elementów  
 * - Bufor cykliczny o pojemności 10 elementów
 * - Synchronizacja przez wait/notify/synchronized
 */
public class ProducerConsumerMain {
    
    // Konfiguracja systemu zgodna ze specyfikacją zadania
    private static final int BUFFER_CAPACITY = 10;      // Pojemność bufora
    private static final int PRODUCERS_COUNT = 3;        // Liczba producentów
    private static final int CONSUMERS_COUNT = 3;        // Liczba konsumentów
    private static final int ITEMS_PER_PRODUCER = 100;   // Elementy na producenta
    private static final int ITEMS_PER_CONSUMER = 100;   // Elementy na konsumenta
    private static final int PRODUCER_START_VALUE = 101; // Pierwsza wartość (101-200)
    
    // Opóźnienia dla lepszej observability
    private static final long PRODUCER_DELAY_MS = 50;
    private static final long CONSUMER_DELAY_MS = 75;
    
    public static void main(String[] args) {
        System.out.println("🚀 URUCHAMIANIE SYSTEMU PRODUCER-CONSUMER");
        System.out.println("=" .repeat(60));
        System.out.println("📋 SPECYFIKACJA:");
        System.out.println("   • Producenci: " + PRODUCERS_COUNT + 
                         " (każdy produkuje " + ITEMS_PER_PRODUCER + " elementów)");
        System.out.println("   • Konsumenci: " + CONSUMERS_COUNT + 
                         " (każdy konsumuje " + ITEMS_PER_CONSUMER + " elementów)");
        System.out.println("   • Bufor cykliczny: " + BUFFER_CAPACITY + " elementów");
        System.out.println("   • Zakres wartości: " + PRODUCER_START_VALUE + "-" + 
                         (PRODUCER_START_VALUE + ITEMS_PER_PRODUCER - 1));
        System.out.println("   • Synchronizacja: synchronized + wait/notify");
        System.out.println("=" .repeat(60));
        System.out.println();
        
        try {
            // Tworzenie współdzielonego bufora cyklicznego
            IBuffer sharedBuffer = new CircularBuffer(BUFFER_CAPACITY);
            
            // Wyświetl początkowy stan bufora
            if (sharedBuffer instanceof CircularBuffer) {
                ((CircularBuffer) sharedBuffer).printBufferState("POCZĄTKOWY");
            }
            
            System.out.println();
            
            // ===== TWORZENIE I URUCHAMIANIE PRODUCENTÓW =====
            
            System.out.println("🏭 Tworzenie producentów...");
            Thread[] producers = new Thread[PRODUCERS_COUNT];
            
            for (int i = 0; i < PRODUCERS_COUNT; i++) {
                String name = "P" + (i + 1);
                
                // Każdy producent ma te same wartości (101-200)
                // Zgodnie ze specyfikacją: "począwszy od 101"
                producers[i] = new Producer(
                    name, 
                    sharedBuffer, 
                    PRODUCER_START_VALUE,           // Wszyscy od 101
                    ITEMS_PER_PRODUCER,             // 100 elementów każdy
                    PRODUCER_DELAY_MS
                );
                
                producers[i].start();
                System.out.println("   ✓ Uruchomiono producenta " + name);
            }
            
            // Małe opóźnienie przed uruchomieniem konsumentów
            Thread.sleep(100);
            
            // ===== TWORZENIE I URUCHAMIANIE KONSUMENTÓW =====
            
            System.out.println("\n🛒 Tworzenie konsumentów...");
            Thread[] consumers = new Thread[CONSUMERS_COUNT];
            
            for (int i = 0; i < CONSUMERS_COUNT; i++) {
                String name = "C" + (i + 1);
                
                consumers[i] = new Consumer(
                    name,
                    sharedBuffer,
                    ITEMS_PER_CONSUMER,             // 100 elementów każdy
                    CONSUMER_DELAY_MS
                );
                
                consumers[i].start();
                System.out.println("   ✓ Uruchomiono konsumenta " + name);
            }
            
            System.out.println("\n🔄 Wszystkie wątki uruchomione. Oczekiwanie na zakończenie...\n");
            
            // ===== OCZEKIWANIE NA ZAKOŃCZENIE WSZYSTKICH WĄTKÓW =====
            
            // Czekaj na producentów
            System.out.println("⏳ Oczekiwanie na producentów...");
            for (int i = 0; i < PRODUCERS_COUNT; i++) {
                producers[i].join();
                System.out.println("   ✓ Producent P" + (i + 1) + " zakończony");
            }
            
            // Czekaj na konsumentów  
            System.out.println("\n⏳ Oczekiwanie na konsumentów...");
            for (int i = 0; i < CONSUMERS_COUNT; i++) {
                consumers[i].join();
                System.out.println("   ✓ Konsument C" + (i + 1) + " zakończony");
            }
            
            // ===== PODSUMOWANIE =====
            
            System.out.println("\n" + "=" .repeat(60));
            System.out.println("🎉 SYSTEM ZAKOŃCZYŁ DZIAŁANIE POMYŚLNIE!");
            System.out.println("=" .repeat(60));
            
            // Wyświetl końcowy stan bufora
            if (sharedBuffer instanceof CircularBuffer) {
                CircularBuffer cb = (CircularBuffer) sharedBuffer;
                cb.printBufferState("KOŃCOWY");
                
                System.out.println("\n📊 STATYSTYKI KOŃCOWE:");
                System.out.println("   • Wyprodukowano łącznie: " + 
                                 (PRODUCERS_COUNT * ITEMS_PER_PRODUCER) + " elementów");
                System.out.println("   • Skonsumowano łącznie: " + 
                                 (CONSUMERS_COUNT * ITEMS_PER_CONSUMER) + " elementów");
                System.out.println("   • Pozostało w buforze: " + cb.size() + " elementów");
                System.out.println("   • Status bufora: " + 
                                 (cb.isEmpty() ? "PUSTY" : 
                                  cb.isFull() ? "PEŁNY" : "CZĘŚCIOWO ZAPEŁNIONY"));
            }
            
            // Analiza balansu produkcja vs konsumpcja
            int totalProduced = PRODUCERS_COUNT * ITEMS_PER_PRODUCER;
            int totalConsumed = CONSUMERS_COUNT * ITEMS_PER_CONSUMER;
            int difference = totalProduced - totalConsumed;
            
            System.out.println("\n📈 ANALIZA BALANSU:");
            if (difference == 0) {
                System.out.println("   ✅ Idealna równowaga: produkcja = konsumpcja");
            } else if (difference > 0) {
                System.out.println("   📈 Nadwyżka produkcji: +" + difference + " elementów");
            } else {
                System.out.println("   📉 Niedobór produkcji: " + difference + " elementów");
            }
            
        } catch (InterruptedException e) {
            System.err.println("\n❌ Główny wątek przerwany: " + e.getMessage());
            Thread.currentThread().interrupt();
        } catch (Exception e) {
            System.err.println("\n❌ Nieoczekiwany błąd: " + e.getMessage());
            e.printStackTrace();
        }
        
        System.out.println("\n👋 Program zakończony.");
    }
}