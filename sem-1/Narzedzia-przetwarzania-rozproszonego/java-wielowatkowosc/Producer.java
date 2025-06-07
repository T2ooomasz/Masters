// =============================================================================
// PRODUCENT - Producer.java
// =============================================================================
import java.rmi.registry.LocateRegistry;
import java.rmi.registry.Registry;

/**
 * Klasa Producer reprezentuje wątek producenta, który wstawia dane do bufora.
 * Każdy producent wstawia 100 wartości z zakresu 101-200.
 */
public class Producer extends Thread {
    private final String name;          // Nazwa producenta (dla identyfikacji)
    private final IBuffer buffer;       // Referencja do zdalnego bufora
    private final int startValue;       // Wartość początkowa dla tego producenta
    
    /**
     * Konstruktor producenta.
     * @param name nazwa producenta
     * @param buffer referencja do zdalnego bufora
     * @param startValue wartość początkowa dla sekwencji
     */
    public Producer(String name, IBuffer buffer, int startValue) {
        this.name = name;
        this.buffer = buffer;
        this.startValue = startValue;
    }
    
    /**
     * Główna logika producenta. Wstawia 100 kolejnych wartości do bufora.
     */
    @Override
    public void run() {
        System.out.println("Producent " + name + " rozpoczyna pracę...");
        
        for (int i = 0; i < 100; i++) {
            try {
                int value = startValue + i;
                buffer.put(value);
                System.out.println("Producent " + name + " -> wstawił: " + value);
                
                // Krótka pauza dla lepszej obserwacji działania
                Thread.sleep(10);
                
            } catch (Exception e) {
                System.err.println("Błąd producenta " + name + ": " + e.getMessage());
                e.printStackTrace();
                break;
            }
        }
        
        System.out.println("Producent " + name + " zakończył pracę.");
    }
    
    /**
     * Metoda główna klienta producentów.
     */
    public static void main(String[] args) {
        String host = (args.length < 1) ? null : args[0];
        
        try {
            // Połącz się z rejestrem RMI
            Registry registry = LocateRegistry.getRegistry(host);
            IBuffer stub = (IBuffer) registry.lookup("IBuffer");
            
            System.out.println("Połączono z buforem zdalnym.");
            
            // Utwórz 3 producentów z różnymi zakresami wartości
            Producer p1 = new Producer("P1", stub, 101);  // 101-200
            Producer p2 = new Producer("P2", stub, 201);  // 201-300  
            Producer p3 = new Producer("P3", stub, 301);  // 301-400
            
            // Uruchom wszystkich producentów
            p1.start();
            p2.start();
            p3.start();
            
            // Czekaj na zakończenie wszystkich producentów
            p1.join();
            p2.join();
            p3.join();
            
            System.out.println("Wszyscy producenci zakończyli pracę.");
            
        } catch (Exception e) {
            System.err.println("Błąd klienta producentów: " + e.toString());
            e.printStackTrace();
        }
    }
}
