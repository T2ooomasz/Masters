// =============================================================================
// KONSUMENT - Consumer.java
// =============================================================================
import java.rmi.registry.LocateRegistry;
import java.rmi.registry.Registry;

/**
 * Klasa Consumer reprezentuje wątek konsumenta, który pobiera dane z bufora.
 * Każdy konsument pobiera 100 wartości i wyświetla je.
 */
public class Consumer extends Thread {
    private final String name;      // Nazwa konsumenta (dla identyfikacji)
    private final IBuffer buffer;   // Referencja do zdalnego bufora
    
    /**
     * Konstruktor konsumenta.
     * @param name nazwa konsumenta
     * @param buffer referencja do zdalnego bufora
     */
    public Consumer(String name, IBuffer buffer) {
        this.name = name;
        this.buffer = buffer;
    }
    
    /**
     * Główna logika konsumenta. Pobiera 100 wartości z bufora.
     */
    @Override
    public void run() {
        System.out.println("Konsument " + name + " rozpoczyna pracę...");
        
        for (int i = 0; i < 100; i++) {
            try {
                int value = buffer.get();
                System.out.println("Konsument " + name + " <- pobrał: " + value);
                
                // Krótka pauza dla lepszej obserwacji działania
                Thread.sleep(15);
                
            } catch (Exception e) {
                System.err.println("Błąd konsumenta " + name + ": " + e.getMessage());
                e.printStackTrace();
                break;
            }
        }
        
        System.out.println("Konsument " + name + " zakończył pracę.");
    }
    
    /**
     * Metoda główna klienta konsumentów.
     */
    public static void main(String[] args) {
        String host = (args.length < 1) ? null : args[0];
        
        try {
            // Połącz się z rejestrem RMI
            Registry registry = LocateRegistry.getRegistry(host);
            IBuffer stub = (IBuffer) registry.lookup("IBuffer");
            
            System.out.println("Połączono z buforem zdalnym.");
            
            // Utwórz 3 konsumentów
            Consumer c1 = new Consumer("C1", stub);
            Consumer c2 = new Consumer("C2", stub);
            Consumer c3 = new Consumer("C3", stub);
            
            // Uruchom wszystkich konsumentów
            c1.start();
            c2.start();
            c3.start();
            
            // Czekaj na zakończenie wszystkich konsumentów
            c1.join();
            c2.join();
            c3.join();
            
            System.out.println("Wszyscy konsumenci zakończyli pracę.");
            
        } catch (Exception e) {
            System.err.println("Błąd klienta konsumentów: " + e.toString());
            e.printStackTrace();
        }
    }
}
