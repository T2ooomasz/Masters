// =============================================================================
// IMPLEMENTACJA BUFORA - Buffer.java
// =============================================================================
import java.rmi.RemoteException;
import java.rmi.registry.LocateRegistry;
import java.rmi.registry.Registry;
import java.rmi.server.UnicastRemoteObject;

/**
 * Implementacja bufora cyklicznego o ograniczonej pojemności (10 elementów).
 * Wykorzystuje mechanizmy synchronizacji Java: synchronized, wait(), notifyAll().
 */
public class Buffer implements IBuffer {
    private final int[] buffer = new int[10];  // Bufor o pojemności 10 elementów
    private int count = 0;      // Liczba elementów aktualnie w buforze
    private int putIndex = 0;   // Indeks gdzie wstawić następny element
    private int getIndex = 0;   // Indeks skąd pobrać następny element
    
    /**
     * Wstawia wartość do bufora. Implementuje wzorzec Producer.
     * Metoda synchronized zapewnia wzajemne wykluczanie dostępu do bufora.
     */
    @Override
    public synchronized void put(int v) throws RemoteException, InterruptedException {
        // Oczekuj dopóki bufor nie będzie miał miejsca
        while (count == buffer.length) {
            System.out.println("Bufor pełny, producent czeka...");
            wait(); // Usypia wątek i zwalnia monitor
        }
        
        // Wstaw element do bufora
        buffer[putIndex] = v;
        putIndex = (putIndex + 1) % buffer.length; // Cykliczne przesunięcie indeksu
        count++; // Zwiększ licznik elementów
        
        System.out.println("Wstawiono: " + v + " (elementów w buforze: " + count + ")");
        
        // Powiadom wszystkie czekające wątki konsumentów
        notifyAll();
    }
    
    /**
     * Pobiera wartość z bufora. Implementuje wzorzec Consumer.
     * Metoda synchronized zapewnia wzajemne wykluczanie dostępu do bufora.
     */
    @Override
    public synchronized int get() throws RemoteException, InterruptedException {
        // Oczekuj dopóki bufor nie będzie miał elementów
        while (count == 0) {
            System.out.println("Bufor pusty, konsument czeka...");
            wait(); // Usypia wątek i zwalnia monitor
        }
        
        // Pobierz element z bufora
        int value = buffer[getIndex];
        buffer[getIndex] = 0; // Opcjonalne: wyczyść pozycję
        getIndex = (getIndex + 1) % buffer.length; // Cykliczne przesunięcie indeksu
        count--; // Zmniejsz licznik elementów
        
        System.out.println("Pobrano: " + value + " (elementów w buforze: " + count + ")");
        
        // Powiadom wszystkie czekające wątki producentów
        notifyAll();
        
        return value;
    }
    
    /**
     * Metoda główna serwera RMI. Tworzy instancję bufora i rejestruje ją w RMI Registry.
     */
    public static void main(String args[]) {
        try {
            // Utwórz instancję bufora
            Buffer obj = new Buffer();
            
            // Wyeksportuj obiekt jako zdalny stub
            IBuffer stub = (IBuffer) UnicastRemoteObject.exportObject(obj, 0);
            
            // Pobierz lub utwórz rejestr RMI na porcie 1099
            Registry registry;
            try {
                registry = LocateRegistry.getRegistry();
            } catch (Exception e) {
                // Jeśli rejestr nie istnieje, utwórz go
                registry = LocateRegistry.createRegistry(1099);
            }
            
            // Zarejestruj stub w rejestrze pod nazwą "IBuffer"
            registry.rebind("IBuffer", stub);
            
            System.out.println("Serwer bufora gotowy do działania!");
            System.out.println("Bufor może przechowywać maksymalnie 10 elementów");
            
        } catch (Exception e) {
            System.err.println("Błąd serwera: " + e.toString());
            e.printStackTrace();
        }
    }
}
