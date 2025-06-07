// =============================================================================
// INTERFEJS ZDALNY - IBuffer.java
// =============================================================================
import java.rmi.Remote;
import java.rmi.RemoteException;

/**
 * Interfejs zdalny definiujący operacje na buforze cyklicznym.
 * Dziedziczy po Remote, co pozwala na zdalne wywołania metod.
 */
public interface IBuffer extends Remote {
    /**
     * Wstawia wartość do bufora. Jeśli bufor jest pełny, wątek czeka.
     * @param v wartość do wstawienia
     * @throws RemoteException w przypadku problemów sieciowych
     * @throws InterruptedException gdy wątek zostanie przerwany podczas oczekiwania
     */
    void put(int v) throws RemoteException, InterruptedException;
    
    /**
     * Pobiera wartość z bufora. Jeśli bufor jest pusty, wątek czeka.
     * @return pobrana wartość
     * @throws RemoteException w przypadku problemów sieciowych
     * @throws InterruptedException gdy wątek zostanie przerwany podczas oczekiwania
     */
    int get() throws RemoteException, InterruptedException;
}

