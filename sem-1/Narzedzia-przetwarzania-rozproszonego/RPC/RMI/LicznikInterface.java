// LicznikInterface.java - Interfejs zdalny
import java.rmi.Remote;
import java.rmi.RemoteException;

/**
 * Interfejs zdalnego licznika
 */
public interface LicznikInterface extends Remote {
    /**
     * Zwiększa wartość licznika o podaną wartość
     * @param wartosc wartość o którą zwiększyć licznik
     * @return nowa wartość licznika
     */
    int zwieksz(int wartosc) throws RemoteException;
    
    /**
     * Zmniejsza wartość licznika o podaną wartość
     * @param wartosc wartość o którą zmniejszyć licznik
     * @return nowa wartość licznika
     */
    int zmniejsz(int wartosc) throws RemoteException;
    
    /**
     * Pobiera aktualną wartość licznika
     * @return aktualna wartość licznika
     */
    int pobierzWartosc() throws RemoteException;
    
    /**
     * Resetuje licznik do zera
     * @return nowa wartość licznika (0)
     */
    int reset() throws RemoteException;
}