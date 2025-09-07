// LicznikIml.java
import java.rmi.RemoteException;
import java.rmi.server.UnicastRemoteObject;

/**
 * Implementacja serwera zdalnego licznika
 */
public class LicznikImpl extends UnicastRemoteObject implements LicznikInterface {
    private int licznik; // Jeszcze lepiej użyc AtomicInteger

    // Konstruktor musi rzucać RemoteException
    public LicznikImpl() throws RemoteException {
        super();
        this.licznik = 0;
        System.out.println("Inicjalizacja licznika:" + this.licznik);
    }

    @Override
    public synchronized int zwieksz(int wartosc) throws RemoteException {
        this.licznik += wartosc;
        System.out.println("Zwiekszono licznik o " + wartosc + ". Aktualna wartosc: " + this.licznik);
        return this.licznik;
    }

    @Override
    public synchronized int zmniejsz(int wartosc) throws RemoteException {
        this.licznik -= wartosc;
        System.out.println("Zmniejszono licznik o " + wartosc + ". Aktualna wartosc: " + this.licznik);
        return this.licznik;
    }

    @Override
    public synchronized int pobierzWartosc() throws RemoteException {
        System.out.println("Pobrano wartosc licznika: " + this.licznik);
        return this.licznik;
    }

    @Override
    public synchronized int reset() throws RemoteException {
        this.licznik = 0;
        System.out.println("Licznik zostal zresetowany. Aktualna wartosc: " + this.licznik);
        return this.licznik;
    }
    
}
