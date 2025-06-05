// LicznikSerwer.java - Serwer
import java.rmi.RemoteException;
import java.rmi.registry.LocateRegistry;
import java.rmi.registry.Registry;
import java.rmi.server.UnicastRemoteObject;
import java.util.concurrent.atomic.AtomicInteger;

/**
 * Implementacja serwera zdalnego licznika
 */
public class LicznikSerwer implements LicznikInterface {
    // Używamy AtomicInteger dla thread-safety
    private final AtomicInteger wartoscLicznika = new AtomicInteger(0);
    
    public LicznikSerwer() throws RemoteException {
        super();
    }
    
    @Override
    public int zwieksz(int wartosc) throws RemoteException {
        System.out.printf("[SERWER] Zwiekszanie licznika o: %d%n", wartosc);
        int nowaWartosc = wartoscLicznika.addAndGet(wartosc);
        System.out.printf("[SERWER] Nowa wartosc: %d%n", nowaWartosc);
        return nowaWartosc;
    }
    
    @Override
    public int zmniejsz(int wartosc) throws RemoteException {
        System.out.printf("[SERWER] Zmniejszanie licznika o: %d%n", wartosc);
        int nowaWartosc = wartoscLicznika.addAndGet(-wartosc);
        System.out.printf("[SERWER] Nowa wartosc: %d%n", nowaWartosc);
        return nowaWartosc;
    }
    
    @Override
    public int pobierzWartosc() throws RemoteException {
        int wartosc = wartoscLicznika.get();
        System.out.printf("[SERWER] Pobieranie wartosci: %d%n", wartosc);
        return wartosc;
    }
    
    @Override
    public int reset() throws RemoteException {
        System.out.println("[SERWER] Resetowanie licznika do 0");
        wartoscLicznika.set(0);
        return 0;
    }
    
    public static void main(String[] args) {
        try {
            
            // Tworzenie rejestru RMI
            int port = 1099;
            if (args.length > 0) {
                try {
                    port = Integer.parseInt(args[0]);
                } catch (NumberFormatException e) {
                    System.err.println("Niepoprawny numer portu. Uzywam domyslnego: 1099");
                    port = 1099;
                }
            }
            
            Registry registry = LocateRegistry.createRegistry(port);
            
            // Tworzenie instancji serwera
            LicznikSerwer serwer = new LicznikSerwer();
            
            // Eksportowanie obiektu
            LicznikInterface stub = (LicznikInterface) UnicastRemoteObject.exportObject(serwer, 0);
            
            // Rejestracja obiektu w rejestrze
            registry.rebind("Licznik", stub);
            
            System.out.println("=== Serwer RMI - Zdalny Licznik ===");
            System.out.printf("Serwer nasłuchuje na porcie %d%n", port);
            System.out.println("Obiekt zarejestrowany jako: 'Licznik'");
            System.out.println("Nacisnij Ctrl+C aby zatrzymać serwer...");
            
        } catch (Exception e) {
            System.err.println("Blad serwera: " + e.getMessage());
            e.printStackTrace();
        }
    }
}
