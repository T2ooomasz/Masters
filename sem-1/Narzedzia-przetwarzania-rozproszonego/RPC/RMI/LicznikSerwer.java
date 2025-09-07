// LicznikSerwer.java - Serwer
import java.rmi.Naming;
import java.rmi.RemoteException;
import java.rmi.registry.LocateRegistry;
import java.rmi.registry.Registry;
import java.rmi.server.UnicastRemoteObject;
import java.util.concurrent.atomic.AtomicInteger;

/**
 * Implementacja serwera zdalnego licznika
 */
public class LicznikSerwer {
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
            LicznikImpl licznik = new LicznikImpl();
            
            // Eksportowanie obiektu
            // LicznikInterface stub = (LicznikInterface) UnicastRemoteObject.exportObject(serwer, 0);
            
            // Rejestracja obiektu w rejestrze
            registry.rebind("Licznik", licznik);
            
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
