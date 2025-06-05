// LicznikKlient.java - Klient
// -----------------------------------------------------------------------

// Instrukcje kompilacji i uruchomienia:

/*
=== KOMPILACJA ===
javac *.java

=== URUCHOMIENIE ===

1. Uruchomienie serwera:
   java LicznikSerwer [port]
   
   Przyklady:
   java LicznikSerwer           # domyslny port 1099
   java LicznikSerwer 1234      # port 1234

2. Uruchomienie klienta:
   java LicznikKlient [host] [port]
   
   Przyklady:
   java LicznikKlient                    # localhost:1099
   java LicznikKlient 192.168.1.100     # 192.168.1.100:1099
   java LicznikKlient localhost 1234    # localhost:1234

=== KOMENDY KLIENTA ===
+ 5         - zwieksz licznik o 5
- 3         - zmniejsz licznik o 3
get         - pobierz aktualna wartosc
reset       - zresetuj do 0
help        - pomoc
quit        - zakoncz
*/
import java.rmi.registry.LocateRegistry;
import java.rmi.registry.Registry;
import java.util.Scanner;

/**
 * Klient zdalnego licznika z interaktywnym interfejsem
 */
public class LicznikKlient {
    private static final String HELP_MESSAGE = """
        Dostepne komendy:
        + <liczba>  - zwieksz licznik o podana wartosc
        - <liczba>  - zmniejsz licznik o podana wartosc
        get         - pobierz aktualna wartosc licznika
        reset       - zresetuj licznik do 0
        help        - wyswietl te pomoc
        quit        - zakoncz program
        
        Przyklady:
        + 5         - zwieksz o 5
        - 3         - zmniejsz o 3
        """;
    
    public static void main(String[] args) {
        String host = "localhost";
        int port = 1099;
        
        // Parsowanie argumentow
        if (args.length > 0) {
            host = args[0];
        }
        if (args.length > 1) {
            try {
                port = Integer.parseInt(args[1]);
            } catch (NumberFormatException e) {
                System.err.println("Niepoprawny numer portu. Uzywam domyslnego: 1099");
                port = 1099;
            }
        }
        
        try {
            // Lokalizacja rejestru RMI
            Registry registry = LocateRegistry.getRegistry(host, port);
            
            // Pobranie referencji do zdalnego obiektu
            LicznikInterface licznik = (LicznikInterface) registry.lookup("Licznik");
            
            System.out.println("=== Klient RMI - Zdalny Licznik ===");
            System.out.printf("Polaczono z serwerem: %s:%d%n", host, port);
            System.out.printf("Aktualna wartosc licznika: %d%n", licznik.pobierzWartosc());
            System.out.println("\nWpisz 'help' aby zobaczyc dostepne komendy");
            
            Scanner scanner = new Scanner(System.in);
            String linia;
            
            while (true) {
                System.out.print("> ");
                linia = scanner.nextLine().trim();
                
                if (linia.isEmpty()) {
                    continue;
                }
                
                try {
                    if (linia.equalsIgnoreCase("quit") || linia.equalsIgnoreCase("exit")) {
                        System.out.println("Zakonczenie programu...");
                        break;
                    } else if (linia.equalsIgnoreCase("help")) {
                        System.out.println(HELP_MESSAGE);
                    } else if (linia.equalsIgnoreCase("get")) {
                        int wartosc = licznik.pobierzWartosc();
                        System.out.printf("Aktualna wartosc licznika: %d%n", wartosc);
                    } else if (linia.equalsIgnoreCase("reset")) {
                        int wartosc = licznik.reset();
                        System.out.printf("Licznik zresetowany. Aktualna wartosc: %d%n", wartosc);
                    } else if (linia.startsWith("+") || linia.startsWith("-")) {
                        parsujIWykonajOperacje(linia, licznik);
                    } else {
                        System.err.println("Niepoprawna komenda. Wpisz 'help' aby zobaczyc dostepne komendy.");
                    }
                } catch (Exception e) {
                    System.err.printf("Blad wykonania operacji: %s%n", e.getMessage());
                }
            }
            
            scanner.close();
            
        } catch (Exception e) {
            System.err.printf("Blad klienta: %s%n", e.getMessage());
            e.printStackTrace();
        }
    }
    
    /**
     * Parsuje i wykonuje operacje zwiekszania/zmniejszania
     */
    private static void parsujIWykonajOperacje(String linia, LicznikInterface licznik) {
        try {
            String[] czesci = linia.split("\\s+");
            if (czesci.length != 2) {
                System.err.println("Niepoprawny format. Uzyj: + <liczba> lub - <liczba>");
                return;
            }
            
            String operator = czesci[0];
            int wartosc;
            
            try {
                wartosc = Integer.parseInt(czesci[1]);
            } catch (NumberFormatException e) {
                System.err.println("Niepoprawna liczba: " + czesci[1]);
                return;
            }
            
            int wynik;
            if (operator.equals("+")) {
                wynik = licznik.zwieksz(wartosc);
                System.out.printf("Zwiekszono o %d. Nowa wartosc: %d%n", wartosc, wynik);
            } else if (operator.equals("-")) {
                wynik = licznik.zmniejsz(wartosc);
                System.out.printf("Zmniejszono o %d. Nowa wartosc: %d%n", wartosc, wynik);
            } else {
                System.err.println("Niepoprawny operator: " + operator);
            }
            
        } catch (Exception e) {
            System.err.printf("Blad wykonania operacji: %s%n", e.getMessage());
        }
    }
}