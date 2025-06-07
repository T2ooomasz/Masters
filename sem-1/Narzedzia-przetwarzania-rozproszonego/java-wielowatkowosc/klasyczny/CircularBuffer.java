// =============================================================================
// IMPLEMENTACJA BUFORA CYKLICZNEGO - CircularBuffer.java
// =============================================================================

/**
 * Implementacja bufora cyklicznego z podstawowymi mechanizmami synchronizacji Java.
 * Wykorzystuje synchronized, wait(), notify() i notifyAll() z klasy Object.
 * 
 * KLUCZOWE ELEMENTY BUFORA CYKLICZNEGO:
 * - Tablica o stałym rozmiarze
 * - Indeksy put/get przesuwane cyklicznie (modulo rozmiar)
 * - Licznik elementów dla kontroli stanu pełny/pusty
 * - Synchronizacja metodami Object (monitor pattern)
 */
public class CircularBuffer implements IBuffer {
    
    // ===== ZMIENNE STANU BUFORA =====
    
    private final int[] buffer;         // Właściwy bufor danych
    private final int capacity;         // Maksymalna pojemność bufora
    private int count;                  // Aktualna liczba elementów w buforze
    private int putIndex;               // Indeks gdzie wstawić następny element
    private int getIndex;               // Indeks skąd pobrać następny element
    
    // ===== KONSTRUKTOR =====
    
    /**
     * Tworzy bufor cykliczny o określonej pojemności.
     * @param capacity maksymalna liczba elementów (musi być > 0)
     * @throws IllegalArgumentException jeśli capacity <= 0
     */
    public CircularBuffer(int capacity) {
        if (capacity <= 0) {
            throw new IllegalArgumentException("Pojemność bufora musi być większa od 0");
        }
        
        this.capacity = capacity;
        this.buffer = new int[capacity];
        this.count = 0;
        this.putIndex = 0;
        this.getIndex = 0;
        
        System.out.println("🔧 Utworzono bufor cykliczny o pojemności: " + capacity);
    }
    
    // ===== OPERACJA PUT (PRODUCENT) =====
    
    /**
     * Wstawia element do bufora.
     * 
     * MECHANIZM DZIAŁANIA:
     * 1. Zablokuj dostęp do bufora (synchronized)
     * 2. Jeśli bufor pełny -> czekaj (while + wait)
     * 3. Wstaw element na pozycję putIndex
     * 4. Przesuń putIndex cyklicznie
     * 5. Zwiększ licznik elementów
     * 6. Powiadom czekające wątki (notifyAll)
     * 
     * DLACZEGO WHILE zamiast IF z wait()?
     * - Ochrona przed spurious wakeup (przypadkowe przebudzenie)
     * - Gwarancja że warunek jest nadal spełniony po przebudzeniu
     */
    @Override
    public synchronized void put(int v) {
        // Oczekuj dopóki bufor nie będzie miał miejsca
        while (count == capacity) {
            try {
                System.out.println("🔄 Bufor pełny (" + count + "/" + capacity + 
                                 "), producent czeka...");
                wait(); // Usypia wątek i ZWALNIA monitor (kluczowe!)
            } catch (InterruptedException e) {
                // Przywróć status przerwania wątku
                Thread.currentThread().interrupt();
                System.err.println("❌ Producent przerwany podczas oczekiwania");
                return;
            }
        }
        
        // Wstaw element do bufora
        buffer[putIndex] = v;
        
        // Debugowanie: pokaż stan przed aktualizacją indeksów
        System.out.println("📥 Wstawiono " + v + " na pozycję " + putIndex + 
                         " (było " + count + " elementów)");
        
        // Przesuń indeks cyklicznie (kluczowy element bufora cyklicznego)
        putIndex = (putIndex + 1) % capacity;
        count++;
        
        // Wyświetl aktualny stan bufora
        printBufferState("PO PUT");
        
        // Powiadom WSZYSTKIE czekające wątki (producenci i konsumenci)
        // notifyAll() jest bezpieczniejsze niż notify() - budzi wszystkich
        notifyAll();
    }
    
    // ===== OPERACJA GET (KONSUMENT) =====
    
    /**
     * Pobiera element z bufora.
     * 
     * MECHANIZM DZIAŁANIA:
     * 1. Zablokuj dostęp do bufora (synchronized)
     * 2. Jeśli bufor pusty -> czekaj (while + wait)
     * 3. Pobierz element z pozycji getIndex
     * 4. Opcjonalnie wyczyść pozycję (dla debugowania)
     * 5. Przesuń getIndex cyklicznie
     * 6. Zmniejsz licznik elementów
     * 7. Powiadom czekające wątki (notifyAll)
     * 8. Zwróć pobraną wartość
     */
    @Override
    public synchronized int get() {
        // Oczekuj dopóki bufor nie będzie miał elementów
        while (count == 0) {
            try {
                System.out.println("🔄 Bufor pusty (" + count + "/" + capacity + 
                                 "), konsument czeka...");
                wait(); // Usypia wątek i ZWALNIA monitor
            } catch (InterruptedException e) {
                // Przywróć status przerwania wątku
                Thread.currentThread().interrupt();
                System.err.println("❌ Konsument przerwany podczas oczekiwania");
                return -1; // Wartość błędu
            }
        }
        
        // Pobierz element z bufora
        int value = buffer[getIndex];
        
        // Debugowanie: pokaż stan przed aktualizacją
        System.out.println("📤 Pobrano " + value + " z pozycji " + getIndex + 
                         " (było " + count + " elementów)");
        
        // Opcjonalnie wyczyść pozycję (pomaga w debugowaniu)
        buffer[getIndex] = 0;
        
        // Przesuń indeks cyklicznie
        getIndex = (getIndex + 1) % capacity;
        count--;
        
        // Wyświetl aktualny stan bufora
        printBufferState("PO GET");
        
        // Powiadom wszystkie czekające wątki
        notifyAll();
        
        return value;
    }
    
    // ===== METODY POMOCNICZE =====
    
    /**
     * Wyświetla aktualny stan bufora (do debugowania).
     * Metoda synchronized bo odczytuje zmienne stanu.
     */
    public synchronized void printBufferState(String operation) {
        StringBuilder sb = new StringBuilder();
        sb.append("📊 ").append(operation).append(": [");
        
        for (int i = 0; i < capacity; i++) {
            if (i > 0) sb.append(", ");
            
            // Oznacz pozycje put/get specjalnymi symbolami
            String prefix = "";
            if (i == putIndex && i == getIndex && count > 0) prefix = "⚡"; // Obie pozycje
            else if (i == putIndex) prefix = "→";  // Pozycja wstawiania
            else if (i == getIndex) prefix = "←";  // Pozycja pobierania
            
            sb.append(prefix).append(buffer[i]);
        }
        
        sb.append("] Count: ").append(count).append("/").append(capacity);
        System.out.println(sb.toString());
    }
    
    /**
     * Sprawdza czy bufor jest pusty (thread-safe).
     */
    public synchronized boolean isEmpty() {
        return count == 0;
    }
    
    /**
     * Sprawdza czy bufor jest pełny (thread-safe).
     */
    public synchronized boolean isFull() {
        return count == capacity;
    }
    
    /**
     * Zwraca aktualną liczbę elementów (thread-safe).
     */
    public synchronized int size() {
        return count;
    }
    
    /**
     * Zwraca pojemność bufora.
     */
    public int getCapacity() {
        return capacity;
    }
}