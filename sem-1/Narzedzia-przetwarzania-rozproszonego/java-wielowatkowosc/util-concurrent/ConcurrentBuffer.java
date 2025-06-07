// =============================================================================
// IMPLEMENTACJA BUFORA - ConcurrentBuffer.java
// =============================================================================

import java.util.concurrent.ArrayBlockingQueue;
import java.util.concurrent.BlockingQueue;

/**
 * Implementacja bufora wykorzystująca BlockingQueue z java.util.concurrent.
 * ArrayBlockingQueue automatycznie obsługuje synchronizację i blokowanie wątków.
 */
public class ConcurrentBuffer implements IBuffer {
    
    // BlockingQueue automatycznie obsługuje synchronizację i blokowanie
    private final BlockingQueue<Integer> buffer;
    private final int capacity;
    
    /**
     * Konstruktor bufora o określonej pojemności.
     * @param capacity maksymalna liczba elementów w buforze
     */
    public ConcurrentBuffer(int capacity) {
        this.capacity = capacity;
        // ArrayBlockingQueue to thread-safe implementacja ograniczonej kolejki
        this.buffer = new ArrayBlockingQueue<>(capacity);
    }
    
    /**
     * Wstawia wartość do bufora.
     * Automatycznie blokuje wątek jeśli bufor jest pełny.
     */
    @Override
    public void put(int v) {
        try {
            // put() blokuje dopóki nie będzie miejsca w kolejce
            buffer.put(v);
            System.out.println("Wstawiono: " + v + 
                             " (rozmiar bufora: " + buffer.size() + "/" + capacity + ")");
        } catch (InterruptedException e) {
            // Przywróć status przerwania wątku
            Thread.currentThread().interrupt();
            System.err.println("Producent przerwany podczas wstawiania: " + e.getMessage());
        }
    }
    
    /**
     * Pobiera wartość z bufora.
     * Automatycznie blokuje wątek jeśli bufor jest pusty.
     */
    @Override
    public int get() {
        try {
            // take() blokuje dopóki kolejka nie będzie miała elementów
            int value = buffer.take();
            System.out.println("Pobrano: " + value + 
                             " (rozmiar bufora: " + buffer.size() + "/" + capacity + ")");
            return value;
        } catch (InterruptedException e) {
            // Przywróć status przerwania wątku
            Thread.currentThread().interrupt();
            System.err.println("Konsument przerwany podczas pobierania: " + e.getMessage());
            return -1; // Wartość domyślna w przypadku przerwania
        }
    }
    
    /**
     * Zwraca aktualny rozmiar bufora.
     * @return liczba elementów w buforze
     */
    public int size() {
        return buffer.size();
    }
    
    /**
     * Sprawdza czy bufor jest pusty.
     * @return true jeśli bufor jest pusty
     */
    public boolean isEmpty() {
        return buffer.isEmpty();
    }
    
    /**
     * Sprawdza czy bufor jest pełny.
     * @return true jeśli bufor jest pełny
     */
    public boolean isFull() {
        return buffer.size() == capacity;
    }
}