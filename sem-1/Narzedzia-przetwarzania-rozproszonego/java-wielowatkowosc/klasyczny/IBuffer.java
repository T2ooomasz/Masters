// =============================================================================
// INTERFEJS BUFORA - IBuffer.java
// =============================================================================

/**
 * Interfejs definiujący operacje na buforze cyklicznym.
 * Prostszy od wersji RMI - metody nie rzucają wyjątków zdalnych.
 */
public interface IBuffer {
    /**
     * Wstawia wartość do bufora. Jeśli bufor jest pełny, wątek oczekuje.
     * @param v wartość do wstawienia
     */
    void put(int v);
    
    /**
     * Pobiera wartość z bufora. Jeśli bufor jest pusty, wątek oczekuje.
     * @return pobrana wartość
     */
    int get();
}