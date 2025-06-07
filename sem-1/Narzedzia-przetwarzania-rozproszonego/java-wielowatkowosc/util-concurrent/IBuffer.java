// =============================================================================
// INTERFEJS BUFORA - IBuffer.java
// =============================================================================

/**
 * Interfejs definiujący operacje na buforze dla Producer-Consumer pattern.
 * W przeciwieństwie do wersji RMI, metody nie rzucają RemoteException.
 */
public interface IBuffer {
    /**
     * Wstawia wartość do bufora. Blokuje jeśli bufor jest pełny.
     * @param v wartość do wstawienia
     */
    void put(int v);
    
    /**
     * Pobiera wartość z bufora. Blokuje jeśli bufor jest pusty.
     * @return pobrana wartość
     */
    int get();
}