#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <pthread.h>
#include <semaphore.h>

#define MAX 10
#define LMAX 5  // Rozmiar lokalnego bufora

// Bufor cykliczny
volatile int buf[MAX];
volatile int in = 0;  // Indeks do wstawiania danych
volatile int out = 0; // Indeks do pobierania danych
volatile int count = 0; // Liczba elementów w buforze

// Semafory do synchronizacji
sem_t empty;    // Kontroluje wolne miejsce w buforze
sem_t full;     // Kontroluje dostępność danych w buforze
pthread_mutex_t mutex; // Mutex do wyłącznego dostępu do bufora

// Flag zakończenia produkcji
volatile int production_done = 0;

// Funkcja wstawiająca dane do bufora
void put(int tab[], int size) {
    int i;
    
    for (i = 0; i < size; i++) {
        // Czekaj na wolne miejsce
        sem_wait(&empty);
        
        // Sekcja krytyczna - dostęp do bufora
        pthread_mutex_lock(&mutex);
        
        buf[in] = tab[i];
        in = (in + 1) % MAX;
        count++;
        
        pthread_mutex_unlock(&mutex);
        
        // Powiadom o dostępności danych
        sem_post(&full);
    }
}

// Funkcja pobierająca dane z bufora
int get(int tab[], int size) {
    int i;
    
    // Jeśli produkcja skończona i bufor pusty, zakończ
    if (production_done) {
        pthread_mutex_lock(&mutex);
        if (count == 0) {
            pthread_mutex_unlock(&mutex);
            return 0;
        }
        pthread_mutex_unlock(&mutex);
    }
    
    // Pobierz dostępne dane, ale nie więcej niż size
    int to_read = size;
    
    for (i = 0; i < to_read; i++) {
        // Czekaj na dostępność danych
        sem_wait(&full);
        
        // Sekcja krytyczna - dostęp do bufora
        pthread_mutex_lock(&mutex);
        
        tab[i] = buf[out];
        out = (out + 1) % MAX;
        count--;
        
        pthread_mutex_unlock(&mutex);
        
        // Powiadom o zwolnieniu miejsca
        sem_post(&empty);
    }
    
    return i;
}

void* prod(void *p) {
    int i, j;
    int iv = *(int*)p;
    int local_buf[LMAX];
    
    for (i = 0; i < 100;) {
        for (j = 0; j < LMAX && i < 100; j++, i++) {
            local_buf[j] = iv + i;
        }
        put(local_buf, j);
    }
    
    // Zwiększ licznik zakończonych producentów
    pthread_mutex_lock(&mutex);
    production_done++;
    pthread_mutex_unlock(&mutex);
    
    return NULL;
}

void* kons(void *p) {
    int i, s;
    int local_buf[LMAX];
    
    while (1) {
        s = get(local_buf, LMAX);
        
        // Jeśli nie ma więcej danych do pobrania
        if (s <= 0) {
            break;
        }
        
        for (i = 0; i < s; i++) {
            printf("%d\n", local_buf[i]);
        }
    }
    
    return NULL;
}

int main() {
    pthread_t pth1, pth2, pth3, pth4;
    int initial1 = 1, initial2 = 100;
    
    // Inicjalizacja semaforów i mutexa
    sem_init(&empty, 0, MAX);  // Początkowo bufor jest pusty, więc MAX wolnych miejsc
    sem_init(&full, 0, 0);     // Początkowo bufor jest pusty, więc 0 dostępnych danych
    pthread_mutex_init(&mutex, NULL);
    
    // Tworzenie wątków
    pthread_create(&pth1, NULL, prod, &initial1);
    pthread_create(&pth2, NULL, prod, &initial2);
    pthread_create(&pth3, NULL, kons, NULL);
    pthread_create(&pth4, NULL, kons, NULL);
    
    // Oczekiwanie na zakończenie wątków
    pthread_join(pth1, NULL);
    pthread_join(pth2, NULL);
    pthread_join(pth3, NULL);
    pthread_join(pth4, NULL);
    
    // Zwalnianie zasobów
    sem_destroy(&empty);
    sem_destroy(&full);
    pthread_mutex_destroy(&mutex);
    
    return 0;
}