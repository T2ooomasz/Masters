#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <pthread.h>

#define MAX 10
#define LMAX 3  // Rozmiar lokalnego bufora do operacji odczytu/zapisu

// Bufor cykliczny
int buf[MAX];
// Zmienne do śledzenia pozycji w buforze
volatile int read_pos = 0;
volatile int write_pos = 0;
// Zmienne do śledzenia zapełnienia bufora
volatile int count = 0;
volatile int total_produced = 0;
volatile int total_consumed = 0;
volatile int producers_done = 0;

// Muteks i zmienne warunkowe do synchronizacji
pthread_mutex_t mutex = PTHREAD_MUTEX_INITIALIZER;
pthread_cond_t not_full = PTHREAD_COND_INITIALIZER;
pthread_cond_t not_empty = PTHREAD_COND_INITIALIZER;

void put(int tab[], int size){
   int i;
   for (i=0; i < size; i++){
      pthread_mutex_lock(&mutex);
      
      // Czekaj jeśli bufor jest pełny
      while(count == MAX) {
         pthread_cond_wait(&not_full, &mutex);
      }
      
      // Zapisz wartość do bufora
      buf[write_pos] = tab[i];
      write_pos = (write_pos + 1) % MAX;
      count++;
      total_produced++;
      
      // Zasygnalizuj, że bufor nie jest już pusty
      pthread_cond_signal(&not_empty);
      pthread_mutex_unlock(&mutex);
   }
}

int get(int tab[], int size){
   int i;
   
   for (i=0; i < size; i++){
      pthread_mutex_lock(&mutex);
      
      // Sprawdź czy producenci skończyli i bufor jest pusty
      if (producers_done == 2 && count == 0) {
         pthread_mutex_unlock(&mutex);
         return i;
      }
      
      // Czekaj jeśli bufor jest pusty
      while(count == 0 && producers_done < 2) {
         pthread_cond_wait(&not_empty, &mutex);
      }
      
      // Sprawdź ponownie po obudzeniu
      if (producers_done == 2 && count == 0) {
         pthread_mutex_unlock(&mutex);
         return i;
      }
      
      // Odczytaj wartość z bufora
      tab[i] = buf[read_pos];
      read_pos = (read_pos + 1) % MAX;
      count--;
      total_consumed++;
      
      // Zasygnalizuj, że bufor nie jest już pełny
      pthread_cond_signal(&not_full);
      pthread_mutex_unlock(&mutex);
   }
   
   return i;  // Zwróć liczbę odczytanych elementów
}

void* prod(void *p){
   int i, j;
   int iv = *(int*)p;
   int local_buf[LMAX];
   
   for (i=0; i < 100; i++){
      for (j=0; j<LMAX && i<100; j++, i++){
         local_buf[j] = iv+i;
      }
      put(local_buf, j);
      i--;  // Korekta licznika
   }
   
   // Oznacz, że producent skończył pracę
   pthread_mutex_lock(&mutex);
   producers_done++;
   pthread_cond_broadcast(&not_empty);  // Obudź wszystkie czekające wątki
   pthread_mutex_unlock(&mutex);
   
   return NULL;
}

void* kons(void *p){
   int i, s;
   int local_buf[LMAX];
   
   while ((s = get(local_buf, LMAX)) > 0){
      for (i=0; i < s; i++){
         printf("%d\n", local_buf[i]);
      }
   }
   
   return NULL;
}

int main(){
   pthread_t pth1, pth2, pth3, pth4;
   int initial1 = 1, initial2 = 100;
   
   pthread_create(&pth1, NULL, prod, &initial1);
   pthread_create(&pth2, NULL, prod, &initial2);
   pthread_create(&pth3, NULL, kons, NULL);
   pthread_create(&pth4, NULL, kons, NULL);
   
   pthread_join(pth1, NULL);
   pthread_join(pth2, NULL);
   pthread_join(pth3, NULL);
   pthread_join(pth4, NULL);
   
   return 0;
}