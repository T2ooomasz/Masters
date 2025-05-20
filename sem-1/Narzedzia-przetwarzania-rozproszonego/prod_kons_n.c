#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <pthread.h>

#define MAX_BUFFER_SIZE 5 
#define NUM_ITEMS 99    // Liczba elementów do wyprodukowania/skonsumowania

int buf[MAX_BUFFER_SIZE];
int in_idx = 0; 
int out_idx = 0; 
int count = 0;   

pthread_mutex_t mutex;
pthread_cond_t cond_not_full;  
pthread_cond_t cond_not_empty; 

// Sygnalizacja zakończenia produkcji
volatile int production_finished = 0;

void* prod(void *p){
   int i;
   for (i = 1; i <= NUM_ITEMS; i++){ 
      pthread_mutex_lock(&mutex); 

      while (count == MAX_BUFFER_SIZE){
         //printf("Producent: Bufor pełny, czekam...\n");
         pthread_cond_wait(&cond_not_full, &mutex);
      }

      buf[in_idx] = i;
      //printf("Producent wyprodukował: %d w buf[%d]\n", i, in_idx); 
      in_idx = (in_idx + 1) % MAX_BUFFER_SIZE; 
      count++;

      pthread_cond_signal(&cond_not_empty); 
      pthread_mutex_unlock(&mutex);

      // usleep(50000); // 50ms
   }

   // Sygnalizacja zakończenia produkcji
   pthread_mutex_lock(&mutex);
   production_finished = 1;
   pthread_cond_broadcast(&cond_not_empty); // broadcast a nie signal
   pthread_mutex_unlock(&mutex);

   //printf("Producent zakończył.\n"); 
   return NULL;
}

void* kons(void *p){
   int item;
   int consumed_count = 0;

   while(consumed_count < NUM_ITEMS) {
      pthread_mutex_lock(&mutex);

    
      while (count == 0 && !production_finished){
         //printf("Konsument: Bufor pusty, czekam...\n");
         pthread_cond_wait(&cond_not_empty, &mutex);
      }

      // Produkcja zakończona
      if (count == 0 && production_finished) {
          pthread_mutex_unlock(&mutex);
          break;
      }

      item = buf[out_idx]; 
      printf("%d\n", item);
      //printf("Konsument odczytał: %d z buf[%d]\n", item, out_idx);
      out_idx = (out_idx + 1) % MAX_BUFFER_SIZE;
      count--; 
      consumed_count++;

      pthread_cond_signal(&cond_not_full); 
      pthread_mutex_unlock(&mutex); 

      // usleep(100000); // 100ms
   }
   //printf("Konsument zakończył. Odczytano %d elementów.\n", consumed_count); // Opcjonalne
   return NULL;
}

int main(){
   pthread_t pth_prod, pth_kons;

   // Inicjalizacja mutexu i zmiennych warunkowych
   pthread_mutex_init(&mutex, NULL);
   pthread_cond_init(&cond_not_full, NULL);
   pthread_cond_init(&cond_not_empty, NULL);

   // Utworzenie wątków
   printf("Uruchamianie producenta i konsumenta...\n");
   pthread_create(&pth_prod, NULL, prod, NULL);
   pthread_create(&pth_kons, NULL, kons, NULL);

   // Oczekiwanie na zakończenie wątków
   pthread_join(pth_prod, NULL);
   pthread_join(pth_kons, NULL);

   // Zniszczenie mutexu i zmiennych warunkowych
   pthread_mutex_destroy(&mutex);
   pthread_cond_destroy(&cond_not_full);
   pthread_cond_destroy(&cond_not_empty);

   printf("Program zakończony.\n");
   return 0;
}