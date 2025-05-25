#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <pthread.h>

volatile int buf;
pthread_mutex_t mutex;
pthread_cond_t cond_prod; 
pthread_cond_t cond_kons; 
int data_available = 0; // 0 - bufor pusty, 1 - bufor pełny

void* prod(void *p){
   int i;
   for (i=1; i<100; i++){
      pthread_mutex_lock(&mutex); // Mutex - doczytaj

      // buf ma dane
      while (data_available == 1){
         pthread_cond_wait(&cond_prod, &mutex);
      }

      buf = i; 
      data_available = 1; 
      //printf("Producent: %d\n", buf);

      pthread_cond_signal(&cond_kons); // Powiadomienie
      pthread_mutex_unlock(&mutex); 
   }
   // Dobra praktyka na zakończenie.
   pthread_mutex_lock(&mutex);
   data_available = 1; 
   buf = -1; // Koniec działania; wartość specjalna
   pthread_cond_signal(&cond_kons);
   pthread_mutex_unlock(&mutex);
   return NULL;
}

void* kons(void *p){
   int i;
   int consumed_val;
   for (i=1; i<100; i++){
      pthread_mutex_lock(&mutex); 

      // buf pusty
      while (data_available == 0){
         pthread_cond_wait(&cond_kons, &mutex);
      }

      consumed_val = buf;
      data_available = 0;
      printf("%d\n", consumed_val);

      pthread_cond_signal(&cond_prod);
      pthread_mutex_unlock(&mutex);

      // Sygnalizacja końca
      if (consumed_val == -1) break;
   }
   return NULL;
}

int main(){
   pthread_t pth1, pth2;

   // Inicjalizacja mutexu i zmiennych warunkowych
   pthread_mutex_init(&mutex, NULL);
   pthread_cond_init(&cond_prod, NULL);
   pthread_cond_init(&cond_kons, NULL);

   // Utworzenie wątków
   pthread_create(&pth1, NULL, prod, NULL);
   pthread_create(&pth2, NULL, kons, NULL);

   // Oczekiwanie na zakończenie wątków
   pthread_join(pth1, NULL);
   pthread_join(pth2, NULL);

   // Zniszczenie mutexu i zmiennych warunkowych
   pthread_mutex_destroy(&mutex);
   pthread_cond_destroy(&cond_prod);
   pthread_cond_destroy(&cond_kons);

   return 0;
}