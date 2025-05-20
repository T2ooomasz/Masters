#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <pthread.h>

#define MAX 5

// Bufor
int buf[MAX];
// Zmienne do śledzenia pozycji w buforze
volatile int read_pos = 0;
volatile int write_pos = 0;
// Zmienne do śledzenia zapełnienia bufora
volatile int count = 0;
volatile int total_produced = 0;
volatile int total_consumed = 0;

// Muteks i zmienne warunkowe do synchronizacji
pthread_mutex_t mutex = PTHREAD_MUTEX_INITIALIZER;
pthread_cond_t not_full = PTHREAD_COND_INITIALIZER;
pthread_cond_t not_empty = PTHREAD_COND_INITIALIZER;

void* prod(void *p){
   int i;
   for (i=1; i<100; i++){
      pthread_mutex_lock(&mutex);
      
      while(count == MAX) {
         pthread_cond_wait(&not_full, &mutex);
      }
      
      buf[write_pos] = i;
      write_pos = (write_pos + 1) % MAX;
      count++;
      total_produced++;
      
      pthread_cond_signal(&not_empty);
      pthread_mutex_unlock(&mutex);
   }
   return NULL;
}

void* kons(void *p){
   int value;
   
   while(1) {
      pthread_mutex_lock(&mutex);
      
      if (total_consumed >= 2 * 99) {
         pthread_mutex_unlock(&mutex);
         break;
      }
      
      while(count == 0 && total_consumed < 2 * 99) {
         pthread_cond_wait(&not_empty, &mutex);
      }
      
      if (total_consumed >= 2 * 99) {
         pthread_mutex_unlock(&mutex);
         break;
      }
      
      value = buf[read_pos];
      read_pos = (read_pos + 1) % MAX;
      count--;
      total_consumed++;

      pthread_cond_signal(&not_full);
      pthread_mutex_unlock(&mutex);
      
      printf("%d\n", value);
   }
   return NULL;
}

int main(){
   pthread_t pth1, pth2, pth3, pth4;
   
   pthread_create(&pth1, NULL, prod, NULL);
   pthread_create(&pth2, NULL, prod, NULL);
   pthread_create(&pth3, NULL, kons, NULL);
   pthread_create(&pth4, NULL, kons, NULL);
   
   pthread_join(pth1, NULL);
   pthread_join(pth2, NULL);
   pthread_join(pth3, NULL);
   pthread_join(pth4, NULL);
   
   return 0;
}