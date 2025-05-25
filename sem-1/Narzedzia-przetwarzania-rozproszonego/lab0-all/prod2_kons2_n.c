#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <pthread.h>
#include <semaphore.h>

#define MAX 5

int buf[MAX];
volatile int count = 0;

volatile int in = 0, out = 0;

pthread_mutex_t mutex = PTHREAD_MUTEX_INITIALIZER;
pthread_cond_t not_full = PTHREAD_COND_INITIALIZER;
pthread_cond_t not_empty = PTHREAD_COND_INITIALIZER;

void* prod(void *p) {
    int i;
    for (i = 1; i < 100; i++) {
         pthread_mutex_lock(&mutex);
      
      while(count == MAX) {
         pthread_cond_wait(&not_full, &mutex);
      }
      
      buf[in] = i;
      in = (in + 1) % MAX;
      count++;
      
      pthread_cond_signal(&not_empty);
      pthread_mutex_unlock(&mutex);
   }
    return NULL;
}

void* kons(void *p) {
    int index = 0;
    int i;
    for (i = 1; i < 100; i++) {
        pthread_mutex_lock(&mutex);
        while(count == 0) {
         pthread_cond_wait(&not_empty, &mutex);
      }
        
        printf("%d\n", buf[index]);
        index = (index + 1) % MAX;
        count--;
        pthread_cond_signal(&not_full);
        pthread_mutex_unlock(&mutex);
    }
    return NULL;
}

int main() {
    pthread_t pth1, pth2, pth3, pth4;
    pthread_mutex_init(&mutex, NULL);
    
    pthread_create(&pth1, NULL, prod, NULL);
    pthread_create(&pth2, NULL, prod, NULL);
    pthread_create(&pth3, NULL, kons, NULL);
    pthread_create(&pth4, NULL, kons, NULL);

    pthread_join(pth1, NULL);
    pthread_join(pth2, NULL);
    pthread_join(pth3, NULL);
    pthread_join(pth4, NULL);

    pthread_mutex_destroy(&mutex);
    return 0;
}
