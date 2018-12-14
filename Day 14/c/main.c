#include <stdio.h>
#include <malloc.h>
#include <stdint.h>
#include <stdlib.h>

typedef struct {
    uint64_t *array;
    size_t used;
    size_t size;
} Array;

Array* initArray(size_t initialSize) {
    Array* a = malloc(sizeof(Array));

    a->array = (uint64_t*) malloc(initialSize * sizeof(uint64_t));
    a->used = 0;
    a->size = initialSize;

    return a;
}

void insertArray(Array *arr, uint64_t element) {
  // a->used is the number of used entries, because a->array[a->used++] updates a->used only *after* the array has been accessed.
  // Therefore a->used can go up to a->size
  if (arr->used == arr->size) {
    arr->size *= 2;
    arr->array = (uint64_t *)realloc(arr->array, arr->size * sizeof(uint64_t));
  }
  arr->array[arr->used++] = element;
}

uint64_t at(Array* arr, size_t index) {
    return arr->array[index];
}

void freeArray(Array *arr) {
  free(arr->array);
  arr->array = NULL;
  arr->used = arr->size = 0;
}

void printArray(Array* arr) {
    for (int index = 0; index < arr->used; ++index) {
        printf("%i ", at(arr, index));
    }
}




int main() {
    uint64_t input = 681901;

    uint64_t inputDecs[6] = {6, 8, 1, 9, 0, 1};

    Array* recipes = initArray(2);

    int part2 = 0;

    insertArray(recipes, 3);
    insertArray(recipes, 7);

    uint64_t posA = 0,
             posB = 1;



    for (uint64_t i = 0; i < input + 10 || part2; ++i) {
        uint64_t recipeA = at(recipes, posA),
                 recipeB = at(recipes, posB);

        uint64_t sum = recipeA + recipeB;

        uint64_t quot = sum / 10;
        uint64_t rem  = sum % 10;

        if (quot == 0) {
            insertArray(recipes, rem);
        } else {
            insertArray(recipes, quot);
            insertArray(recipes, rem);
        }

        posA = (posA + recipeA + 1) % recipes->used;
        posB = (posB + recipeB + 1) % recipes->used;

        if (part2) {
            uint64_t inputSize = sizeof(inputDecs) / sizeof(uint64_t);

            if (recipes->used >= inputSize) {
                int eq = 1;
                for (uint64_t j = recipes->used - inputSize; j < recipes->used; ++j) {
                    uint64_t a = inputDecs[j - (recipes->used - inputSize)];
                    uint64_t b = at(recipes, j);

                    if (a != b) {
                        eq = 0;
                        break;
                    }
                }
                if (eq) {
                    printf("Found match\n");
                    for (uint64_t j = recipes->used - inputSize; j < recipes->used; ++j) {
                        uint64_t a = inputDecs[j - (recipes->used - inputSize)];
                        uint64_t b = at(recipes, j);
                        printf("%llu ==? %llu, ", a, b);
                    }
                    break;
                }
            }
        }
    }

    if (!part2) {
        printf(" ---- PART ONE ---- \n");
        for (size_t j = input; j < input + 10; ++j) {
            printf("%llu", at(recipes, j));
        }
    }

    freeArray(recipes);

    return 0;
}