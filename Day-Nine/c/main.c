#include <stdio.h>
#include <sys/time.h>
#include <stdint-gcc.h>
#include <malloc.h>
#include <inttypes.h>

/**
 * @brief A node for a doubly linked list
 */
typedef struct DoubleNode {
    struct DoubleNode* prev;
    uint64_t value;
    struct DoubleNode* next;
} DoubleNode;

typedef struct DoublyLinkedList {
    DoubleNode* head;
    DoubleNode* last;
} DoublyLinkedList;

/**
 * @param val The value inside the node
 * @return The new node
 */
DoubleNode* singleton(uint64_t val) {
    DoubleNode* dn = (DoubleNode*) malloc(sizeof(DoubleNode));

    dn->next = dn;
    dn->prev = dn;
    dn->value = val;

    return dn;
}

/**
 * @param val The value to store
 * @return a Doubly Linked List, with a singular value @param val inside
 */
DoublyLinkedList* untiedSingleton(uint64_t val) {
    DoubleNode* dn = (DoubleNode*) malloc(sizeof(DoubleNode));

    dn->next = NULL;
    dn->prev = NULL;
    dn->value = val;

    DoublyLinkedList* ll = (DoublyLinkedList*) malloc(sizeof(DoublyLinkedList));
    ll->head = dn;
    ll->last = dn;
    return ll;
}

/**
 * @param node The node we want to add after
 * @param val The value inside of the new node
 * @return The new node
 */
DoubleNode* insert(DoubleNode *node, uint64_t val) {
    DoubleNode* other = singleton(val);

    other->next = node->next;
    other->prev = node;
    node->next->prev = other;
    node->next = other;

    return other;
}

/**
 * Pushes to end of list
 * @param dn Doubly linked list
 * @param val Value to push
 * @return @param dn
 */
DoublyLinkedList* push(DoublyLinkedList* dn, uint64_t val) {
    DoubleNode* n = singleton(val);
    n->next = NULL;
    n->prev = dn->last;
    dn->last->next = n;
    dn->last = n;
    return dn;
}

/**
 * Take from a doubly linked list's head
 * @param dn Doubly linked list
 * @return The value at the start
 */
uint64_t consume(DoublyLinkedList* dn) {
    uint64_t val = dn->head->value;
    DoubleNode* newHead = dn->head->next;
    dn->head->next->prev = NULL;
    dn->head = newHead;

    return val;
}

/**
 * Log out a doubly linked list
 * @param dn
 */
void showDoublyLinkedList(DoublyLinkedList* dn) {
    for (DoubleNode* curr = dn->head; curr != NULL; curr = curr->next) {
        printf("%" PRId64 ", ", curr->value);
    }
}

/**
 * Get the maximum number in a linked list
 * @param dn Linked list
 * @return maximum number
 */
uint64_t maximum(DoublyLinkedList* dn) {
    uint64_t maximum = 0;
    for (DoubleNode* curr = dn->head; curr != NULL; curr = curr->next) {
        if (curr->value > maximum) maximum = curr->value;
    }
    return maximum;
}

/**
 * Generate a list of zeroes
 * @param playercount amount of zeroes
 * @return Linked list
 */
DoublyLinkedList* generatePlayers(uint64_t playercount) {
    DoublyLinkedList* dn = untiedSingleton(0);

    for (int i = 1; i < playercount; ++i) {
        push(dn, 0);
    }

    return dn;
}

int main() {
    struct timespec start, end;

    // start timer
    clock_gettime(CLOCK_MONOTONIC, &start);

    uint64_t playerCount = 427;
    DoublyLinkedList* players = generatePlayers(playerCount);
    int maximumMarbles = 7072300;
    DoubleNode* head = singleton(0);

    DoubleNode* curr = head;

    for (uint64_t i = 1; i < maximumMarbles; ++i) {
        uint64_t player = consume(players);

        if (i % 23 == 0) {
            for (int j = 0; j < 8; ++j) {
                curr = curr->prev;
            }

            DoubleNode* next = curr->next;
            uint64_t value = curr->value;

            next->prev = curr->prev;
            curr->prev->next = next;

            curr = next->next;

            push(players, player + i + value);
        } else {
            curr = insert(curr, i)->next;
            push(players, player);
        }
    }

    // stop timer
    clock_gettime(CLOCK_MONOTONIC, &end);

    printf("Winner's score is: %" PRId64 "!\n", maximum(players));

    time_t delta_us = (end.tv_sec - start.tv_sec) * 1000000 + (end.tv_nsec - start.tv_nsec) / 1000;
    printf("Took %" PRId64 "us", delta_us);

    return 0;
}