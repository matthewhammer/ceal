/*
 * Reinhard Munz <munz@mpi-sws.org>
 *
 */

#include "cuckoo_hash_table.c"
#include "load_words.c"
#include "debug.h"

cuckoo_hash_table* hashtable_new() {
    DPRINTF(DEBUG_CALL, "Cuckoo interface ht new called!\n");
    cuckoo_mult_init(8, 25);
}

void hashtable_fill(cuckoo_hash_table* ht, char* buffer, long buffer_size) {
    DPRINTF(DEBUG_CALL, "Cuckoo interface ht fill called!\n");
    load_words_into_hashtable(ht, buffer, buffer_size);
}



void entry_first(cuckoo_hash_table* ht) {
    DPRINTF(DEBUG_CALL, "Cuckoo interface first called!\n");
    cuckoo_mult_first(ht);
}

void entry_next(cuckoo_hash_table* ht) {
    DPRINTF(DEBUG_CALL, "Cuckoo interface next called!\n");
    cuckoo_mult_next(ht);
}

char* entry_getword(cuckoo_hash_table* ht) {
    DPRINTF(DEBUG_CALL, "Cuckoo interface getword called!\n");
    int key_size;
    char* key = cuckoo_mult_key(ht, &key_size);
    char* ret = malloc(key_size + 1);
    memcpy(ret, key, key_size);
    ret[key_size] = '\0';
    return ret;
}

long entry_getcount(cuckoo_hash_table* ht) {
    DPRINTF(DEBUG_CALL, "Cuckoo interface getcount called!\n");
    int data_size;
    char* data = cuckoo_mult_data(ht, &data_size);
    if (data_size != sizeof(long)) {
        fprintf(stderr, "Fatal error: Size of value should be size of long: is %i should be %i\n",
                data_size, sizeof(long));
        exit(1);
    }
    long ret = *((long*)data);
    return ret;
}
