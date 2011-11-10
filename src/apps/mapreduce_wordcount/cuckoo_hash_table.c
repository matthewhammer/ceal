/*
 * Reinhard Munz <munz@mpi-sws.org>
 *
 * My Cuckoo hash table implementation
 */

#include <stdlib.h>
#include "bob_hash.c"
#include "debug.h"

#define SEED1 0xdeadbeaf;
#define SEED2 0xc0ffee;

typedef struct {
    char** table;
    long size;
    int power;
    int cutoff;
    long index;
} cuckoo_hash_table;

cuckoo_hash_table* cuckoo_global_table;

cuckoo_hash_table* cuckoo_mult_init(int size_power, int put_cutoff) {
    DPRINTF(DEBUG_CALL, "Cuckoo mult init called!\n");
    DASSERT(size_power > 0);
    DASSERT(put_cutoff > 0);
    cuckoo_hash_table* ret = malloc(sizeof(cuckoo_hash_table));
    ret->power = size_power;
    ret->size = hashsize(ret->power);
    ret->cutoff = put_cutoff;
    ret->table = malloc(sizeof(char*) * ret->size);
    memset(ret->table, 0, sizeof(char*) * ret->size);
    ret->index = ret->size;
    DPRINTF(DEBUG_FLOW, "Initialized Cuckoo hash table with space for %li elements and put cutoff"
            " %i\n", ret->size, ret->cutoff);
    return ret;
}

void cuckoo_init(int size_power, int put_cutoff) {
    DPRINTF(DEBUG_CALL, "Cuckoo init called!\n");
    DASSERT(size_power > 0);
    DASSERT(put_cutoff > 0);
    DASSERT(!cuckoo_global_table);
    cuckoo_global_table = cuckoo_mult_init(size_power, put_cutoff);
}

void cuckoo_mult_relocate(cuckoo_hash_table* ht, char* entry, uint32_t original_bin, int num_tries);
void cuckoo_print_entry_debug(char* entry);

void cuckoo_mult_put_entry(cuckoo_hash_table* ht, char* entry) {
    DPRINTF(DEBUG_CALL, "Cuckoo mult put entry called!\n");
    DASSERT(ht);
    DASSERT(entry);
#ifdef DEBUG
    cuckoo_print_entry_debug(entry);
#endif
    uint32_t hash1 = SEED1;
    uint32_t hash2 = SEED2;
    hashlittle2(&entry[2 * sizeof(int)], *((int*)entry), &hash1, &hash2);
    hash1 = (hash1 & hashmask(ht->power));
    hash2 = (hash2 & hashmask(ht->power));
    DPRINTF(DEBUG_FLOW, "Generated hashes: #1: %i #2: %i\n", hash1, hash2);
    if (ht->table[hash1] == NULL) {
        DPRINTF(DEBUG_FLOW, "First bin is empty. Put entry there.\n");
        ht->table[hash1] = entry;
    } else if (ht->table[hash2] == NULL) {
        DPRINTF(DEBUG_FLOW, "Second bin is empty. Put entry there.\n");
        ht->table[hash2] = entry;
    } else {
        DPRINTF(DEBUG_FLOW, "Both bins occupied. Throw out second and put entry there.\n");
        char* tmp = ht->table[hash2];
        ht->table[hash2] = entry;
        cuckoo_mult_relocate(ht, tmp, hash2, 1);
    }
}

void cuckoo_put_entry(char* entry) {
    DPRINTF(DEBUG_CALL, "Cuckoo put called!\n");
    DASSERT(cuckoo_global_table);
    DASSERT(entry);
    cuckoo_mult_put_entry(cuckoo_global_table, entry);
}

void cuckoo_mult_put(cuckoo_hash_table* ht, char* key, char* data, int key_size, int data_size) {
    DPRINTF(DEBUG_CALL, "Cuckoo mult put called!\n");
    DASSERT(ht);
    DASSERT(key_size > 0);
    DASSERT(data_size > 0);
    DASSERT(key);
    DASSERT(data);
    char* entry = malloc(2 * sizeof(int) + key_size + data_size);
    *((int*)entry) = key_size;
    *((int*)&entry[sizeof(int)]) = data_size;
    memcpy(&entry[2 * sizeof(int)], key, key_size);
    memcpy(&entry[2 * sizeof(int) + key_size], data, data_size);
    DPRINTF(DEBUG_FLOW, "Created entry\n");
#ifdef DEBUG
    cuckoo_print_entry_debug(entry);
#endif
    cuckoo_mult_put_entry(ht, entry);
}

void cuckoo_put(char* key, char* data, int key_size, int data_size) {
    DPRINTF(DEBUG_CALL, "Cuckoo put called!\n");
    DASSERT(cuckoo_global_table);
    DASSERT(key_size > 0);
    DASSERT(data_size > 0);
    DASSERT(key);
    DASSERT(data);
    cuckoo_mult_put(cuckoo_global_table, key, data, key_size, data_size);
}

char* cuckoo_mult_get(cuckoo_hash_table* ht, char* key, int key_size, int* data_size) {
    DPRINTF(DEBUG_CALL, "Cuckoo mult get called!\n");
    DASSERT(ht);
    DASSERT(key_size > 0);
    DASSERT(key);
    DASSERT(data_size);
    DPRINTF(DEBUG_FLOW, "Key size: %i Key: %.*s\n", key_size, key_size, key);
    uint32_t hash1 = SEED1;
    uint32_t hash2 = SEED2;
    hashlittle2(key, key_size, &hash1, &hash2);
    hash1 = (hash1 & hashmask(ht->power));
    hash2 = (hash2 & hashmask(ht->power));
    DPRINTF(DEBUG_FLOW, "Generated hashes: #1: %i #2: %i\n", hash1, hash2);
    if (ht->table[hash1] != NULL &&
        key_size == *((int*)(ht->table[hash1])) &&
        memcmp(key, &((ht->table[hash1])[2 * sizeof(int)]), key_size) == 0) {
        DPRINTF(DEBUG_FLOW, "Matches first bin\n");
#ifdef DEBUG
        cuckoo_print_entry_debug(ht->table[hash1]);
#endif
        *data_size = *((int*)(&((ht->table[hash1])[sizeof(int)])));
        return &((ht->table[hash1])[2 * sizeof(int) + key_size]);
    } else if (ht->table[hash2] != NULL &&
               key_size == *((int*)(ht->table[hash2])) &&
               memcmp(key, &((ht->table[hash2])[2 * sizeof(int)]), key_size) == 0) {
        DPRINTF(DEBUG_FLOW, "Matches second bin\n");
#ifdef DEBUG
        cuckoo_print_entry_debug(ht->table[hash2]);
#endif
        *data_size = *((int*)(&((ht->table[hash2])[sizeof(int)])));
        return &((ht->table[hash2])[2 * sizeof(int) + key_size]);
    }
    DPRINTF(DEBUG_FLOW, "Doesn't match\n");
    *data_size = 0;
    return NULL;
}

char* cuckoo_get(char* key, int key_size, int* data_size) {
    DPRINTF(DEBUG_CALL, "Cuckoo get called!\n");
    DASSERT(cuckoo_global_table);
    DASSERT(key_size > 0);
    DASSERT(key);
    DASSERT(data_size);
    return cuckoo_mult_get(cuckoo_global_table, key, key_size, data_size);
}

void cuckoo_mult_remove(cuckoo_hash_table* ht, char* key, int key_size) {
    DPRINTF(DEBUG_CALL, "Cuckoo mult remove called!\n");
    DASSERT(ht);
    DASSERT(key_size > 0);
    DASSERT(key);
    DPRINTF(DEBUG_FLOW, "Key size: %i Key: %.*s\n", key_size, key_size, key);
    uint32_t hash1 = SEED1;
    uint32_t hash2 = SEED2;
    hashlittle2(key, key_size, &hash1, &hash2);
    hash1 = (hash1 & hashmask(ht->power));
    hash2 = (hash2 & hashmask(ht->power));
    DPRINTF(DEBUG_FLOW, "Generated hashes: #1: %i #2: %i\n", hash1, hash2);
    if (ht->table[hash1] != NULL &&
        key_size == *((int*)(ht->table[hash1])) &&
        memcmp(key, &((ht->table[hash1])[2 * sizeof(int)]), key_size) == 0) {
        DPRINTF(DEBUG_FLOW, "Matches first bin");
#ifdef DEBUG
        cuckoo_print_entry_debug(ht->table[hash1]);
#endif
        free(ht->table[hash1]);
        ht->table[hash1] = NULL;
    } else if (ht->table[hash2] != NULL &&
               key_size == *((int*)(ht->table[hash2])) &&
               memcmp(key, &((ht->table[hash2])[2 * sizeof(int)]), key_size) == 0) {
        DPRINTF(DEBUG_FLOW, "Matches second bin");
#ifdef DEBUG
        cuckoo_print_entry_debug(ht->table[hash2]);
#endif
        free(ht->table[hash2]);
        ht->table[hash2] = NULL;
    }
}

void cuckoo_remove(char* key, int key_size) {
    DPRINTF(DEBUG_CALL, "Cuckoo remove called!\n");
    DASSERT(cuckoo_global_table);
    DASSERT(key_size > 0);
    DASSERT(key);
    cuckoo_mult_remove(cuckoo_global_table, key, key_size);
}

void cuckoo_mult_resize(cuckoo_hash_table* ht) {
    DPRINTF(DEBUG_CALL, "Cuckoo mult resize called!\n");
    DASSERT(ht);
    cuckoo_hash_table* new_ht = cuckoo_mult_init(ht->power + 1, ht->cutoff);
    long i;
    for (i=0; i < ht->size; i++) {
        if (ht->table[i] != NULL) {
            cuckoo_mult_put_entry(new_ht, ht->table[i]);
        }
    }
    free(ht->table);
    ht->table = new_ht->table;
    ht->size = new_ht->size;
    ht->power = new_ht->power;
    ht->cutoff = new_ht->cutoff;
    ht->index = -1;
    free(new_ht);
}

void cuckoo_resize() {
    DPRINTF(DEBUG_CALL, "Cuckoo resize called!\n");
    DASSERT(cuckoo_global_table);
    cuckoo_mult_resize(cuckoo_global_table);
}

void cuckoo_mult_relocate(cuckoo_hash_table* ht, char* entry, uint32_t original_bin, int num_tries) {
    DPRINTF(DEBUG_CALL, "Cuckoo mult relocate called!\n");
    DASSERT(ht);
    DASSERT(entry);
    DASSERT(original_bin >= 0);
    DASSERT(num_tries >= 0);
#ifdef DEBUG
    cuckoo_print_entry_debug(entry);
#endif
    uint32_t hash1 = SEED1;
    uint32_t hash2 = SEED2;
    hashlittle2(&entry[2 * sizeof(int)], *((int*)entry), &hash1, &hash2);
    hash1 = (hash1 & hashmask(ht->power));
    hash2 = (hash2 & hashmask(ht->power));
    DPRINTF(DEBUG_FLOW, "Generated hashes: #1: %i #2: %i\n", hash1, hash2);
    uint32_t new_bin;
    if (hash1 == original_bin) {
        DPRINTF(DEBUG_FLOW, "First bin is the original bin. Use second bin for relocation.\n");
        new_bin = hash2;
    } else if (hash2 == original_bin) {
        DPRINTF(DEBUG_FLOW, "Second bin is the original bin. Use first bin for relocation.\n");
        new_bin = hash1;
    } else {
        fprintf(stderr, "Fatal error: Original bin should always equal one of the bins!\n");
        DPRINTF(DEBUG_ERRS,
                "When you relocate an entry you recalculate the hashes of its key, giving you the\n"
                "two possible bins it could go into. One of it should be the bin you just took\n"
                "this element out of, to relocate it.\n");
        exit(1);
    }

    if (ht->table[new_bin] == NULL) {
        DPRINTF(DEBUG_FLOW, "New bin is empty. Put entry there.\n");
        ht->table[new_bin] = entry;
    } else {
        DPRINTF(DEBUG_FLOW, "New bin is occupied. Throw out entry and put entry there.\n");
        char* tmp = ht->table[new_bin];
        ht->table[new_bin] = entry;
        if (num_tries < ht->cutoff) {
            DPRINTF(DEBUG_FLOW, "Number of relocation tries haven't reached the cutoff yet.\n"
                    "Relocate thrown out entry.\n");
            cuckoo_mult_relocate(ht, tmp, new_bin, num_tries + 1);
        } else {
            DPRINTF(DEBUG_FLOW, "Number of relocation tries reached cutoff. Resize table.\n");
            cuckoo_mult_resize(ht);
            cuckoo_mult_put_entry(ht, tmp);
        }
    }
}

void cuckoo_relocate(char* entry, uint32_t original_bin, int num_tries) {
    DPRINTF(DEBUG_CALL, "Cuckoo relocate called!\n");
    DASSERT(cuckoo_global_table);
    DASSERT(entry);
    DASSERT(original_bin >= 0);
    DASSERT(num_tries >= 0);
    cuckoo_mult_relocate(cuckoo_global_table, entry, original_bin, num_tries);
}

int cuckoo_mult_is_init(cuckoo_hash_table* ht) {
    DPRINTF(DEBUG_CALL, "Cuckoo mult is init called!\n");
    DASSERT(ht);
    if (ht->table) {
        DPRINTF(DEBUG_FLOW, "Table is not null. Is init is TRUE.\n");
        return 1;
    }
    DPRINTF(DEBUG_FLOW, "Table is null. Is init is FALSE.\n");
    return 0;
}

int cuckoo_is_init() {
    DPRINTF(DEBUG_CALL, "Cuckoo is init called!\n");
    if (cuckoo_global_table) {
        DPRINTF(DEBUG_FLOW, "Global hash table struct is not null. Check its table.\n");
        return cuckoo_mult_is_init(cuckoo_global_table);
    }
    DPRINTF(DEBUG_FLOW, "Global hash table struct is null. Is init is FALSE.\n");
    return 0;
}

void cuckoo_mult_destroy(cuckoo_hash_table* ht) {
    DPRINTF(DEBUG_CALL, "Cuckoo mult destroy called!\n");
    DASSERT(ht);
    long i;
    for (i = 0; i < ht->size; i++) {
        if (ht->table[i] != NULL) {
            DPRINTF(DEBUG_FLOW, "Destroy\n");
#ifdef DEBUG
            cuckoo_print_entry_debug(ht->table[i]);
#endif
            free(ht->table[i]);
        }
    }
    free(ht);
}

void cuckoo_destroy() {
    DPRINTF(DEBUG_CALL, "Cuckoo destroy called!\n");
    DASSERT(cuckoo_global_table);
    cuckoo_mult_destroy(cuckoo_global_table);
    cuckoo_global_table = NULL;
}

void cuckoo_mult_next(cuckoo_hash_table* ht) {
    DPRINTF(DEBUG_CALL, "Cuckoo mult next called!\n");
    DASSERT(ht);
    DASSERT(ht->index >= 0);
    DASSERT(ht->index < ht->size);
    DPRINTF(DEBUG_FLOW, "Set index from %ld to ", ht->index);
    while (ht->index < ht->size - 1 && ht->table[ht->index + 1] == NULL) {
        ht->index++;
    }
    ht->index++;
    DPRINTF(DEBUG_FLOW, "%ld\n", ht->index);
}

void cuckoo_next() {
    DPRINTF(DEBUG_CALL, "Cuckoo next called!\n");
    DASSERT(cuckoo_global_table);
    cuckoo_mult_next(cuckoo_global_table);
}

void cuckoo_mult_first(cuckoo_hash_table* ht) {
    DPRINTF(DEBUG_CALL, "Cuckoo mult first called!\n");
    DASSERT(ht);
    ht->index = 0;
    if (ht->table[ht->index] == NULL) {
        cuckoo_mult_next(ht);
    }
}

void cuckoo_first() {
    DPRINTF(DEBUG_CALL, "Cuckoo first called!\n");
    DASSERT(cuckoo_global_table);
    cuckoo_mult_first(cuckoo_global_table);
}

char* cuckoo_mult_key(cuckoo_hash_table* ht, int* key_size) {
    DPRINTF(DEBUG_CALL, "Cuckoo mult key called!\n");
    DASSERT(ht);
    DASSERT(ht->index >= 0);
    if (ht->index < ht->size) {
        DPRINTF(DEBUG_FLOW, "Return key of\n");
#ifdef DEBUG
        cuckoo_print_entry_debug(ht->table[ht->index]);
#endif
        *key_size = *((int*)(ht->table[ht->index]));
        return &((ht->table[ht->index])[2 * sizeof(int)]);
    }
    DPRINTF(DEBUG_FLOW, "Selector past end of table.\n");
    *key_size = 0;
    return NULL;
}

char* cuckoo_key(int* key_size) {
    DPRINTF(DEBUG_CALL, "Cuckoo key called!\n");
    DASSERT(cuckoo_global_table);
    return cuckoo_mult_key(cuckoo_global_table, key_size);
}

char* cuckoo_mult_data(cuckoo_hash_table* ht, int* data_size) {
    DPRINTF(DEBUG_CALL, "Cuckoo mult data called!\n");
    DASSERT(ht);
    DASSERT(ht->index >= 0);
    if (ht->index < ht->size) {
        DPRINTF(DEBUG_FLOW, "Return data of\n");
#ifdef DEBUG
        cuckoo_print_entry_debug(ht->table[ht->index]);
#endif
        *data_size = *((int*)(&((ht->table[ht->index])[sizeof(int)])));
        return &((ht->table[ht->index])[2 * sizeof(int) + *((int*)(ht->table[ht->index]))]);
    }
    DPRINTF(DEBUG_FLOW, "Selector past end of table.\n");
    *data_size = 0;
    return NULL;
}

char* cuckoo_data(int* data_size) {
    DPRINTF(DEBUG_CALL, "Cuckoo data called!\n");
    DASSERT(cuckoo_global_table);
    return cuckoo_mult_data(cuckoo_global_table, data_size);
}

void cuckoo_mult_print_table(cuckoo_hash_table* ht) {
    DPRINTF(DEBUG_CALL, "Cuckoo mult print table called!\n");
    DASSERT(ht);
    long tmp = ht->index;
    int key_size;
    int data_size;
    char* key;
    char* data;
    long counter = 0;
    printf("==========================================================="
           " Hash table start "
           "===========================================================\n");
    cuckoo_mult_first(ht);
    key = cuckoo_mult_key(ht, &key_size);
    while (key != NULL) {
        data = cuckoo_mult_data(ht, &data_size);
        printf("Entry:  [Key size: %3i, Data size: %3i, Key: %-16.*s", key_size, data_size,
               key_size, key);
        if (key_size == sizeof(int)) {
            printf(", Key(Int): %12i", *((int*)key));
        } else {
            printf(", Key(Int):             ");
        }
        printf(", Data: %-16.*s", data_size, data);
        if (data_size == sizeof(int)) {
            printf(", Data(Int): %12i", *((int*)data));
        } else {
            printf(", Data(Int):             ");
        }
        printf("]\n");
        counter++;
        cuckoo_mult_next(ht);
        key = cuckoo_mult_key(ht, &key_size);
    }
    printf("==========================================================="
           "  Hash table end  "
           "===========================================================\n");
    printf("Total number of entries : %ld\n", counter);
    printf("Current table size      : %ld\n", ht->size);
    printf("Current power           : %i\n", ht->power);
    printf("Current cut off point   : %i\n", ht->cutoff);
    printf("Current index (selector): %ld\n", tmp);
    printf("==========================================================="
           "=================="
           "===========================================================\n");
    ht->index = tmp;
}

void cuckoo_print_table() {
    DPRINTF(DEBUG_CALL, "Cuckoo print table called!\n");
    DASSERT(cuckoo_global_table);
    cuckoo_mult_print_table(cuckoo_global_table);
}

void cuckoo_print_entry_debug(char* entry) {
        DPRINTF(DEBUG_FLOW, "Entry:  [Key size: %3i, Data size: %3i, Key: %-16.*s",
                *((int*)entry),
                *((int*)(&(entry[sizeof(int)]))),
                *((int*)entry),
                &(entry[2 * sizeof(int)]));
        if (*((int*)entry) == sizeof(int)) {
            DPRINTF(DEBUG_FLOW, ", Key(Int): %12i",
                    *((int*)&(entry[2 * sizeof(int)])));
        } else {
            DPRINTF(DEBUG_FLOW, ", Key(Int):             ");
        }
        DPRINTF(DEBUG_FLOW, ", Data: %-16.*s",
                *((int*)(&(entry[sizeof(int)]))),
                &(entry[2 * sizeof(int) + *((int*)entry)]));
        if (*((int*)(&(entry[sizeof(int)]))) == sizeof(int)) {
            DPRINTF(DEBUG_FLOW, ", Data(Int): %12i",
                    *((int*)&(entry[2 * sizeof(int) + *((int*)entry)])));
        } else {
            DPRINTF(DEBUG_FLOW, ", Data(Int):             ");
        }
        DPRINTF(DEBUG_FLOW, "]\n");
}
