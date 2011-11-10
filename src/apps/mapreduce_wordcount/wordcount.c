/*
 * Matthew Hammer <hammer@mpi-sws.org>
 * Reinhard Munz <munz@mpi-sws.org>
 */

#include <stdlib.h>
#include <stdio.h>
#include <stdint.h>
#include <unistd.h>
#include <string.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <errno.h>
#include <regex.h>

#include "cuckoo_hash_table.c"
#include "debug.h"
#include "load_words.c"

#define BUFFSIZE (1<<20)
#define FILEPATH "/var/tmp/ceal_mapreduce_input.xml"

char* read_file_into_buffer(char* filepath) {
    DPRINTF(DEBUG_CALL, "Wordcount read file into buffer called!\n");
    // Create buffer
    char* buffer = (char*) malloc(BUFFSIZE);
    DPRINTF(DEBUG_FLOW, "Created buffer of size: %i\n", BUFFSIZE);
    // Open file
    int fd = open(filepath, O_RDONLY);
    if (fd == -1) {
        fprintf(stderr, "Can't open input file %s!\n", filepath);
        exit(1);
    }
    DPRINTF(DEBUG_FLOW, "Opened input file %s\n", filepath);
    // Initialize counters and pointers
    long left_size = BUFFSIZE - 1;
    long curr_read_bytes = -1;
    long n_read_bytes = 0;
    char* insert_here = buffer;

    while (!(left_size==0 || curr_read_bytes==0)) {
        curr_read_bytes = read(fd, insert_here, left_size);
        if (curr_read_bytes < 0) {
            fprintf(stderr, "Error reading from file: %s\n", strerror( errno ));
            exit(1);
        }
        n_read_bytes += curr_read_bytes;
        left_size -= curr_read_bytes;
        insert_here = &insert_here[curr_read_bytes];
    }
    DPRINTF(DEBUG_FLOW, "Read %ld bytes from file\n", n_read_bytes);
    if (n_read_bytes < 1) {
        fprintf(stderr, "Error nothing read from file\n");
        exit(1);
    }
    DASSERT(n_read_bytes <= BUFFSIZE);
    buffer[n_read_bytes] = '\0';
    close(fd);
    DPRINTF(DEBUG_FLOW, "Closed input file %s\n", filepath);
    return buffer;
}

int main(void) {
    char* buffer = read_file_into_buffer(FILEPATH);
    cuckoo_hash_table* ht = cuckoo_mult_init(6, 25);
    load_words_into_hashtable(ht, buffer, strlen(buffer));
    cuckoo_mult_print_table(ht);
    return 0;
}
