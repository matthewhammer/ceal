/*
 * Reinhard Munz <munz@mpi-sws.org>
 */

#include "debug.h"

#define REGEX "\\b\\w+\\b"

int match_regex(char* string, char* regex, regmatch_t* pmatch, size_t nmatch) {
    DPRINTF(DEBUG_CALL, "Load words match regex called!\n");
    DASSERT(string);
    regex_t preg;
    if (regcomp(&preg, regex, REG_EXTENDED | REG_ICASE) != 0) {
        fprintf(stderr, "Failed to compile regex %s\n", regex);
        exit(1);
    }
    DPRINTF(DEBUG_FLOW, "Compiled regex %s\n", regex);
    if (regexec(&preg, string, nmatch, pmatch, 0) == 0) {
        DPRINTF(DEBUG_FLOW, "Regex matched something\n");
        regfree(&preg);
        return 0;
    }
    DPRINTF(DEBUG_FLOW, "Regex did not match anything\n");
    regfree(&preg);
    return 1;
}

void load_words_into_hashtable(cuckoo_hash_table* ht, char* string, long string_size) {
    DPRINTF(DEBUG_CALL, "Load words into hashtable called!\n");
    DASSERT(ht)
    DASSERT(string);
    DASSERT(string_size > 0);
    regmatch_t match;
    int ret = 0;
    long string_index = 0;
    DPRINTF(DEBUG_FLOW, "Current string pointer initialized to 0\n");
    while (ret == 0 && (ret = match_regex(&string[string_index], REGEX, &match, 1)) == 0) {
        char* entry_count = NULL;
        int data_size;
        if ((entry_count = cuckoo_mult_get(ht,
                                           &(string[string_index + match.rm_so]),
                                           match.rm_eo - match.rm_so,
                                           &data_size)) != NULL) {
            DPRINTF(DEBUG_FLOW, "Word is in hashmap so increment counter from %ld to ",
                    *((long*)entry_count));
            (*((long*)entry_count))++;
            DPRINTF(DEBUG_FLOW, "%ld\n", *((long*)entry_count));
        } else {
            DPRINTF(DEBUG_FLOW, "Word is not in hashmap so put it there with counter initialized to"
                    " 1\n");
            long val = 1;
            cuckoo_mult_put(ht,
                            &(string[string_index + match.rm_so]), (char*)(&val), match.rm_eo -
                            match.rm_so, sizeof(long));
        }
        string_index += match.rm_eo;
        DPRINTF(DEBUG_FLOW, "Current string pointer set to %ld\n", string_index);
    }
    DPRINTF(DEBUG_FLOW, "Done with parsing string\n");
}
