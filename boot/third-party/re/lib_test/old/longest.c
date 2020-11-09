
#include <stdlib.h>
#include <regex.h>

main () {
  regex_t preg;
  regmatch_t pmatch[3];

  regcomp (&preg,  "(aa?)*", REG_EXTENDED);
  regexec (&preg,  "aaa", 2, pmatch,  0);

  printf ("%d %d %d %d (expected: 0 3 1 3)\n",
          pmatch[0].rm_so, pmatch[0].rm_eo, pmatch[1].rm_so, pmatch[1].rm_eo);

  regcomp (&preg,  "(a*)(ab)?b*", REG_EXTENDED);
  regexec (&preg,  "aaabb", 3, pmatch,  0);

  printf ("%d %d %d %d %d %d (expected: 0 5 0 2 2 4)\n",
          pmatch[0].rm_so, pmatch[0].rm_eo,
          pmatch[1].rm_so, pmatch[1].rm_eo,
          pmatch[2].rm_so, pmatch[2].rm_eo);

  regcomp (&preg,  "((a?)|b)*", REG_EXTENDED);
  regexec (&preg,  "ab", 3, pmatch,  0);

  printf ("%d %d %d %d %d %d (expected: 0 2 1 2 -1 -1)\n",
          pmatch[0].rm_so, pmatch[0].rm_eo,
          pmatch[1].rm_so, pmatch[1].rm_eo,
          pmatch[2].rm_so, pmatch[2].rm_eo);

  regcomp (&preg,  "((a)|b)*", REG_EXTENDED);
  regexec (&preg,  "ab", 3, pmatch,  0);

  printf ("%d %d %d %d %d %d (expected: 0 2 1 2 -1 -1)\n",
          pmatch[0].rm_so, pmatch[0].rm_eo,
          pmatch[1].rm_so, pmatch[1].rm_eo,
          pmatch[2].rm_so, pmatch[2].rm_eo);
}
