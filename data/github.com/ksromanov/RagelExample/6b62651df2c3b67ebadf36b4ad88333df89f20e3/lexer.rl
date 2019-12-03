#include <stdio.h>
#include <string.h>

%%{

  machine test_lexer;
  
  spaces = " "+;
  words  = (^" ")+; # Can be alnum+
  
  main := |*
    words  => { count ++; };
    spaces => { /* nothing here */ };
  *|;  
  
}%%

int run( char * str, unsigned int len) {
    int count = 0;

    // Various variables, which will be used by Ragel generated code
    // below.
    char *p = str, 
         *pe = str + len,
         *eof = pe;
    char *ts, *te; // token start, token end.
    int cs, act;   // Variables, internally required by scanner.
    
    %% write data;
    %% write init;
    %% write exec;

    return count;
}

// Below is just a function main.
int main() {
    char * str = "a b c";//"alpha b c d e f g hi"; // deprecation warning, but it is ok.

    printf("%d\n", run(str, strlen(str)));

    return 0;
}
