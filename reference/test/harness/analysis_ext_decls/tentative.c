int x = 2;      /* def */
int y;         /* tentative, decl */
static int u;  /* tentative, def = 0 */
static int z;  /* tentative , decl */
int x;         /* decl */ 
int y;         /* tentative, decl */
static int z = 2; /* def */
int x;         /* decl */
int y = 3;     /* def */
extern int u;   /* decl */
