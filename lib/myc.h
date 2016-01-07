#ifndef MYCH
#define MYCH

#include <sys/stat.h>
#include <sys/time.h>
#include <sys/wait.h>
#include <sys/types.h>
#include <sys/acl.h>
#include <sys/param.h>
#include <sys/mount.h>

#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>
#include <string.h>
#include <assert.h>
#include <unistd.h>
#include <fcntl.h>
#include <grp.h>
#include <limits.h>
#include <paths.h>
#include <err.h>

extern char **environ;

#define FOR(k,a,b) for(__typeof(a) k=(a); k < (b); k++)
#define REP(k,a) FOR(k,0,a)
#define SIZE(x) (sizeof(x)/sizeof((x)[0]))
#define TRUE 1
#define FALSE 0

void p(char *fmt, ...) {
    va_list list;
    va_start(list, fmt);
    vprintf(fmt, list);
    va_end(list);
}

void P(char *fmt, ...) {
    va_list list;
    va_start(list, fmt);
    vprintf(fmt, list);
    va_end(list);
}

#define PVI(a) REP(i, SIZE(a)) P("%d, ", a[i]);

#endif
