#define _POSIX_SOURCE   /* fdopen()     */
#define _BSD_SOURCE     /* upsleep()    */

#include <sys/types.h>  /* pid_t        */
#include <stdlib.h>     /* exit()       */
#include <unistd.h>     /* syscalls     */
#include <curses.h>     /* curses       */
#include <signal.h>     /* kill()       */
#include <string.h>     /* strlen()     */

struct leaf {
  int x;
  int y;
  char sym;
};

struct leaf *read_tree(FILE *in, FILE *out, int *n);
void next_tree(FILE *in);
void prev_tree(FILE *in);
void quit_tree(FILE *in);
void new_tree(FILE *in);

void draw_tree(int n, struct leaf *ls);

int main()
{
  int in_p[2], out_p[2];
  FILE *in, *out;
  pid_t pid;

  pipe(in_p);
  pipe(out_p);

  pid = fork();
  if (pid > 0) {
    /*printf("parent process: child has pid %d, buffer size: %d\n", pid, BUFSIZ);*/
    close(in_p[0]);
    close(out_p[1]);

    in  = fdopen(in_p[1], "w");
    if (!in) {
      perror("fdopen");
      exit(1);
    }
    setvbuf(in, 0, _IOLBF, BUFSIZ);

    out = fdopen(out_p[0], "r");
    if (!out) {
      perror("fdopen");
      exit(1);
    }
    setvbuf(out, 0, _IOLBF, BUFSIZ);

    initscr();
    atexit((void (*)(void))endwin);
    noecho();

    int c, n;
    struct leaf *ls;
    ls = read_tree(in, out, &n);
    draw_tree(n, ls);
    free(ls);
    do {
      c = getch();
      switch(c) {
      case 'n':
      case 'j':
        next_tree(in);
        break;
      case 'p':
      case 'k':
        prev_tree(in);
        break;
      case 'r':
        new_tree(in);
        break;
      }
      ls = read_tree(in, out, &n);
      draw_tree(n, ls);
      free(ls);
    } while (c != 'q');

    quit_tree(in);
  } else {
    close(STDOUT_FILENO);
    dup(out_p[1]);
    close(out_p[1]);
    close(out_p[0]);

    close(STDIN_FILENO);
    dup(in_p[0]);
    close(in_p[0]);
    close(in_p[1]);

    execl("/usr/bin/erl", "/usr/bin/erl", "-noshell", "-s", "a_shell", "-s", "init", "stop", 0);
  }

  return 0;
}

struct leaf *read_tree(FILE *in, FILE *out, int *n)
{
  char buf[BUFSIZ];

  fputs("p\n", in);
  usleep(10000);
  fgets(buf, BUFSIZ, out);
  sscanf(buf, "%d\n", n);

  struct leaf *r, *l;
  r = l = calloc(sizeof(struct leaf), *n);

  int i;
  for (i = 0; i < *n; i++) {
    fgets(buf, BUFSIZ, out);
    sscanf(buf, "%d %d %c\n", &l->x, &l->y, &l->sym);
    l++;
  }

  return r;
}

void next_tree(FILE *in)
{
  fputs("j\n", in);
}

void prev_tree(FILE *in)
{
  fputs("k\n", in);
}

void new_tree(FILE *in)
{
  fputs("n\n", in);
}

void quit_tree(FILE *in)
{
  fputs("q\n", in);
  sleep(1);
}

void draw_tree(int n, struct leaf *ls)
{
  clear();

  int i;
  for (i = 0; i < n; i++, ls++) {
    int x,y;
    x = ls->x + COLS/2 - 1;
    y = LINES - ls->y - 1;
    mvaddch(y, x, ls->sym);
  }

  move(LINES-1,COLS-1);
  refresh();
}

/* vim: se ai sts=3 sw=2 et: */
