#define _POSIX_SOURCE   /* fdopen()     */
#define _BSD_SOURCE     /* upsleep()    */

#include <sys/types.h>  /* pid_t        */
#include <stdlib.h>     /* malloc/free  */
#include <unistd.h>     /* syscalls     */
#include <curses.h>     /* curses       */
#include <fcntl.h>      /* open() flags */

struct leaf {
  int x;
  int y;
  char sym;
};

struct leaf *read_tree(FILE *in, FILE *out, int *n);
void init_tree(FILE *in, char *rfile);
void next_tree(FILE *in);
void prev_tree(FILE *in);
void quit_tree(FILE *in);
void new_tree(FILE *in);

void draw_tree(int n, struct leaf *ls);

int main(int argc, char **argv)
{
  int st;
  char *rfile;

  if (argc > 2) {
    printf("usage: %s [rulefile]\n", argv[0]);
  } else if (argc == 2) {
    st = open(argv[1], O_RDONLY);
    if (st >= 0) {
      rfile = argv[1];
    } else {
      fprintf(stderr, "cannot open rule file `%s'\n", argv[1]);
      exit(EXIT_FAILURE);
    }
  } else {
    rfile = 0;
  }

  int in_p[2], out_p[2];
  pid_t pid;

  pipe(in_p);
  pipe(out_p);

  pid = fork();
  if (pid > 0) {
    close(in_p[0]);
    close(out_p[1]);

    FILE *in, *out;
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

    if (rfile)
      init_tree(in, rfile);

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
      case KEY_RIGHT:
      case KEY_DOWN:
        next_tree(in);
        break;
      case 'p':
      case 'k':
      case KEY_LEFT:
      case KEY_UP:
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

    int err_fd;
    err_fd = open("errors.log", O_CREAT|O_WRONLY, 0666);
    close(STDERR_FILENO);
    dup(err_fd);
    close(err_fd);

    execl("/usr/bin/erl", "/usr/bin/erl", "-noshell", "-s", "a_shell", "-s", "init", "stop", 0);
  }

  return EXIT_SUCCESS;
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

void init_tree(FILE *in, char *rfile)
{
  fprintf(in, "r %s\n", rfile);
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
