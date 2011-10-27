#define _POSIX_SOURCE
#define _BSD_SOURCE

#include <sys/types.h>
#include <unistd.h>
#include <curses.h>
#include <signal.h>
#include <string.h>

int main()
{
  char buf[BUFSIZ];
  int in_p[2], out_p[2];
  /*FILE *in, *out;*/
  pid_t pid;

  pipe(in_p);
  pipe(out_p);

  pid = fork();
  if (pid > 0) {
    printf("parent process: child has pid %d\n",pid);
    close(in_p[0]);
    close(out_p[1]);

    /*
    in  = fdopen(in_p[1], "w");
    if (!in)
      perror("fdopen");
    out = fdopen(out_p[0], "r");
    if (!out)
      perror("fdopen");
      */
    puts("hello");

    read(out_p[0],buf,5);
    fputs(buf, stdout);
    while (1) {
      fgets(buf, BUFSIZ, stdin);
      /*fputs(buf, in);*/
      write(in_p[1],buf,strlen(buf));
      usleep(10000);
      /*fgets(buf, 5, out);*/
      read(out_p[0],buf,BUFSIZ);
      fputs(buf, stdout);
    }

    kill(pid, 9);
    puts("bye");

    /*
    initscr();
    atexit(endwin);
    */
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
    /*execl("/bin/echo", "/bin/echo", "hallo", "welt", (char *) 0);*/
    /*execl("/bin/cat", "/bin/cat", (char *) 0);*/
  }

  return 0;
}
