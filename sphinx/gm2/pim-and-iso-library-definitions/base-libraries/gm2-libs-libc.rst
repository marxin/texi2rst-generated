.. _gm2-libs-libc:

gm2-libs/libc
^^^^^^^^^^^^^

.. code-block:: modula2

  DEFINITION MODULE FOR "C" libc ;

  FROM SYSTEM IMPORT ADDRESS, CSIZE_T, CSSIZE_T ;

  EXPORT UNQUALIFIED time_t, timeb, tm, ptrToTM,
                     write, read,
                     system, abort,
                     malloc, free,
                     exit, isatty,
                     getenv, putenv, getpid,
                     dup, close, open, lseek,
                     readv, writev,
                     perror, creat,
                     getcwd, chown, strlen, strcpy, strncpy,
                     unlink, setenv,
                     memcpy, memset, memmove, printf, realloc,
                     rand, srand,
                     time, localtime, ftime,
                     shutdown, rename, setjmp, longjmp, atexit,
                     ttyname, sleep, execv ;

  TYPE
  time_t (type)
     time_t = LONGINT ;

  ptrToTM (type)
     ptrToTM = POINTER TO tm ;
  tm (type)
     tm = RECORD
             tm_sec: INTEGER ;     (* Seconds.     [0-60] (1 leap second) *)
             tm_min: INTEGER ;     (* Minutes.     [0-59]   *)
             tm_hour: INTEGER ;    (* Hours.       [0-23]   *)
             tm_mday: INTEGER ;    (* Day.         [1-31]   *)
             tm_mon: INTEGER ;     (* Month.       [0-11]   *)
             tm_year: INTEGER ;    (* Year - 1900.          *)
             tm_wday: INTEGER ;    (* Day of week. [0-6]    *)
             tm_yday: INTEGER ;    (* Days in year.[0-365]  *)
             tm_isdst: INTEGER ;   (* DST.         [-1/0/1] *)
             tm_gmtoff: LONGINT ;  (* Seconds east of UTC.  *)
             tm_zone: ADDRESS ;    (* char * zone name      *)
  END (type)
          END ;

  timeb (type)
     timeb = RECORD
                time    : time_t ;
                millitm : SHORTCARD ;
                timezone: SHORTCARD ;
                dstflag : SHORTCARD ;
  END (type)
             END ;

  exitP (type)
     exitP = PROCEDURE () : INTEGER ;

  (*
       ssize_t write (int d, void *buf, size_t nbytes)
  *)

  write
  PROCEDURE write (d: INTEGER; buf: ADDRESS; nbytes: CSIZE_T) : [ CSSIZE_T ] ;

  (*
       ssize_t read (int d, void *buf, size_t nbytes)
  *)

  read
  PROCEDURE read (d: INTEGER; buf: ADDRESS; nbytes: CSIZE_T) : [ CSSIZE_T ] ;

  (*
       int system(string)
       char *string;
  *)

  system
  PROCEDURE system (a: ADDRESS) : [ INTEGER ] ;

  (*
       abort - generate a fault

       abort() first closes all open files if possible, then sends
       an IOT signal to the process.  This signal usually results
       in termination with a core dump, which may be used for
       debugging.

       It is possible for abort() to return control if is caught or
       ignored, in which case the value returned is that of the
       kill(2V) system call.
  *)

  abort
  PROCEDURE abort <* noreturn *> ;

  (*
       malloc - memory allocator.

       void *malloc(size_t size);

       malloc() returns a pointer to a block of at least size
       bytes, which is appropriately aligned.  If size is zero,
       malloc() returns a non-NULL pointer, but this pointer should
       not be dereferenced.
  *)

  malloc
  PROCEDURE malloc (size: CSIZE_T) : ADDRESS ;

  (*
       free - memory deallocator.

       free (void *ptr);

       free() releases a previously allocated block.  Its argument
       is a pointer to a block previously allocated by malloc,
       calloc, realloc, malloc, or memalign.
  *)

  free
  PROCEDURE free (ptr: ADDRESS) ;

  (*
       void *realloc (void *ptr, size_t size);

       realloc changes the size of the memory block pointed to
       by ptr to size bytes. The contents will be  unchanged  to
       the minimum of the old and new sizes; newly allocated memory
       will be uninitialized. If ptr is NIL, the call is
       equivalent  to malloc(size); if size is equal to zero, the
       call is equivalent to free(ptr). Unless ptr is NIL, it
       must have been returned by an earlier call to malloc(),
       realloc.
  *)

  realloc
  PROCEDURE realloc (ptr: ADDRESS; size: CSIZE_T) : ADDRESS ;

  (*
     isatty - does this descriptor refer to a terminal.
  *)

  isatty
  PROCEDURE isatty (fd: INTEGER) : INTEGER ;

  (*
     exit - returns control to the invoking process. Result, r, is
            returned.
  *)

  exit
  PROCEDURE exit (r: INTEGER) <* noreturn *> ;

  (*
     getenv - returns the C string for the equivalent C environment
              variable.
  *)

  getenv
  PROCEDURE getenv (s: ADDRESS) : ADDRESS ;

  (*
     putenv - change or add an environment variable.
  *)

  putenv
  PROCEDURE putenv (s: ADDRESS) : INTEGER ;

  (*
     getpid - returns the UNIX process identification number.
  *)

  getpid
  PROCEDURE getpid () : INTEGER ;

  (*
     dup - duplicates the file descriptor, d.
  *)

  dup
  PROCEDURE dup (d: INTEGER) : INTEGER ;

  (*
     close - closes the file descriptor, d.
  *)

  close
  PROCEDURE close (d: INTEGER) : [ INTEGER ] ;

  (*
     open - open the file, filename with flag and mode.
  *)

  open
  PROCEDURE open (filename: ADDRESS; oflag: INTEGER; ...) : INTEGER ;

  (*
     creat - creates a new file
  *)

  creat
  PROCEDURE creat (filename: ADDRESS; mode: CARDINAL) : INTEGER;

  (*
     lseek - calls unix lseek:

             off_t lseek(int fildes, off_t offset, int whence);
  *)

  lseek
  PROCEDURE lseek (fd: INTEGER; offset: LONGINT; whence: INTEGER) : LONGINT ;

  (*
     perror - writes errno and string. (ARRAY OF CHAR is translated onto ADDRESS).
  *)

  perror
  PROCEDURE perror (string: ARRAY OF CHAR);

  (*
     readv - reads an io vector of bytes.
  *)

  readv
  PROCEDURE readv (fd: INTEGER; v: ADDRESS; n: INTEGER) : [ INTEGER ] ;

  (*
     writev - writes an io vector of bytes.
  *)

  writev
  PROCEDURE writev (fd: INTEGER; v: ADDRESS; n: INTEGER) : [ INTEGER ] ;

  (*
     getcwd - copies the absolute pathname of the
              current working directory to the array pointed to by buf,
              which is of length size.

              If the current absolute path name would require a buffer
              longer than size elements, NULL is returned, and errno is
              set to ERANGE; an application should check for this error,
              and allocate a larger buffer if necessary.
  *)

  getcwd
  PROCEDURE getcwd (buf: ADDRESS; size: CSIZE_T) : ADDRESS ;

  (*
     chown - The  owner  of  the  file  specified  by  path or by fd is
             changed.  Only the super-user may change the  owner  of  a
             file.   The  owner  of  a file may change the group of the
             file to any group of which that owner is  a  member.   The
             super-user may change the group arbitrarily.

             If  the owner or group is specified as -1, then that ID is
             not changed.

             On success, zero is returned.  On error, -1  is  returned,
             and errno is set appropriately.
  *)

  chown
  PROCEDURE chown (filename: ADDRESS; uid, gid: INTEGER) : [ INTEGER ] ;

  (*
     strlen - returns the length of string, a.
  *)

  strlen
  PROCEDURE strlen (a: ADDRESS) : CSIZE_T ;

  (*
     strcpy - copies string, src, into, dest.
              It returns dest.
  *)

  strcpy
  PROCEDURE strcpy (dest, src: ADDRESS) : [ ADDRESS ] ;

  (*
     strncpy - copies string, src, into, dest, copying at most, n, bytes.
               It returns dest.
  *)

  strncpy
  PROCEDURE strncpy (dest, src: ADDRESS; n: CARDINAL) : [ ADDRESS ] ;

  (*
     unlink - removes file and returns 0 if successful.
  *)

  unlink
  PROCEDURE unlink (file: ADDRESS) : [ INTEGER ] ;

  (*
     memcpy - copy memory area

     SYNOPSIS

     #include <string.h>

     void *memcpy(void *dest, const void *src, size_t n);
     It returns dest.
  *)

  memcpy
  PROCEDURE memcpy (dest, src: ADDRESS; size: CSIZE_T) : [ ADDRESS ] ;

  (*
     memset - fill memory with a constant byte

     SYNOPSIS

     #include <string.h>

     void *memset(void *s, int c, size_t n);
     It returns s.
  *)

  memset
  PROCEDURE memset (s: ADDRESS; c: INTEGER; size: CSIZE_T) : [ ADDRESS ] ;

  (*
     memmove - copy memory areas which may overlap

     SYNOPSIS

     #include <string.h>

     void *memmove(void *dest, const void *src, size_t n);
     It returns dest.
  *)

  memmove
  PROCEDURE memmove (dest, src: ADDRESS; size: CSIZE_T) : [ ADDRESS ] ;

  (*
     int printf(const char *format, ...);
  *)

  printf
  PROCEDURE printf (format: ARRAY OF CHAR; ...) : [ INTEGER ] ;

  (*
     setenv - sets environment variable, name, to value.
              It will overwrite an existing value if, overwrite,
              is true.  It returns 0 on success and -1 for an error.
  *)

  setenv
  PROCEDURE setenv (name: ADDRESS; value: ADDRESS; overwrite: INTEGER) : [ INTEGER ] ;

  (*
     srand - initialize the random number seed.
  *)

  srand
  PROCEDURE srand (seed: INTEGER) ;

  (*
     rand - return a random integer.
  *)

  rand
  PROCEDURE rand () : INTEGER ;

  (*
     time - returns a pointer to the time_t value. If, a,
            is not NIL then the libc value is copied into
            memory at address, a.
  *)

  time
  PROCEDURE time (a: ADDRESS) : time_t ;

  (*
     localtime - returns a pointer to the libc copy of the tm
                 structure.
  *)

  localtime
  PROCEDURE localtime (VAR t: time_t) : ADDRESS ;

  (*
     ftime - return date and time.
  *)

  ftime
  PROCEDURE ftime (VAR t: timeb) : [ INTEGER ] ;

  (*
     shutdown - shutdown a socket, s.
                if how = 0, then no more reads are allowed.
                if how = 1, then no more writes are allowed.
                if how = 2, then mo more reads or writes are allowed.
  *)

  shutdown
  PROCEDURE shutdown (s: INTEGER; how: INTEGER) : [ INTEGER ] ;

  (*
     rename - change the name or location of a file
  *)

  rename
  PROCEDURE rename (oldpath, newpath: ADDRESS) : [ INTEGER ] ;

  (*
     setjmp - returns 0 if returning directly, and non-zero
              when returning from longjmp using the saved
              context.
  *)

  setjmp
  PROCEDURE setjmp (env: ADDRESS) : INTEGER ;

  (*
     longjmp - restores the environment saved by the last call
               of setjmp with the corresponding env argument.
               After longjmp is completed, program execution
               continues as if the corresponding call of setjmp
               had just returned the value val.  The value of
               val must not be zero.
  *)

  longjmp
  PROCEDURE longjmp (env: ADDRESS; val: INTEGER) ;

  (*
     atexit - execute, proc, when the function exit is called.
  *)

  atexit
  PROCEDURE atexit (proc: exitP) : [ INTEGER ] ;

  (*
     ttyname - returns a pointer to a string determining the ttyname.
  *)

  ttyname
  PROCEDURE ttyname (filedes: INTEGER) : ADDRESS ;

  (*
     sleep - calling thread sleeps for seconds.
  *)

  sleep
  PROCEDURE sleep (seconds: CARDINAL) : [ CARDINAL ] ;

  (*
     execv - execute a file.
  *)

  execv
  PROCEDURE execv (pathname: ADDRESS; argv: ADDRESS) : [ INTEGER ] ;

  END libc.

