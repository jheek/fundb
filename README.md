# fundb

convenience command for testing
```
stack build --pedantic --file-watch --haddock --test --test-arguments "--fail-fast --rerun --failure-report=./failures.log --rerun-all-on-success"
```

convenience command for testing (without docs)
```
stack build --pedantic --file-watch --test --test-arguments "--fail-fast --rerun --failure-report=./failures.log --rerun-all-on-success"
```

convenience command for executable (without docs)
```
stack build --pedantic --file-watch --exec "stack exec -- main +RTS -N1 -s"
```

        160,104,936 bytes allocated in the heap
     287,872,648 bytes copied during GC
      50,519,296 bytes maximum residency (9 sample(s))
         140,032 bytes maximum slop
             114 MB total memory in use (0 MB lost due to fragmentation)

                                     Tot time (elapsed)  Avg pause  Max pause
  Gen  0       145 colls,     0 par    0.443s   0.523s     0.0036s    0.1096s
  Gen  1         9 colls,     0 par    0.017s   0.018s     0.0020s    0.0027s

  TASKS: 4 (1 bound, 3 peak workers (3 total), using -N1)

  SPARKS: 0 (0 converted, 0 overflowed, 0 dud, 0 GC'd, 0 fizzled)

  INIT    time    0.001s  (  0.010s elapsed)
  MUT     time    0.069s  (  0.086s elapsed)
  GC      time    0.460s  (  0.541s elapsed)
  EXIT    time   -0.001s  (  0.025s elapsed)
  Total   time    0.529s  (  0.663s elapsed)

  Alloc rate    2,304,662,962 bytes per MUT second

  Productivity  13.0% of total user, 16.8% of total elapsed

   2,476,366,440 bytes allocated in the heap
     143,893,376 bytes copied during GC
      32,443,000 bytes maximum residency (8 sample(s))
         312,712 bytes maximum slop
              65 MB total memory in use (0 MB lost due to fragmentation)

                                     Tot time (elapsed)  Avg pause  Max pause
  Gen  0      2402 colls,     0 par    0.140s   0.173s     0.0001s    0.0417s
  Gen  1         8 colls,     0 par    0.000s   0.000s     0.0001s    0.0002s

  TASKS: 4 (1 bound, 3 peak workers (3 total), using -N1)

  SPARKS: 0 (0 converted, 0 overflowed, 0 dud, 0 GC'd, 0 fizzled)

  INIT    time    0.001s  (  0.010s elapsed)
  MUT     time    0.887s  (  0.909s elapsed)
  GC      time    0.141s  (  0.174s elapsed)
  EXIT    time    0.001s  (  0.017s elapsed)
  Total   time    1.030s  (  1.110s elapsed)

  Alloc rate    2,790,344,358 bytes per MUT second

  Productivity  86.3% of total user, 83.4% of total elapsed