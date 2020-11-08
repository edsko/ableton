# Pattern files

Format:

1. One line per entry in the pattern
2. Each line starts with a duration indicating when the next line
   should be read
3. The rest of the line consists of groups of three. Each group has:
   a. An index selecting a a note from the active chord
   b. A transposition
   c. A duration

For example, the first line of comptine is

```
0, 8n 0 0 4n 0 12 8n;
```

This says:

* Read the next line after 8n (8th note)
* Play two notes from the chord:
  - The first (0), transpose 0, duration 4n (quarter note)
  - The first again (0), transpose up an ocative (12), 
    duration 8n (eigth note)
