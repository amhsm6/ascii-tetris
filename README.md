# Terminal tetris
The goal of that was to check out lenses.

## Usage
- Install ncurses.
- Compile and run.

## Haskell ncurses package error
```
dist/build/UI/NCurses/Enums.chs.h:140: (column 25) [ERROR]  >>> Unknown identifier!
  Cannot find a definition for `KEY_EVENT' in the header file.
```

- If you get that error or similar one, go to cabal cache directory (typical ~/.cache/cabal/hackage.haskell.org).
- Find ncurses package directory and there file ncurses-\*.\*.\*.\*.tar.gz.
- Unpack it.
- Find the file mentioned in the error (for example lib/UI/NCurses/Enum.chs).
- Delete the line which it is angry at.
- Repack it.
