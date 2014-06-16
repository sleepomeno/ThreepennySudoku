# Haskell Web Sudoku Interface

## What is it?

This application provides an interface for solving sudokus. It is
written in Haskell with the
[threepenny-gui](https://github.com/HeinrichApfelmus/threepenny-gui)
framework.

![alt text](https://raw.github.com/sleepomeno/ThreepennySudoku/master/screenshot.png
 "Sudoku Screenshot")
## What is it for?

Writing Haskell GUIs has not been easy in the past because bindings to
native GUI framworks like GTK have been a pain. It's therefore very
clever to use a web interface instead. This project is therefore a
playground to explore the possibilities of the
[threepenny-gui](https://github.com/HeinrichApfelmus/threepenny-gui)
framework for Haskell Web GUIs. So don't expect any groundbreaking
Sudoku functionality ;)

## How can I start it?
You need *cabal* in order to build the application.

Execute in */*

```
cabal sandbox init;
cabal -fbuildExecutables install
```
    
In the folder */dist/dist-sandbox-xxx/build/sudoku/* there can then be found an executable.
If you run it you can access the application on **localhost:10000**!

## What is the scraper executable for?
Apart from the sudoku application executable there is also a _scraper_
executable (in */dist/dist-sandbox-xxx/build/scraper/*). This is how I got the free sudokus of the
*/wwwroot/sudokus.db* database file . The executable fetches the sudokus from
[sudoku.org](http://www.soduko.org/sudoku-list.php) and stores them in
the current directory. You would then need to copy the *sudokus.db*
file to */wwwroot*, however, I already put the file there ;)
