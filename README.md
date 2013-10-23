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
You need *cabal* in order to build the application. I actually
recommend *cabal-dev* (which downloads dependencies into a sandbox
instead of your global package archive).

Execute in */*

```
cabal-dev -fbuildExamples install
```
    
In the folder */cabal-dev/bin* there can then be found an executable.
If you run it you can access the application on **localhost:10000**!

## What is the scraper executable for?
Apart from the sudoku application executable there is also a _scraper_
executable. This is how I got the free sudokus of the
*/wwwroot/sudokus* directory. The executable fetches the sudokus from
[sudoku.org](http://www.soduko.org/sudoku-list.php), use it like this
to get the first 30 easy sudokus:

```
./scraper 1000000000 30 > easy
```
(The first parameter is the starting index of the sudoku.org sudokus. As you can see on [sudoku.org](http://www.soduko.org/sudoku-list.php) the sudokus can be accessed through a fairly simply URL pattern.)

## Next (IMPORTANT!) step

* Make the extracted Sudokus choosable and playable in the application!! ;)
