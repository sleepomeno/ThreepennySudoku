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

## What needs to be done?

* Write better styling in *wwwroot/css/sudoku.css*
* Download free sudokus in Haskell cocde and make them selectable in GUI
