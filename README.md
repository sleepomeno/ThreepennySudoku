# Haskell Web Sudoku Interface

## What is it?

This application provides an interface for solving sudokus. It is
written in Haskell with the
[threepenny-gui](https://github.com/HeinrichApfelmus/threepenny-gui)
framework.


## How can I start it?
You need *cabal* in order to build the application. I actually
recommend *cabal-dev* (which downloads dependencies into a sandbox
instead of your global package archive).

Execute in */*
    cabal-dev -fbuildExamples install
    
In the folder */cabal-dev/bin* there can then be found an executable.
If you run it you can access the application on **localhost:10000**!

## What needs to be done?

* Write better styling in *wwwroot/css/sudoku.css*
* Download free sudokus and select them in GUI
