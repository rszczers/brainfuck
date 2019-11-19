# Brainfuck interpreter

Application of Graham Hutton's [monadic parser combinators](http://www.cs.nott.ac.uk/~pszgmh/monparsing.pdf) to [Brainfuck](https://en.wikipedia.org/wiki/Brainfuck) esoteric programming language. 

## How to run
In terminal:

```
stack build
stack exec bf [file]
```

## Example

```
> stack exec bf ./examples/helloworld.bf
> Hello World!
```
