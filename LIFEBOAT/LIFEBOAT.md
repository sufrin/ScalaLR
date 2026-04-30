## The Lifeboat Project

The material here depends only on 

1. the stand-alone `bootstrap` module being compileable and packageable by *sbt*
2. the presence of `scala-cli`

It is designed to provide a last-resort bootstrap scalalr code
generator in case something goes horribly wrong with later stages
of the bootstrap and the later bootstrap gets bricked. See also
the `PREPAREFLAB` documentation.

### What does "bricking the bootstrap" mean?

👉 You’ve bricked the bootstrap = you can’t rebuild the toolchain

1. You’re working on a compiler that will eventually compile itself. 
   Compilation evolves through stages, the earliest of which is
   constructed using a - possibly simplified - compiler for a  - possibly
   simplified -version of the language.
  
2. You introduce a bug at intermediate stage *n* -- perhaps by trying
   to make the stage *n* compiler 

3. Now the compiler can’t compile its own source, and you have to use 
   another compiler entirely. 

### Making and Testing the Lifeboat
1. Use `makelifeboat.sh` to construct a  `scalalrlifeboat` program
in this directory using only `sbt` and `scala-cli`. If you are worried
about the resulting program, then don't install it in scripts. 

2. The directory `examples/TinyFun` has a script  `maketinyfun`
that uses the `scalalrlifeboat` program defined HERE to 
generate components of a tiny example calculator. It then
assembles a runnable `runtinyfun`.

3. When `runtinyfun` has been assemlbed, you can test it 
by typing simple expressions and assignments at it.
