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

### Testing the Lifeboat

This command generates the parser-support files in ./generated/tinyfun

        ./scalalrlifeboat tinyfun.scalalr

This command runs the runtinyfun top-level

        scala-cli runtinyfun.scala TinyFun.scala generated

This command packages the  runtinyfun top-level as an executable

        scala-cli --power package -f -o runtinyfun runtinyfun.scala TinyFun.scala generated
        

