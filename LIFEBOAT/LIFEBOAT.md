## The Lifeboat Project

The material here depends only on the stand-alone
`org.sufrin.scalalr.generate` program and the project
artefact `scalalr.jar` containing the scalalr runtime
and bootstrap generator.

It is designed to provide a last-resort bootstrap scalalr code
generator in case something goes horribly wrong with later stages
of the bootstrap. Ask an AI what "bricking a bootstrap" means, and
one of the answers will be something like:
````
  Compiler / language bootstrap
  
  If you’re working on a compiler that compiles itself:
  
  You introduce a bug in an early phase
  Now the compiler can’t compile its own source anymore
  
  👉 You’ve bricked the bootstrap — you can’t rebuild the toolchain
````

### Testing the Lifeboat

This command generates the three parser-support files in ./generated/tinyfun

        ./translifeboat.sh tinyfun.scalalr

This command runs the runtinyfun top-level

        scala-cli runtinyfun.scala TinyFun.scala generated

This command packages the  runtinyfun top-level as an executable

        scala-cli --power package -o runtinyfun runtinyfun.scala TinyFun.scala generated
        

