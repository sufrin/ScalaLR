#!/bin/bash
ROOT=../..
[ ! -e ROOT ] && ln -s $ROOT ROOT
(cd ROOT; sbt package)
for STAGE in scalalr
do 
  echo Making the runtinyfun app using java -jar $ROOT/$STAGE.jar
  java -jar $ROOT/$STAGE.jar -html --output=generated-$STAGE tinyfun.scalalr
  scala-cli --power package -f -o runtinyfun runtinyfun.scala TinyFun.scala generated-$STAGE
done
exit


# pre distribution this script was
#!/bin/bash
ROOT=../..
[ ! -e ROOT ] && ln -s $ROOT ROOT
(cd ROOT; sbt package)
for STAGE in stage2
do
  echo Making the runtinyfun app using scalalr$STAGE
  $ROOT/scripts/scalalr$STAGE -html --output=generated-$STAGE tinyfun.scalalr
  scala-cli --power package -f -o runtinyfun runtinyfun.scala TinyFun.scala generated-$STAGE
done

