#!/bin/bash
#
# Build asset components and copy them to ../Assets
#

./mill ScalaLR.release  && ./mill Runtime.release && ./mill ApiDocs.release &&\
cp -pv ScalaLR/bin/scalalr               ../Assets &&\
cp -pv Runtime/scalalrruntime.jar        ../Assets &&\
ln -sFv MillBuild/ApiDocs                ../Assets/ApiDocs
                        