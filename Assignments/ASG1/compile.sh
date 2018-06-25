#!/bin/sh -x
# $Id: compile.sh,v 1.1 2018-04-18 05:06:13-07 - - $
sbirdir=../sbir-files
sbtran=../translator/sbtran
cid + $0
if [ ! -d $sbirdir ]
then
   mkdir $sbirdir
fi
for sbfile in *.sb
do 
   cid + $sbfile
   $sbtran $sbfile >../sbir-files/${sbfile}ir
done
mkpspdf ../sb-files.ps *.sb $0
