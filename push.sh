git status
cd ./src/
ghc Main.hs -O2
rm *~ *.hi *.hi-boot *.o-boot *.o
mv ./Main ./../Main
cd ./../
rm *~ # ./Main ./transpiler
read -p "Enter commit message:"
git add *
git stage *
git commit -a -m "${REPLY}"
git status
git gc
git push origin master
