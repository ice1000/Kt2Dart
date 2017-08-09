git status
cd ./src/
ghc Main.hs
rm *~ *.hi *.hi-boot *.o-boot *.o
mv ./Main ./../Main
cd ./../
read -p "Enter commit message:"
echo
git add *
git stage *
git commit -a -m "${REPLY}"
git status
git gc
git push origin master
