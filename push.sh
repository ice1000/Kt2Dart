git status

gcc -Wall compile.c -o transpiler -O3

cd ./src/
ghc Main.hs -O2
rm *~
rm *.hi
rm *.hi-boot
rm *.o-boot
rm *.o
mv ./Main ./../Main
cd ./../

# rm ./Main
# rm ./transpiler
rm *~

echo ""
read -p "Enter commit message:"

git add *
git stage *
git commit -a -m "${REPLY}"

git status

echo ""
echo "commit finished..."

git gc
git push origin master

echo ""
echo "hia hia I have finished!"
