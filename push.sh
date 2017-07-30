git status

ghc Main.hs -O2
gcc -Wall compile.c -o transpiler -O3

rm *~
rm *.hi
rm *.o

rm ./Main
rm ./transpiler

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
