git status

echo ""
read -p "Enter commit message:"

rm *~

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
