printf "\n"
echo "Energy Time Series Modeling"
printf "\n"
echo "."
echo ".."
echo "..."
echo "...."
echo "....."
echo "...... Updating  remote repository!"
echo "....."
echo "...."
echo "..."
echo ".."
echo "."

currentBranch=$(git branch | sed -n -e 's/^\* \(.*\)/\1/p')



git add .
timestamp=$(date +"%D %T")

git commit -m "Energy Time Series Modeling: Branch $currentBranch ($timestamp)"

git push

if [[ "$(git push --porcelain)" == *"Done"* ]]
then
  echo "Git Push was successful!"
fi


printf "\n \n"

echo "Press any key to continue"
while [ true ] ; do
read -t 2 -n 1
if [ $? = 0 ] ; then
exit ;
else
echo -n "."
fi
done

printf "\n \n"