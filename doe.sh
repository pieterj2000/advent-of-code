if [ -z "$1" ]
  then
    echo "geen jaar en nummer"
    exit
fi
if [ -z "$2" ]
  then
    echo "geen nummer"
    exit
fi

cd "$(dirname "$0")"
echo $1
mkdir -p $1/$2
cd $1/$2
touch $2a.ex.input
touch $2a.input
cat > $2a.hs <<- EOM

inputFile = "7a.ex.input"
main = do
    input <- readFile inputFile
    print $ calc . parse $ input

parse :: String -> [[Int]]
parse = map (map read . words) . lines

calc :: [[Int]] -> Int
calc reports = length $ filter (isSafe . increments) reports
    where
        increments levels = zipWith (-) (tail levels) levels
        isSafe incrs = (all (\i -> i < 0 && i > -4) incrs) || (all (\i -> i > 0 && i < 4) incrs )

EOM

touch $2b.hs