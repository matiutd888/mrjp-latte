set -e
set -o xtrace
make 
./test_good.sh >/dev/null 2>/dev/null
./test_good.sh ../mrjp-tests-2/good/basic/ >/dev/null 2>/dev/null
./test_good.sh lattests/extensions/struct/ >/dev/null 2>/dev/null
./test_good.sh lattests/extensions/objects1 >/dev/null 2>/dev/null
./test_good.sh lattests/extensions/objects2 >/dev/null 2>/dev/null
./test_good.sh ../mrjp-tests/good/ >/dev/null 2>/dev/null
./test_good.sh ../gbz-test/ >dev/null 2>/dev/null
echo "ALL TESTS OK"
