
taskkill /f /im java.exe
taskkill /f /im Rterm.exe

cd "C:\GenRegMLBench"
ren ModelTesterAllAuto.Rout rout.txt
move rout.txt "C:\GenRegMLBench\LAPTOP-1SBQTC5I"
cd "C:\GenRegMLBench"

bunchofgitstheirsauto.sh

"C:\Program Files\R\R-3.4.1\bin\x64\R.exe" CMD BATCH --max-mem-size=3000 ModelTesterAllAuto.R

