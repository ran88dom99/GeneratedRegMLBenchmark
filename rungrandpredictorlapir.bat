
taskkill /f /im java.exe
taskkill /f /im Rterm.exe

cd "C:\Users\irina grishina\Desktop\generated data test"
ren ModelTesterAllAuto.Rout rout.txt
move rout.txt "C:\Users\John\Documents\GitHub\GeneratedRegMLBenchmark\ACEREBOUT"
cd "C:\Users\John\Documents\GitHub\GeneratedRegMLBenchmark"

bunchofgitstheirsauto.sh

"C:\Program Files\R\R-3.4.1\bin\x64\R.exe" CMD BATCH --max-mem-size=2000 ModelTesterAllAuto.R

