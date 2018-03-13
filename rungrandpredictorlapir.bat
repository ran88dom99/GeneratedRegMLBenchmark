
taskkill /f /im java.exe
taskkill /f /im Rterm.exe

cd "C:\Users\irina grishina\Desktop\generated data test"
ren ModelTesterAllAuto.Rout rout.txt
move rout.txt "C:\Users\irina grishina\Desktop\generated data test\LAPTOP-1SBQTC5I"
cd "C:\Users\irina grishina\Desktop\generated data test"

bunchofgitstheirsauto.sh

"C:\Program Files\R\R-3.4.1\bin\x64\R.exe" CMD BATCH --max-mem-size=3000 ModelTesterAllAuto.R

