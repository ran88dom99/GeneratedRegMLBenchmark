taskkill /f /im java.exe
taskkill /f /im Rterm.

cd "C:\Users\Dm\Desktop\generated data test"
set isodate=%date:~11,3%-%date:~3,2%-%date:~0,2%
ren ModelTesterAllAuto.Rout rout%isodate%.txt
move rout%isodate%.txt "C:\Users\Dm\Desktop\generated data test\ALTA"
cd "C:\Users\Dm\Desktop\generated data test"

bunchofgitstheirsauto.sh

"C:\Program Files\R\R-3.4.4\bin\i386\R.exe" CMD BATCH --max-mem-size=3000 ModelTesterAllAuto.R