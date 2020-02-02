taskkill /f /im java.exe
taskkill /f /im Rterm.exe

cd "C:\Users\gvg\Desktop\Gen Test\gentest\"
set isodate=%date:~11,3%-%date:~3,2%-%date:~0,2%
ren ModelTesterAllAuto.Rout rout%isodate%.txt
move rout%isodate%.txt "C:\Users\gvg\Desktop\Gen Test\gentest\HOPPER"
cd "C:\Users\gvg\Desktop\Gen Test\gentest\"

bunchofgitstheirsauto.sh

"C:\Users\gvg\Documents\R\R-3.6.2\bin\x64\R.exe" CMD BATCH --max-mem-size=3000 ModelTesterAllAuto.R