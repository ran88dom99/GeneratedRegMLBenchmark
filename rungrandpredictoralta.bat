taskkill /f /im java.exe
taskkill /f /im Rterm.

bunchofgits.sh

cd "C:\Users\gvg\Desktop\Gen Test\gentest\"
set isodate=%date:~11,3%-%date:~3,2%-%date:~0,2%
ren ModelTesterAllAuto.Rout rout%isodate%.txt
move rout%isodate%.txt "C:\Users\gvg\Desktop\Gen Test\gentest\ALTA"
cd "C:\Users\gvg\Desktop\Gen Test\gentest\"

"C:\Users\gvg\Documents\R\R-3.4.1\bin\x64\R.exe" CMD BATCH ModelTesterAllAuto.R