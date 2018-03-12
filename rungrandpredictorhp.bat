taskkill /f /im java.exe
taskkill /f /im Rterm.exe

cd "C:\Users\gvg\Desktop\Gen Test\gentest\"
set isodate=%date:~10,4%-%date:~4,2%-%date:~7,2%
ren ModelTesterAllAuto.Rout rout%isodate%.txt
move rout%isodate%.txt "C:\Users\gvg\Desktop\Gen Test\gentest\HOPPER"
cd "C:\Users\gvg\Desktop\Gen Test\gentest\"

bunchofgitstheirsauto.sh

"C:\Users\gvg\Documents\R\R-3.4.3\bin\x64\R.exe" CMD BATCH ModelTesterAllAuto.R