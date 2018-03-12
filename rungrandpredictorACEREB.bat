taskkill /f /im java.exe
taskkill /f /im Rterm.exe

cd "C:\Users\John\Documents\GitHub\GeneratedRegMLBenchmark"
set isodate=%date:~10,4%-%date:~4,2%-%date:~7,2%
ren ModelTesterAllAuto.Rout rout%isodate%.txt
move rout%isodate%.txt "C:\Users\John\Documents\GitHub\GeneratedRegMLBenchmark\ACEREBOUT"
cd "C:\Users\John\Documents\GitHub\GeneratedRegMLBenchmark"



"C:\Program Files\R\R-3.4.2\bin\x64\R.exe" CMD BATCH --max-mem-size=2000M ModelTesterAllAuto.R 
