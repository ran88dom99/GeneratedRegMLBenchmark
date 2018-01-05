 #!/bin/bash

killall rsession
killall Rscript
killall R
 cd /home/dg/GeneratedRegMLBenchmark/
git add -A
git commit -a -m "auto"
git pull
git mergetool
git push origin
Rscript 'Model Tester Quick .R'
