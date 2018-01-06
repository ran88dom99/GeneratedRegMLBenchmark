 #!/bin/bash

killall rsession
killall Rscript
killall R
 cd /home/dg/GeneratedRegMLBenchmark/
git add -A
git commit -a -m "auto l"
git pull

git push origin
Rscript 'ModelTesterAllAuto.R'
