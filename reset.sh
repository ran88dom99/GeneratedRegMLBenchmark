 #!/bin/bash

killall rsession
killall Rscript
killall R
 cd C:\Users\gvg\Desktop\Gen Test\gentest
git add -A
git commit -a -m "auto"
git pull
git mergetool
git push origin
Rscript 'Model Tester Quick .R'
