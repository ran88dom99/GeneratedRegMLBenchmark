 #!/bin/bash

git add -A
git commit -a -m "auto"
git pull
git mergetool
git push origin
