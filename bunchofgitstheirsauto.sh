 #!/bin/bash

git add -A
git commit -a -m "auto"
git pull -s recursive -X theirs

git push origin
