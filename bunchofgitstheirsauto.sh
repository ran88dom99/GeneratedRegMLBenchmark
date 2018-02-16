 #!/bin/bash

git add -A
git commit -a -m "auto"
git pull --no-edit -s recursive -X theirs origin master

git push --set-upstream origin master
