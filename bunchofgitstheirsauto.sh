 #!/bin/bash

git add -A
git commit -a -m "auto"
git pull --no-edit -s recursive -X theirs

git push origin
