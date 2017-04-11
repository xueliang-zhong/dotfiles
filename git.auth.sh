git config credential.helper store
git push
git config --global credential.helper 'cache --timeout 7200'
