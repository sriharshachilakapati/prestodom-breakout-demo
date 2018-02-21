#!/bin/sh
git checkout master
npm run compile-ps
npm run compile-js
git checkout gh-pages
git checkout master -- dist/
rm index_bundle.js
rm index_bundle.js.map
rm index.html
rm -rf resources
mv dist/* .
git add .
git commit -m "Updated new binaries"
git push origin gh-pages
