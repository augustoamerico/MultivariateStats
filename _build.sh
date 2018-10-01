#!/bin/sh


Rscript -e "rmarkdown::render_site(encoding = 'UTF-8')"
sed -i -e 's/https:\/\/cdn.bootcss\.com\/mathjax\/2\.7\.1\/MathJax\.js\?config=TeX-MML-AM_CHTML/https:\/\/cdnjs\.cloudflare\.com\/ajax\/libs\/mathjax\/2\.7\.5\/MathJax\.js\?config=TeX-MML-AM_CHTML/g' _book/index.html

rm ./docs/*

mv _book docs/
