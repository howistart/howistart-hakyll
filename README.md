**Content and site are now at [https://github.com/howistart/howistart.org](https://github.com/howistart/howistart.org)**

# How I Start

Posts are under `posts`. Site is built with [hakyll](https://jaspervdj.be/hakyll/).

```
$ stack build
$ stack exec site rebuild
Removing _site...
Removing _cache...
Removing _cache/tmp...
Initialising...
  Creating store...
  Creating provider...
  Running rules...
Checking for out-of-date items
Compiling
  ....
Success
$ git checkout gh-pages
Switched to branch 'gh-pages'
$ setopt extended_glob
$ rm -rf -- ^_site
$ mv _site/* .
$ git commit -a -m "update site"
$ git push origin gh-pages:gh-pages
```
