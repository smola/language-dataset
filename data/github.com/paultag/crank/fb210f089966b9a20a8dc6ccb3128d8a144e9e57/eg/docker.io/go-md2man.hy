(require crank.language)
(import [crank.utils [gits]])


(build
  :source "go-md2man"
  :key "0x70DB41EB"
  :upstream "git://github.com/cpuguy83/go-md2man.git"
  :version (-> (gits "describe" "--tags")
               (.lstrip "v")
               (.replace "-" "+")) ; "1+3+g946ab09"
  :maintainer-email "admwiggin@gmail.com"
  :maintainer-name "Tianon Gravi"
  :upload-location "https://launchpad.net/~docker-maint/+archive/ubuntu/testing/+files/{source}_{version}.dsc"
  :suites "wily" "vivid" "trusty"
  :target "ppa:docker-maint/testing"
  :debian "git://git.debian.org/pkg-go/packages/go-md2man.git")
