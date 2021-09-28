# install dependincyes
sudo apt install guile-3.0-dev texinfo
# add logger
sudo ln -s /run/systemd/journal/dev-log /dev/log
# build
autoreconf -vif
./configure
make
# documentation
info doc/guile-smc.info
