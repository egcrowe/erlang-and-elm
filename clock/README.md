# Building and Installing
```sh
./bootstrap.sh
./configure
make
sudo make install
popd
```

# Running
Start the webserver daemon:
```sh
start-hello.sh
```

Check that the webserver is running:
```sh
to_erl
```

Enter `localhost:8080` into a web browser.

`<CTRL-D>` to quit the shell leaving the daemon running in the
background or enter `q().` to kill the daemon.
