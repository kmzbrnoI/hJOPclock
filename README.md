# hJOPclock

This application is a simple model railroad clock display. It connects to
[hJOPserver](https://github.com/kmzbrnoI/hJOPserver).

 * Platform: OS Windows
 * Programming language: Object Pascal
 * Author: Jan Horacek (jan.horacek@kmz-brno.cz)
 * Licence: Apache License v2.0

## Config file

Application loads config file by default located at './config.ini':

```
[server]
host=localhost
port=5896

[time]
seconds=1
```

You can customize path to config file via application`s first argument.

## Useful shortcuts

 * Ctrl+F: full screen
 * Ctrl+S: show/hide seconds