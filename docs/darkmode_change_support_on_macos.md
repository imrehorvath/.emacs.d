
Minimal application, which gets notified about interface changes and which invokes the command supplied as arguments, as a response to the change.

```swift

// Based on: https://github.com/bouk/dark-mode-notify/blob/main/dark-mode-notify.swift

import Cocoa

DistributedNotificationCenter.default.addObserver(
  forName: Notification.Name("AppleInterfaceThemeChangedNotification"),
  object: nil,
  queue: nil) { _ in
    let proc = Process()
    proc.launchPath = "/usr/bin/env"
    proc.arguments = Array(CommandLine.arguments.suffix(from: 1))
    proc.launch()
    proc.waitUntilExit()
}

NSApplication.shared.run()
```

Complile the code and place the binary in `/usr/local/bin`.

```shell
swiftc dark-mode-change-notifier.swift -o /usr/local/bin/dark-mode-change-notifier
```

Optional. Link the emacsclient command to be accessible from `/usr/local/bin` if needed.

```shell
ln -s /Applications/Emacs.app/Contents/MacOS/bin/emacsclient /usr/local/bin
```

Configure a LaunchAgent to run the `dark-mode-change-notifier` command with the arguments `emacsclient -e (match-system-dark-mode) -q`.

```xml
<?xml version="1.0" encoding="UTF-8"?>
<!DOCTYPE plist PUBLIC "-//Apple//DTD PLIST 1.0//EN"
"http://www.apple.com/DTDs/PropertyList-1.0.dtd">
<plist version="1.0">
  <dict>
    <key>Label</key>
    <string>com.imrehorvath.emacsdarkmodechange.agent</string>

    <key>EnvironmentVariables</key>
    <dict>
      <key>PATH</key>
      <string>/usr/local/sbin:/usr/local/bin:/usr/bin:/bin:/usr/sbin:/sbin:/Library/Apple/usr/bin</string>
    </dict>

    <key>ProgramArguments</key>
    <array>
      <string>/usr/local/bin/dark-mode-change-notifier</string>
      <string>emacsclient</string>
      <string>-e</string>
      <string>(match-system-dark-mode)</string>
      <string>-q</string>
    </array>

    <key>KeepAlive</key>
    <true/>
  </dict>
</plist>
```

Bootstrap the LaunchAgent for the default user 501.

```shell
launchctl bootstrap gui/501/ ~/Library/LaunchAgents/com.imrehorvath.emacsdarkmodechange.agent.plist
```

Check if it's running

```shell
launchctl list | grep emacsdarkmodechange
```

Bootout (turn off) the LaunchAgent.

```shell
launchctl bootout gui/501/ ~/Library/LaunchAgents/com.imrehorvath.emacsdarkmodechange.agent.plist
```

Print some details. 

```shell
launchctl print gui/501/com.imrehorvath.emacsdarkmodechange.agent
```

Copyright © 2021 Imre Horvath
MIT License
