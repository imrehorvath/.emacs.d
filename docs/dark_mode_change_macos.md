# How to set up external support for light/dark mode changes on macOS

## Helper Application

Minimalistic application, which gets notified about interface changes and as a response to the change, invokes the command supplied as arguments.

```swift
// Based on: https://github.com/bouk/dark-mode-notify/blob/main/dark-mode-notify.swift

import Cocoa

let args = Array(CommandLine.arguments[1...])

DistributedNotificationCenter.default.addObserver(
  forName: Notification.Name("AppleInterfaceThemeChangedNotification"),
  object: nil,
  queue: nil) { _ in
    let proc = Process()
    proc.launchPath = "/usr/bin/env"
    proc.arguments = args
    proc.launch()
    proc.waitUntilExit()
}

NSApplication.shared.run()
```

Compile the code and place the binary in `/usr/local/bin`.

```shell
swiftc dark-mode-change-notifier.swift -o /usr/local/bin/dark-mode-change-notifier
```

## LaunchAgent Configuration

Link the `emacsclient` command in case it's not on the `PATH` already. Otherwise skip this step.

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
    <string>com.imrehorvath.darkmodechange.emacs.agent</string>

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
      <string>(sysdm-match-system-dark-mode)</string>
      <string>-q</string>
    </array>
    
    <key>KeepAlive</key>
    <true/>
  </dict>
</plist>
```

## LaunchAgent Operation

### Start/Stop

Bootstrap the LaunchAgent for the current user.

```shell
launchctl bootstrap gui/$UID/ ~/Library/LaunchAgents/com.imrehorvath.darkmodechange.emacs.agent.plist
```

Bootout (turn off) the LaunchAgent.

```shell
launchctl bootout gui/$UID/ ~/Library/LaunchAgents/com.imrehorvath.darkmodechange.emacs.agent.plist
```

### Status

Check if it's running

```shell
launchctl list | grep darkmodechange
```

Print some details. 

```shell
launchctl print gui/$UID/com.imrehorvath.darkmodechange.emacs.agent
```

Copyright © 2021 Imre Horvath, MIT License
