# snowbot

This is the nominal bot for the Snowdrift.coop IRC channel, `#snowdrift`
on FreeNode.

[Discussion on Snowdrift.coop](https://snowdrift.coop/p/snowdrift/w/en/snowbot/d)

## Installation

1.  [Install stack][1]
2.  Run the following commands in a terminal:

        git clone git://github.com/snowdriftcoop/snowbot.git
        cd snowbot
        stack setup && stack install

## Usage

### Features

Snowbot has two main features:

* **Memos**: you can send memos to other users which Snowbot will relay to them
  once they are back online.

* **Logging**: Snowbot can log up to 10 lines of activity for you while you
  are gone from the channel. If you have the logging preference enabled, Snowbot
  will show you the activity you missed (with a maximum of 10 lines) once you
  reenter the channel.

### Commands

Snowbot has 5 basic commands, all of which can be invoked either by
private messaging them to Snowbot, or by prefixing them with "snowbot",
"snowbot:" or "snowbot," and posting them in the main channel:

* **log** and **nolog**: enable and disable your logging preference.
* **forget**: removes all your preferences.
* **memo**: send a memo to a user. Usage: `memo [recipient] [message]`

### Preferences

Currently Snowbot only stores one user preference:

* **logging**: if this is enabled Snowbot will relay missed activity to
  you.  See features.


[1]: https://github.com/commercialhaskell/stack/wiki/Downloads
