# Snowbot - the bot for the Snowdrift.coop IRC channel

[Discussion on Snowdrift.coop](https://snowdrift.coop/p/snowdrift/w/en/snowbot/d)

## Features
Snowbot has two main features:

* __Memos__: you can send memos to other users which Snowbot will relay to them
  once they are back online.
* __Logging__: Snowbot can log up to 10 lines of activity for you while you
  are gone from the channel. If you have the logging preference enabled, Snowbot
  will show you the activity you missed (with a maximum of 10 lines) once you
  reenter the channel.

## Commands
Snowbot has 5 basic commands, all of these can be invoked either by private
messaging them to Snowbot, or by prefixing them with "snowbot", "snowbot:" or
"snowbot," and posting them in the main channel:

* __log__ and __nolog__: enable and disable your logging preference.
* __forget__: removes all your preferences.
* __known\_users__: private messages you all user preferences. You shouldn't
  use this command on the main Snowbot, since the amount of users is so big
  that the bot will be kicked from the server for flooding.
* __memo__: send a memo to a user. Usage: memo [recipient] [message]

## Preferences
Currently Snowbot only stores one user preference:

* __logging__: if this is enabled Snowbot will relay missed activity to you.
  See features.
