# Slack-telegram-bot
This is the server-side implementation of echo-bot for slack and telegram. Feel free to contribute. Check paragraphs below for instructions about app's structure and how to build it.

## How it works

### Telegram bot:

Echoes user messages (each message is echoed n times. See more in "preparing the app").

/repeats -- change n

/help -- get help message

### Slack bot

Echoes user messages (each message is echoed n times. See more in "preparing the app"). To get the answer you have to start the message with @BotName .

/repeats -- change n with emoji-reaction

/help -- get help message

## Building app

Go to app folder after downloading or cloning from git. Run command "stack build"

## Preparing the app

Before launching the app you should set the configuration of slack and telegram bots:

### Telegram bot:

Set environment variables:

TG_TOKEN="your telegram bot token"

#### Optional variables:

TG_REPEATS="default counts of repeating messages (if not specified, will be equal to 3)"

TG_HELP="bot help message (if not specified, will be "Echo bot. Repeats every message n times (default n = TG_REPEATS). To change n write /repeat")"


### Slack bot:

SL_BOT_TOKEN="Bot User OAuth Access Token"

SL_BOT_NAME="Encoded bot name(example: UBUDH33LL)

SL_APP_TOKEN="OAuth Access Token"

SL_CHANNEL="id of channel (example: CBVM94KQT)"

#### Optional variables:

SL_REPEATS="default counts of repeating messages (if not specified, will be equal to 3)"

SL_HELP="bot help message (if not specified, will be "Echo bot. Repeats every message n times (default n = SL_REPEATS). To change n write /repeat")"

Your app needs next scopes permissions:
channels:history
channels:read
channels:write
chat:write:bot

## Launching the app

After building the app and setting environment variables you can launci it with command "stack exec slack-telegram-bot-exe"

## App structure

app/Main.hs -- main function, getting bots configs and launching bots in parallel

src/

  Bot -- typeclasses for messenger bot and bot's message

  Echo -- typeclasses for echo messenger bots

  Examples - implementations of Slack and Telegram messenger bots

## Testing
"slack test" -- run unit tests

Files:

test/

  Spec.hs -- main testing file, launches all tests

  SlackTests.hs -- slack bot unit tests

  TelegramTests.hs -- telegram bot unit tests

  TestUtils.hs -- helper-functions for unit testing and checking results


## Logging
Logs from telegram bot are written to "telegram.log"

Logs from slack bot are written to "slack.log"

