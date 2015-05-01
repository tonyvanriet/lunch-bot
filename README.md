# lunch-bot

`lunch-bot` is a Slack bot that manages group lunch activity.

`lunch-bot` keeps track of who buys lunch each day along with the cost of everybody's lunch, and maintains a running balance for each team member. When somebody pays off their debt, tell `lunch-bot` and the balances will be adjusted accordingly.

`lunch-bot` doesn't bother tracking who owes what to whom. Instead, it's treated as a single pool of money amongst the team. The people that buy lunch will show a positive balance reflecting the amount of money they're owed. The people that just order lunch without buying will show a negative balance reflecting the amount that they owe. As different people buy lunch, the balance shifts between them and, in the end, this provides a very simple way to minimize the number times you actually have to exchange money.

`lunch-bot` can also gather up orders from each person and provide a summary for whoever is placing the order.

For a full list of commands, ask `lunch-bot` for [`help`](https://github.com/tonyvanriet/lunch-bot/blob/master/help.md). You can issue commands in your direct message channel with the bot. You can also issue commands in your team's `#lunch` channel by including the name of your bot.

`lunch-bot` uses [`clj-slack-client`](https://github.com/tonyvanriet/clj-slack-client) to connect to the Slack Real Time Messaging and Web APIs.

## Usage

`lunch-bot` is very much under development, though the basic functionality described above is working well.

To use it, you'll have to create a bot user in Slack and put its API token in an `api-token.txt` file at the top-level of the project.

All of the commands issued to `lunch-bot` are written out to the `events.edn` file, and `lunch-bot` will be initialized from that file during startup. I intend to make that more robust in the future, but in the meantime, you might want to backup that file somehow.

I plan to add commands for adding and modifying restaurants but currently they are hard-coded in `command/parse.clj`.

## License

Copyright © 2015 Tony van Riet

Distributed under the Eclipse Public License either version 1.0 or (at
your option) any later version.
