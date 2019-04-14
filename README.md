<img src="logo.png" width="120px" align="right" display="block" style="padding-top: 2em; -webkit-filter: drop-shadow(0px 0px 3px #111);">

RDota2Collect
=====================================================

Many R developers use the package RDota2 to build yours own programs to collect Dota2 data, so the
purpose of this package is to provide a easy way to get this data, with just few commands you will
being collecting the data and storing it.

### Installation

First of all you need to install MongoDB. In Ubuntu just type:

`sudo apt-get install mongodb`

Then install the package.

`devtools::install_github('andryas/RDota2Collect'")`

### Usage

To use the package you'll need [API KEY](https://steamcommunity.com/dev/apikey). It is important
to clarify some things. If you want to collect the history of each player present in each match
you need to have more than one API KEY, because each API KEY has a limited number of requisitions.
But if you just want to collect the matches, for this, you just need one API KEY.

To configure the collection details see the example below.

```
library(RDota2Plus)
config(key = c('key1', 'key2', ..., 'keyN'), ## a vector of api keys
       game_mode = c(2, 22),                 ## the game modes you want to collect see the help('config')
       lobby_type = 7,                       ## the lobby type, default only Ranked
       skill = 3,                            ## the expertise of the players 
       duration = 900,                       ## minimum match duration
       public_account_id = 5,                ## minimum public account_id in a match
       n_history_matches = 10                ## the amount of history matches that must be collected
       )                                     ## for each player in the game. if 0 will not be
                                             ## collected player history.
```

And if you use Ubuntu.

`RDota2::config_crontab()`

Done. Now your computer has been scheduled to collect Dota2 Data.


### How the collection works.
