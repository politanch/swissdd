# Package index

## Retrieve vote results

Download federal and cantonal vote results from opendata.swiss.

- [`get_nationalvotes()`](https://politanch.github.io/swissdd/reference/get_nationalvotes.md)
  : Get national results and counting status in real time or for
  selected dates or a time range in the past
- [`get_cantonalvotes()`](https://politanch.github.io/swissdd/reference/get_cantonalvotes.md)
  : Get cantonal results and counting status in real time or for
  selected dates or a time range in the past
- [`available_votedates()`](https://politanch.github.io/swissdd/reference/available_votedates.md)
  : Get a vector of available vote dates via \`get_nationalvotes\` and
  \`get_cantonalvotes“

## Parse JSON

Convert raw API JSON to tidy data frames.

- [`swiss_json_to_dfr()`](https://politanch.github.io/swissdd/reference/swiss_json_to_dfr.md)
  : Transform an opendata.swiss national results json into a tibble
- [`canton_json_to_dfr()`](https://politanch.github.io/swissdd/reference/canton_json_to_dfr.md)
  : Transform a opendata.swiss cantonal results json into a tibble

## Geodata & maps

Retrieve administrative boundaries and plot vote result maps.

- [`get_geodata()`](https://politanch.github.io/swissdd/reference/get_geodata.md)
  : Get Swiss Geodata
- [`plot_nationalvotes()`](https://politanch.github.io/swissdd/reference/plot_nationalvotes.md)
  : Plot National Votes
- [`plot_cantonalvotes()`](https://politanch.github.io/swissdd/reference/plot_cantonalvotes.md)
  : Plot Cantonal Votes

## Swissvotes database

Access the Swissvotes database and post-vote survey data.

- [`get_swissvotes()`](https://politanch.github.io/swissdd/reference/get_swissvotes.md)
  : Download additional data collected by annee politique suisse (the
  complete SwissVotes-Database)
- [`get_poll()`](https://politanch.github.io/swissdd/reference/get_poll.md)
  : Download poll data collected after a national vote by gfs.bern and
  the political science departements of the universities of Berne,
  Zurich, and Geneva.

## Analysis

- [`similar_votes()`](https://politanch.github.io/swissdd/reference/similar_votes.md)
  : Obtain similarities a vote result shares with other votes

## Data

- [`keys`](https://politanch.github.io/swissdd/reference/keys.md) :
  Merge data
