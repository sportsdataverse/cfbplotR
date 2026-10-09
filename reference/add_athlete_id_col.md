# Add Athlete ID's to data frame

This function attempts to add ESPN athlete ID's to a data frame using
the roster data in the cfbfastR-data repo. The function is experimental
and not guaranteed to be accurate.

## Usage

``` r
add_athlete_id_col(df, name_col, team_col = NULL, headshot_urls = FALSE)
```

## Arguments

- df:

  a data frame.

- name_col:

  the column in `df` with the player names to join with the roster data.

- team_col:

  Optional column with team names to join with the roster data to reduce
  the chance of matching with two players from different teams with the
  same name. If NULL and `df` has a column named "team" or "school," the
  function will use those as `team_col`. The function also checks `df`
  for a column named "season" to match names to rosters going back to
  2009.

- headshot_urls:

  logical to return headshot URLs. If TRUE, the output has an additional
  column called "headshot_url" with links for player headshots.

## Value

the original `df` with extra columns:

- `athlete_id`:

  athlete ESPN ID.

- `headshot_url`:

  url of athlete's headshot.

## Examples

``` r
# \donttest{
x <- data.frame(
  player_name = c("Britain Covey","JT Daniels")
)

add_athlete_id_col(x, player_name)
#> ℹ No season column, using "most_recent_cfb_season()" rosters
#> Error in readRDS(url(glue::glue("https://github.com/sportsdataverse/cfbfastR-data/blob/main/rosters/rds/cfb_rosters_{x}.rds?raw=true"))): cannot open the connection to 'https://github.com/sportsdataverse/cfbfastR-data/blob/main/rosters/rds/cfb_rosters_2026.rds?raw=true'


x$season <- c(2021,2021)
add_athlete_id_col(x, player_name)
#> Error in readRDS(url(glue::glue("https://github.com/sportsdataverse/cfbfastR-data/blob/main/rosters/rds/cfb_rosters_{x}.rds?raw=true"))): cannot open the connection to 'https://github.com/sportsdataverse/cfbfastR-data/blob/main/rosters/rds/cfb_rosters_2021.rds?raw=true'


x$team = c("Utah","Georgia")
add_athlete_id_col(x, player_name, team, headshot_urls = TRUE)
#> Error in readRDS(url(glue::glue("https://github.com/sportsdataverse/cfbfastR-data/blob/main/rosters/rds/cfb_rosters_{x}.rds?raw=true"))): cannot open the connection to 'https://github.com/sportsdataverse/cfbfastR-data/blob/main/rosters/rds/cfb_rosters_2021.rds?raw=true'
# }
```
