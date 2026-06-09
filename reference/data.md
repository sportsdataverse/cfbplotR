# **Data in the package for reference**

A dataset containing the school names, colors & logos for all college
teams (NCAA DI/DII/DIII and FBS/FCS).

A dataset mapping from abbreviation or alternate name forms to most
commonly used.

## Usage

``` r
logo_ref

team_name_mapping
```

## Format

A data frame with 830+ rows and 6 variables:

- school:

  School name

- type:

  Conference-tier (i.e. P5, G5, etc.)

- logo:

  primary school logo from ESPN.com

- color:

  current primary school color

- alt_color:

  current secondary school color

- wordmark:

  Wordmark for school if available

A vector with 1100+ name variations

- Short Name:

  to Full Naame
