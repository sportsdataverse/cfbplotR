---
name: Feature Request
about: Suggest a new feature or enhancement
title: "[FEATURE] "
labels: enhancement
assignees: ''
---

## Problem

<!-- What problem does this feature solve? What use case does it address?
     e.g. "I want to display team logos in facet strip labels but there is no
     element_cfb_* for strip.text" -->

## Desired Solution

<!-- How should it work? Include a proposed function signature and expected output. -->

```r
# Example usage
library(cfbplotR)
library(ggplot2)

# Proposed new function / argument
ggplot(df, aes(x = team, y = epa)) +
  geom_col() +
  new_cfbplotr_feature(...)
```

## Alternatives Considered

<!-- Other approaches you've thought about and why the proposed solution is preferred. -->

## Context

<!-- Related packages, examples from other projects (nflplotR, hoopR, etc.),
     mockups, or any other context that would help. -->
