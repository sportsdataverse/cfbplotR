# Clear the cfbplotR (ggpath) image cache

Clears the image cache used by the ggpath rendering backend. ggpath (as
of its current release) does not expose a public cache-clearing
function, so this is a safe no-op that is forward-compatible: if a
future ggpath version exports `clear_cache()` this will call it
automatically.

## Usage

``` r
.cfbplotR_clear_cache()
```

## Value

Invisibly `NULL`.
