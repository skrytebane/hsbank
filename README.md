# hsbank

Just experimenting with the [Sbanken](https://sbanken.no/)-API and
Haskell.

(This is probably horribly wrong in all sorts of ways.)

See the [Sbanken Github page](https://github.com/Sbanken/) for more
information about their APIs.

## Build

``` {.example}
$ stack build
```

## Run

1. Put a JSON file with the credentials that you got from Sbanken's API
   page into a file called `~/secrets/sbanken.json`:

``` {.javascript}
{
  "customer": "12345612345",
  "apiKey": "fbabd5687897da89798798b789798a",
  "secret": "ufhiuhiuBJKNuihiu89789-789hfkje=fewewf343MZ",
  "expires": "2018-01-03T00:00:00.000Z"
}
```

2. `$ stack exec hsbank`
