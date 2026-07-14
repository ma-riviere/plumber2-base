# Vendored front-end assets

Pinned third-party assets, committed as-is. Downloaded from the jsDelivr npm mirror on 2026-07-06 (flags on 2026-07-13). To refresh, bump the version, re-download from the same URL, update the sha256 below, and re-run `front/scripts/build-assets.R`.

| File | Package | Version | Source URL | sha256 |
| --- | --- | --- | --- | --- |
| `htmx.min.js` | htmx.org | 2.0.10 | https://cdn.jsdelivr.net/npm/htmx.org@2.0.10/dist/htmx.min.js | `71ea67185bfa8c98c39d31717c6fce5d852370fcdfd129db4543774d3145c0de` |
| `bootstrap.bundle.min.js` | bootstrap | 5.3.8 | https://cdn.jsdelivr.net/npm/bootstrap@5.3.8/dist/js/bootstrap.bundle.min.js | `e4fd49181388c48ec5040bd3fe66f57c29c8e67fcd8502b3354b96ec7ab47cc7` |
| `bootstrap.min.css` | bootswatch (flatly) | 5.3.8 | https://cdn.jsdelivr.net/npm/bootswatch@5.3.8/dist/flatly/bootstrap.min.css | `86d5cf565a9767acd314482ee14b8d91afaa7b44cd0d4c7cb72873ed4551496d` |
| `bootstrap-icons.min.css` | bootstrap-icons | 1.13.1 | https://cdn.jsdelivr.net/npm/bootstrap-icons@1.13.1/font/bootstrap-icons.min.css | `a5d6387a32ca3baec4d02336b5b3edab50c9dd518355576a011ea3dd9c1d884e` |
| `fonts/bootstrap-icons.woff` | bootstrap-icons | 1.13.1 | https://cdn.jsdelivr.net/npm/bootstrap-icons@1.13.1/font/fonts/bootstrap-icons.woff | `f55513b7b591cb84a3b87ff0e34ea24d4831d6fedc22e54b911ca64b5b544a15` |
| `fonts/bootstrap-icons.woff2` | bootstrap-icons | 1.13.1 | https://cdn.jsdelivr.net/npm/bootstrap-icons@1.13.1/font/fonts/bootstrap-icons.woff2 | `6c75710364a1ca5604267716f6d28997b26319fdb078cf11e0b42ab66ff2ea61` |
| `flags/gb.svg` | flag-icons (MIT) | 7.5.0 | https://cdn.jsdelivr.net/npm/flag-icons@7.5.0/flags/4x3/gb.svg | `c8be1e7208798a4ae692ee1e937065d498bb29e741943f6172b29118b8ed8066` |
| `flags/fr.svg` | flag-icons (MIT) | 7.5.0 | https://cdn.jsdelivr.net/npm/flag-icons@7.5.0/flags/4x3/fr.svg | `8cdacc8d79bcf210cdca2777a2c0de1f9e5862526877bd3026c9d59ecdcd4578` |

Notes:
- The Bootswatch flatly build already bundles Bootstrap's own CSS, so stock `bootstrap.min.css` is intentionally not vendored.
- `bootstrap-icons.min.css` references its fonts as `fonts/bootstrap-icons.woff2?<query>`; `build_assets()` rewrites those to the fingerprinted font names in `dist/`.
- The language-switcher flags come from the MIT-licensed `flag-icons` set (only the two 4x3 SVGs are vendored, not the package).
