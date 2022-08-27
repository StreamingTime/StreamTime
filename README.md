# Abschlussprojekt

## Prerequisites

Follow [this](https://dev.twitch.tv/docs/authentication/register-app) instruction in order to register a Twitch application.
Set OAuth Redirect URL to `http://localhost:8000` and capture your Client ID.

Create a `TwitchConfig.elm` file with your Client ID in `src/`. We recommend to use `TwitchConfig.elm.example` as a template.

## Preparation

### Install dependencies
```
npm ci
```

### Generate Tailwind Elm code

Repeat this step every time the Tailwind configuration changed.
```
npx elm-tailwind-modules --dir ./gen --tailwind-config tailwind.config.js
```

## Review

```
npx elm-review --ignore-dirs gen
```

## Test

Make sure [elm-test](https://github.com/elm-explorations/test) is installed.

```
elm-test
```

## Build

Make sure you have [installed dependencies](https://github.com/HS-Flensburg-DST/abschlussprojekt-fabian-w-und-florian#install-dependencies) and [generated tailwind elm code](https://github.com/HS-Flensburg-DST/abschlussprojekt-fabian-w-und-florian#generate-tailwind-elm-code).

### Development

Verify [elm-live](https://github.com/wking-io/elm-live) is installed on your machine.

To build the app for development purposes, use

```
elm-live src/Main.elm -- --output=main.js
```

and visit `http://localhost:8000` in your browser.

### Production

To build the app for production, use

````
elm make src/Main.elm --optimize --output=main.js
````

Then deploy `main.js` and `index.html` using a webserver of your choice. Make sure the domain/address and port of your server is included in the  _OAuth Redirect URLs_ list for your twitch app (See [Prerequisites](https://github.com/HS-Flensburg-DST/abschlussprojekt-fabian-w-und-florian#prerequisites)).

## Dependencies
- [TailwindCSS](https://v2.tailwindcss.com)
- [elm-tailwind-modules](https://github.com/matheus23/elm-tailwind-modules)
- [daisyUI](https://v1.daisyui.com)

## Assets
- login_background.jpg (https://unsplash.com/photos/2mQSmmge7t8)
- Google Material Icons ([License](https://github.com/google/material-design-icons#license))
  - [repeat](https://fonts.gstatic.com/s/i/short-term/release/materialsymbolsoutlined/repeat/default/48px.svg)
  - [warning](https://fonts.gstatic.com/s/i/short-term/release/materialsymbolsrounded/warning/default/48px.svg)
