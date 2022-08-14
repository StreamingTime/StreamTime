# Abschlussprojekt

## What and why?

Twitch allows streamers to create schedules to inform their community about when and what they will stream. Users can view this schedule when visiting a streamers profile page. Our application allows Twitch users to view multiple schedules at once, without the need to manually visit multiple profile pages.

## Prerequisites

Follow [this](https://dev.twitch.tv/docs/authentication/register-app) instruction in order to register a Twitch application.
Set OAuth Redirect URL to `http://localhost:8000` and capture your Client ID.

Create a `TwitchConfig.elm` file with your Client ID in `src/`. We recommend to use `TwitchConfig.elm.example` as a template.

## Build

```
npm ci
npx elm-tailwind-modules --dir ./gen --tailwind-config tailwind.config.js
```

## Review

```
npx elm-review --ignore-dirs gen
```

## Development

Make sure [elm-live](https://github.com/wking-io/elm-live) is installed and you have followed
build instructions.

```
elm-live src/Main.elm -- --output=main.js
```

Visit `http://localhost:8000`


## Miscellaneous

### Time

Twitch uses the RFC3339 format in its API responses, which can not be handled directly by elm.
Using `elm/parser`, we implemented a parser that can convert RFC3339 strings to our own data structures. Our datatypes then can be converted to `Time.Posix` for further use. There is also a Json Decoder that directly decodes into `Time.Posix`. Our Implementation is tested against the examples contained in the RFC specification, but we deliberately ignore fractional seconds. See [RFC3339Test.elm](https://github.com/HS-Flensburg-DST/abschlussprojekt-fabian-w-und-florian/blob/master/tests/RFC3339Test.elm) for usage examples.

To display time and date values to our users, we created our `FormatTime` module that creates humand readable and flexible text representations from a `Time.Posix` value, a `Time.Zone` and a format string. Using `elm/parser`, we convert the format string (e.g. "%DD.%MM.%YYYY") to tokens, which are then used to build the resulting string. See [FormatTimeTest.elm](https://github.com/HS-Flensburg-DST/abschlussprojekt-fabian-w-und-florian/blob/master/tests/FormatTimeTest.elm) for examples.

We use the browsers timezone and `elm/time` to display time and date values with the offset our users expect.

### Styling

For the look and feel of our app, we use [TailwindCSS](https://v2.tailwindcss.com) and [daisyUI](https://v1.daisyui.com). TailwindCSS is a CSS framework that allows styling within the markup. 
daisyUI is a component library that adds classes for various UI components to TailwindCSS. This way we don't need extra CSS files and can
change the styling directly in the markup. These tools provide a consistent look across all components in our app.
In order to use Tailwind within elm we use [elm-tailwind-modules](https://github.com/matheus23/elm-tailwind-modules). 
The library generates Elm code for Tailwind utilities and components. It ensures we can use TailwindCSS with elm-css.

TailwindCSS may be configured and customized by a config file (`tailwind.config.js`). We've added additional colors and modifications to the default daisyUI theme.
In addition we've included some custom icon components (`tailwind_icons.js`) and extra grid classes.

### Ports

We make use of Ports to allow communication between Elm and JavaScript. We use them to save and load the Twitch access token from local storage. This allows us to skip the login process and directly send users to our app when their token is valid.

Therefore we created a port module (`LocalStorage.elm`). It contains the port definitions, decoding and encoding functions for our local storage data and a function to save data to
local storage. The `index.html` contains a script which provides respective JavaScript functions.

## Dependencies
- [TailwindCSS](https://v2.tailwindcss.com)
- [elm-tailwind-modules](https://github.com/matheus23/elm-tailwind-modules)
- [daisyUI](https://v1.daisyui.com)

## Assets
- login_background.jpg (https://unsplash.com/photos/2mQSmmge7t8)
- Google Material Icons ([License](https://github.com/google/material-design-icons#license))
  - [repeat](https://fonts.gstatic.com/s/i/short-term/release/materialsymbolsoutlined/repeat/default/48px.svg)
  - [warning](https://fonts.gstatic.com/s/i/short-term/release/materialsymbolsrounded/warning/default/48px.svg)
