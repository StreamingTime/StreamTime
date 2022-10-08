# Stream Time


[![Elm review](https://github.com/HS-Flensburg-DST/abschlussprojekt-fabian-w-und-florian/actions/workflows/elm-review.yml/badge.svg)](https://github.com/HS-Flensburg-DST/abschlussprojekt-fabian-w-und-florian/actions/workflows/elm-review.yml)
[![Elm test](https://github.com/HS-Flensburg-DST/abschlussprojekt-fabian-w-und-florian/actions/workflows/elm-test.yml/badge.svg)](https://github.com/HS-Flensburg-DST/abschlussprojekt-fabian-w-und-florian/actions/workflows/elm-test.yml)
[![Elm format](https://github.com/HS-Flensburg-DST/abschlussprojekt-fabian-w-und-florian/actions/workflows/elm-format.yml/badge.svg)](https://github.com/HS-Flensburg-DST/abschlussprojekt-fabian-w-und-florian/actions/workflows/elm-format.yml)

## What and why?

Twitch allows streamers to create schedules to inform their community about when and what they will stream. Users can view this schedule when visiting a streamers profile page. Our application allows Twitch users to view multiple schedules at once, without the need to manually visit multiple profile pages.

## Prerequisites

Follow [this](https://dev.twitch.tv/docs/authentication/register-app) instruction in order to register a Twitch application.
Set OAuth Redirect URL to `http://localhost:8000` and capture your Client ID.

Create a `TwitchConfig.elm` file with your Client ID in `src/`. We recommend to use `TwitchConfig.elm.example` as a template.

## Preparation

### Install dependencies
```
npm ci
```

The `package.json` contains all tools used in the commands below. We recommend `npx` to execute them, but you can execute them however you like or use globally installed versions.

### Generate Tailwind Elm code

Repeat this step every time the Tailwind configuration changed.
```
elm-tailwind-modules --dir ./gen --tailwind-config tailwind.config.js
```

## Review

```
elm-review --ignore-dirs gen
```

## Test

```
elm-test
```

## Build

Make sure you have [installed dependencies](https://github.com/HS-Flensburg-DST/abschlussprojekt-fabian-w-und-florian#install-dependencies) and [generated tailwind elm code](https://github.com/HS-Flensburg-DST/abschlussprojekt-fabian-w-und-florian#generate-tailwind-elm-code).

### Development

We use [elm-live](https://github.com/wking-io/elm-live) to test our app locally with our `index.html`.

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

Then deploy `main.js`, `assets`, `favicon.ico` and `index.html` using a webserver of your choice. Make sure the domain/address and port of your server is included in the  _OAuth Redirect URLs_ list for your twitch app (See [Prerequisites](https://github.com/HS-Flensburg-DST/abschlussprojekt-fabian-w-und-florian#prerequisites)).


## Miscellaneous

Below, we want to give insights into some aspects we found particularly interesting and/or challenging while developing the application.

### Time

Twitch uses the RFC3339 format in its API responses, which can not be handled directly by elm.
Using `elm/parser`, we implemented a parser that can convert RFC3339 strings to our own data structures. Our datatypes then can be converted to `Time.Posix` for further use. There is also a Json Decoder that directly decodes into `Time.Posix`. Our Implementation is tested against the examples contained in the RFC specification, but we deliberately ignore fractional seconds. See [RFC3339Test.elm](https://github.com/HS-Flensburg-DST/abschlussprojekt-fabian-w-und-florian/blob/master/tests/RFC3339Test.elm) for usage examples.

To display time and date values to our users, we created our `FormatTime` module that creates humand readable and flexible text representations from a `Time.Posix` value, a `Time.Zone` and a format string. Using `elm/parser`, we convert the format string (e.g. "%DD.%MM.%YYYY") to tokens, which are then used to build the resulting string. See [FormatTimeTest.elm](https://github.com/HS-Flensburg-DST/abschlussprojekt-fabian-w-und-florian/blob/master/tests/FormatTimeTest.elm) for examples.

We use the browsers timezone and `elm/time` to display time and date values with the offset our users expect.

### Twitch Login Process

The Twitch API we are using requires an OAuth access token to access resources.
In order to get this token we use Twitch [Implicit grant flow](https://dev.twitch.tv/docs/authentication/getting-tokens-oauth/#implicit-grant-flow). We navigate the user to 
`https://id.twitch.tv/oauth2/authorize` with some required query parameters such as 
the apps's registered client ID. The user needs to log in into Twitch and will be asked
to authorize our application’s access. Twitch sends the user back to our redirect URI. The server includes the access token in the fragment portion of the URI. We can now read the access token from the URI and are able to send requests to Twitch APIs.

### Pagination

Some Twitch API endpoints return ressources split into pages, each page containing a subset of the requested data and a cursor used to fetch the next page. We tried two separate ways to handle this. The first "message based" approach was to inspect the response in our `update` function and (depending on the result), start a new request to fetch the next page. Since we use the same message, all subsequent pages are handled the same way until the _message_ -> _update_ -> _request_ -> _message_ loop ends.

Our second approach (the one we still use) is based on Elm Tasks. We use direct recursion (rather than recursion through messages and update) and an accumulator to fetch all pages (`pagesWhile`) or some pages as long as certain conditions are true (`pagesWhile`). When the data is loaded (or one of the requests has failed), a single message is emitted and handled by our `update`.This implementation is much more straightforward and improved the readability and reusability of the code that uses paginated endpoints.

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

## 3rd Party code

- `Twitch.handleJsonResponse` from [Combining HTTP requests with Task in Elm](https://korban.net/posts/elm/2019-02-15-combining-http-requests-with-task-in-elm)
- `RFC3339.toPosix` ported from [this C Code](https://de.wikipedia.org/wiki/Unixzeit)

## Assets
- login_background.jpg (https://unsplash.com/photos/2mQSmmge7t8)
- Google Material Icons ([License](https://github.com/google/material-design-icons#license))
  - [repeat](https://fonts.gstatic.com/s/i/short-term/release/materialsymbolsoutlined/repeat/default/48px.svg)
  - [warning](https://fonts.gstatic.com/s/i/short-term/release/materialsymbolsrounded/warning/default/48px.svg)
  - [check circle](https://fonts.google.com/icons?selected=Material%20Symbols%20Outlined%3Acheck_circle%3AFILL%400%3Bwght%40400%3BGRAD%400%3Bopsz%4048)
  - [calendar today (modified for the favicon)](https://fonts.gstatic.com/s/i/short-term/release/materialsymbolsoutlined/calendar_today/default/48px.svg)
