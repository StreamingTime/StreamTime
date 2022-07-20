# Abschlussprojekt

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

## Dependencies
- [TailwindCSS](https://v2.tailwindcss.com)
- [elm-tailwind-modules](https://github.com/matheus23/elm-tailwind-modules)
- [daisyUI](https://v1.daisyui.com)

## Assets
- login_background.jpg (https://unsplash.com/photos/2mQSmmge7t8)