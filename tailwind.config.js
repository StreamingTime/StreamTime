module.exports = {
    content: [],
    theme: {
        extend: {
            colors: {
                dark: {
                    50: "#C1C2C5",
                    100: "#A6A7AB",
                    200: "#909296",
                    300: "#5C5F66",
                    400: "#373A40",
                    500: "#2C2E33",
                    600: "#25262B",
                    700: "#1A1B1E",
                    800: "#141517",
                    900: "#101113",
                }
            }
        },
    },
    plugins: [
        require("daisyui"),

        require("./tailwind_icons")
    ],
    variants: [],
    daisyui: {
        themes: [
            {
                mytheme: {
                    "primary": "#793ef9",
                    "primary-focus": "#570df8",
                    "primary-content": "#ffffff",
                    "secondary": "#f000b8",
                    "secondary-focus": "#bd0091",
                    "secondary-content": "#ffffff",
                    "accent": "#37cdbe",
                    "accent-focus": "#2aa79b",
                    "accent-content": "#ffffff",
                    "neutral": "#2a2e37",
                    "neutral-focus": "#16181d",
                    "neutral-content": "#ffffff",
                    "base-100": "#1A1B1E",
                    "base-200": "#141517",
                    "base-300": "#101113",
                    "base-content": "#ebecf0",
                    "info": "#66c6ff",
                    "success": "#87d039",
                    "warning": "#e2d562",
                    "error": "#ff6f6f",
                    "--border-color": "var(--b3)",
                    "--rounded-box": "1rem",
                    "--rounded-btn": "0.5rem",
                    "--rounded-badge": "1.9rem",
                    "--animation-btn": "0.25s",
                    "--animation-input": ".2s",
                    "--btn-text-case": "uppercase",
                    "--btn-focus-scale": "0.95",
                    "--navbar-padding": ".5rem",
                    "--border-btn": "1px",
                    "--tab-border": "1px",
                    "--tab-radius": "0.5rem",
                }
            }
        ]
    }
}