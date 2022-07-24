const plugin = require('tailwindcss/plugin')

module.exports = plugin(function ({ addComponents, theme }) {
    const icons = {
        '.icon-warning': {
            stroke: theme("colors.warning"),
            fill: theme("colors.warning")
        },

        '.icon-neutral': {
            stroke: theme('colors.neutral-content'),
            fill: theme('colors.neutral-content')
        },

        '.icon-m': {
            width: "1em",
            height: "1em",
        }
    }

    addComponents(icons)
})
