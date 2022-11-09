module.exports = {
  content: [
    './src/**/*.elm',
  ],
  corePlugins: {
    'opacity': false,
  },
  theme: {
    colors: ({ colors }) => ({
      transparent: colors.transparent,
      current: colors.current,

      black: colors.black,
      white: colors.white,
      orange: colors.orange,
    }),
  },
  variants: []
}
