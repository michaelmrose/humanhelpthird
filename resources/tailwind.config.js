module.exports = {
  corePlugins: {
    preflight: false,
    transform: false,
  },

  // The project-owned CSS task supplies the complete content list through
  // Tailwind's --content argument, including the Gesso source tree selected by
  // the active Clojure dependency aliases.
  //
  // These local paths remain useful when Tailwind is invoked manually without
  // the project task.
  content: [
    './src/**/*',
    './resources/**/*',
  ],

  theme: {},
  plugins: [],
};
