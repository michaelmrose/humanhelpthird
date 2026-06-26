const { execFileSync } = require('child_process');

function resolveGessoPath() {
  try {
    return execFileSync(
      'bb',
      ['-m', 'gesso.build.find-path', 'gesso'],
      { encoding: 'utf8' }
    ).trim();
  } catch (_e) {
    return null;
  }
}

const gessoPath = resolveGessoPath();

module.exports = {
  corePlugins: {
    preflight: false,
    transform: false,
  },
  content: [
    './src/**/*',
    './resources/**/*',
    ...(gessoPath ? [`${gessoPath}/**/*`] : []),
  ],
  theme: {},
  plugins: [],
};
