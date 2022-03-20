module.exports = {
  root: true,
  env: {
    browser: true,
    es2021: true
  },
  parserOptions: {
    ecmaVersion: 20,
    sourceType: 'module'
  },
  parser: '@typescript-eslint/parser',
  rules: {
    'node/no-path-concat': 'off',
    semi: ['error', 'never'],
    'prettier/prettier': ['error'],
  },
  extends: [
    'eslint:recommended',
    'prettier-standard/prettier-file'
  ],
  plugins: ['prettier']
}

