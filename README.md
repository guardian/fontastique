# Fontastique

Choose your fonts, get your CSS.

## Install

```sh
npm i
```

## Setup

```sh
npm run setup
```

**Note:** Will require Guardian GitHub credentials.

## Run

```sh
npm start
```

Will build the assets and run a local dev server; visit it at http://localhost:8000. **Note:** This will require `python3` to be installed.

Alternatively you can host the `dist` directory locally using a server of your choice. For example:

```sh
npm run build
cd dist
python3 -m http.server 8080
```

## Recommended developer setup

We recommend using VSCode.

### Extensions

VSCode should prompt you to install our recommended extensions when you open the project.

You can also find these extensions by searching for `@recommended` in the extensions pane.
