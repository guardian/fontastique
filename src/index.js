const fs = require('fs').promises;
const { Elm } = require('./cli');

fs.readFile('dist/fonts.json', 'utf-8').then(fonts => {

    const app = Elm.Cli.init({ flags: JSON.parse(fonts) });

    app.ports.outputFontFace.subscribe(console.log);

});
