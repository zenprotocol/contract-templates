#!/usr/bin/env node
const start = require('./index.js');

const args = process.argv.slice(2);

if (args.includes('--version')) {
  logVersion();
  process.exit(0);
}

const contractTemplate = start(args);

contractTemplate.stdout.pipe(process.stdout);
contractTemplate.stderr.pipe(process.stderr);

contractTemplate.on('exit', function (code) {
  console.log('Closed');
  process.exit(code);
});


function logVersion() {
  const packageJson = require('./package.json')
  console.log('contract-template version: ', packageJson.version)
}
