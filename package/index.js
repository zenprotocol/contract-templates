const proc = require('child_process');
const path = require('path');

const programPath = path.join(__dirname, '/Release/contract-templates.exe');

function start(args, dirname = __dirname) {
    // const workingDirectory = path.join(dirname,'Release');
    if (args === undefined)
        args = [];

    let contractTemplate;

    if (process.platform !== "win32") {
        args.unshift(programPath);

        let mono = process.platform == 'darwin' ? '/Library/Frameworks/Mono.framework/Versions/Current/Commands/mono' : 'mono'

        contractTemplate = proc.spawn(mono, args,{
          //  cwd: workingDirectory
        });
    } else {
        contractTemplate = proc.spawn(programPath, args,{
           // cwd: workingDirectory
        });
    }

    return contractTemplate;
}

module.exports = start;